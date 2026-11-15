#!/usr/bin/env python3
"""Local Firefox OpenSearch suggestions backed by a periodically refreshed Nix index.

No external Python dependencies. The server never invokes Nix; only `refresh` does.
"""
import argparse
import fcntl
import json
import logging
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit

LOG = logging.getLogger("nixpkgs-suggest")


def cache_dir():
    return Path(os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache"))) / "nixpkgs-suggest"


def paths(args):
    directory = Path(args.cache_dir).expanduser() if args.cache_dir else cache_dir()
    return directory, directory / "index.json"


def read_index(path):
    with path.open("r", encoding="utf-8") as stream:
        index = json.load(stream)
    if not isinstance(index, dict) or index.get("format") != 1:
        raise ValueError("Unsupported index format")
    packages = index.get("packages")
    if not isinstance(packages, list) or not all(isinstance(x, str) for x in packages):
        raise ValueError("Invalid packages in index")
    return index


def package_names(results):
    """Strip the output and system prefixes, retaining nested attribute paths."""
    names = set()
    for key in results:
        output, system, name = key.split(".", 2)
        if output in ("legacyPackages", "packages") and system and name:
            names.add(name)
    return sorted(names, key=str.casefold)


def refresh(args):
    directory, dest = paths(args)
    directory.mkdir(parents=True, exist_ok=True)
    lock_path = directory / "refresh.lock"
    with lock_path.open("a+b") as lock_file:
        try:
            fcntl.flock(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            LOG.info("Another index refresh is already running")
            return 0

        command = [
            args.nix,
            "--extra-experimental-features", "nix-command flakes",
            "search", "--refresh", "--json", args.source, "^",
        ]
        LOG.info("Refreshing from %s", args.source)
        final_path = None
        try:
            # A potentially large nix search result stays on disk rather than in a pipe.
            with tempfile.TemporaryFile(dir=directory) as raw:
                subprocess.run(command, stdout=raw, check=True)
                raw.seek(0)
                results = json.load(raw)
            if not isinstance(results, dict):
                raise ValueError("nix search did not return a JSON object")
            names = package_names(results)
            if not names:
                raise ValueError("Refusing to publish an empty index")
            index = {"format": 1, "source": args.source, "packages": names}
            with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=directory,
                                             prefix="index-", delete=False) as out:
                final_path = Path(out.name)
                json.dump(index, out, ensure_ascii=False, separators=(",", ":"))
                out.write("\n")
            # On the same filesystem, os.replace is atomic; readers keep the old file
            # until the new one is fully written and swapped into place.
            os.replace(final_path, dest)
            final_path = None
            LOG.info("Published %d packages to %s", len(names), dest)
            return 0
        except (subprocess.CalledProcessError, OSError, ValueError):
            LOG.exception("Index refresh failed; leaving previous index untouched")
            return 1
        finally:
            if final_path is not None:
                final_path.unlink(missing_ok=True)


def matches(query, packages, limit=8):
    q = query.casefold()
    if len(q) < 2:
        return []
    prefixes = []
    components = []
    substrings = []
    for name in packages:
        lowered = name.casefold()
        if q not in lowered:
            continue
        if lowered.startswith(q):
            prefixes.append(name)
        elif any(separator + q in lowered for separator in ".-_"):
            components.append(name)
        else:
            substrings.append(name)
    sort_key = lambda n: (len(n), n.casefold())
    return (sorted(prefixes, key=sort_key) + sorted(components, key=sort_key)
            + sorted(substrings, key=sort_key))[:limit]


class IndexCache:
    def __init__(self, path):
        self.path = path
        self.signature = None
        self.packages = ()

    def get(self):
        try:
            stat = self.path.stat()
            signature = (stat.st_ino, stat.st_mtime_ns, stat.st_size)
            if signature != self.signature:
                self.packages = tuple(read_index(self.path)["packages"])
                self.signature = signature
                LOG.info("Loaded %d packages", len(self.packages))
        except FileNotFoundError:
            # Keep a previously loaded index if the file temporarily disappears.
            pass
        except (ValueError, OSError, json.JSONDecodeError):
            LOG.exception("Invalid index; retaining previously loaded data")
        return self.packages


def serve(args):
    _, index_path = paths(args)
    cache = IndexCache(index_path)
    cache.get()

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            url = urlsplit(self.path)
            if url.path not in ("/suggest", "/health"):
                self.send_error(404)
                return
            if url.path == "/health":
                payload = json.dumps({"ready": bool(cache.get())}).encode()
            else:
                query = parse_qs(url.query).get("q", [""])[0].strip()
                if len(query) > 100:
                    self.send_error(400, "Search term too long")
                    return
                payload = json.dumps([query, matches(query, cache.get())]).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/x-suggestions+json" if url.path == "/suggest" else "application/json")
            self.send_header("Cache-Control", "no-store")
            self.send_header("X-Content-Type-Options", "nosniff")
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            try:
                self.wfile.write(payload)
            except (BrokenPipeError, ConnectionResetError):
                pass

        def log_message(self, fmt, *args):
            # Do not log every keystroke / search term.
            return

    server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    server.daemon_threads = True
    LOG.info("Listening on 127.0.0.1:%d", args.port)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("serve", "refresh"))
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--nix", default="nix")
    parser.add_argument("--source", default="github:NixOS/nixpkgs/nixos-unstable")
    parser.add_argument("--cache-dir")
    args = parser.parse_args(argv)
    if not 1024 <= args.port <= 65535:
        parser.error("--port must be between 1024 and 65535")
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
    return serve(args) if args.operation == "serve" else refresh(args)


if __name__ == "__main__":
    sys.exit(main())
