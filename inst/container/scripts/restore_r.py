#!/usr/bin/env python3
"""Rebuild the commented R inventory from pinned sources, without runtime tests.

Only required dependencies determine installation order. No resolver may upgrade
recorded versions. Source hashes and installation logs are retained in the image.
"""
import hashlib
import json
import re
import shutil
import subprocess
import sys
import tarfile
from pathlib import Path


def read_dcf(path):
    result = {}
    field = None
    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        text = path.read_text(encoding="latin-1")
    for line in text.splitlines():
        if line[:1].isspace() and field:
            result[field] += " " + line.strip()
        elif ":" in line:
            field, value = line.split(":", 1)
            result[field] = value.strip()
    return result


def read_inventory(path):
    packages = {}
    current = None
    for line in path.read_text().splitlines():
        match = re.fullmatch(r"# R: ([A-Za-z][A-Za-z0-9.]*)=(\S+)", line)
        if match:
            name, version = match.groups()
            if name in packages:
                raise ValueError(f"Duplicate R package: {name}")
            current = packages[name] = {"Package": name, "Version": version}
        elif current is not None and line.startswith("#   "):
            field, value = line[4:].split(": ", 1)
            current[field] = value
    if not packages:
        raise ValueError("No R inventory found")
    # Explicit build addition: infer 1.1.0 requires this, but the snapshot lacks it.
    packages.setdefault("patchwork", {
        "Package": "patchwork", "Version": "1.3.2", "Repository": "CRAN",
        "Management": "R (install separately)",
        "Imports": "ggplot2 (>= 3.0.0), gtable (>= 0.3.6), grid, stats, grDevices, "
                   "utils, graphics, rlang (>= 1.0.0), cli, farver",
        "BuildAddition": "Required by infer; absent from the original environment",
    })
    return packages


def dependencies(record):
    names = set()
    for field in ("Depends", "Imports", "LinkingTo"):
        for entry in record.get(field, "").split(","):
            name = re.split(r"[\s(]", entry.strip())[0]
            if name and name != "R":
                names.add(name)
    return names


def fetch(urls, destination):
    for url in urls:
        print(f"Download {url}", flush=True)
        result = subprocess.run([
            "curl", "--fail", "--location", "--silent", "--show-error",
            "--retry", "3", "--connect-timeout", "30", "--max-time", "1800",
            "--output", str(destination), url,
        ])
        if result.returncode == 0:
            return url
    raise RuntimeError(f"Could not download pinned source from {urls}")


def prepare_source(record, sources):
    name, version = record["Package"], record["Version"]
    work = sources / name
    if work.exists():
        shutil.rmtree(work)
    work.mkdir()
    archive = work / "source.tar.gz"
    revision = record.get("RemoteSha", "")
    remote = record.get("RemoteUrl", "")
    if record.get("RemoteType") == "github":
        remote = "https://github.com/" + record["RemoteUsername"] + "/" + record["RemoteRepo"]
    if remote:
        if not re.fullmatch(r"[0-9a-f]{40}", revision):
            raise ValueError(f"{name}: immutable Git revision required")
        if not remote.startswith("https://github.com/"):
            raise ValueError(f"{name}: unsupported source host: {remote}")
        repository = remote.removeprefix("https://github.com/").removesuffix(".git")
        urls = [f"https://codeload.github.com/{repository}/tar.gz/{revision}"]
    elif record.get("Repository") == "CRAN":
        urls = [
            f"https://cran.r-project.org/src/contrib/{name}_{version}.tar.gz",
            f"https://cran.r-project.org/src/contrib/Archive/{name}/{name}_{version}.tar.gz",
        ]
    else:
        raise ValueError(f"{name}: no supported pinned source in inventory")
    url = fetch(urls, archive)
    digest = hashlib.sha256(archive.read_bytes()).hexdigest()
    with tarfile.open(archive) as source:
        source.extractall(work, filter="data")
    roots = [p for p in work.iterdir() if p.is_dir()]
    if len(roots) != 1:
        raise ValueError(f"Unexpected archive structure for {name}")
    package = roots[0] / record.get("RemoteSubdir", "")
    desc = read_dcf(package / "DESCRIPTION")
    if (desc.get("Package"), desc.get("Version")) != (name, version):
        raise ValueError(f"Source identity mismatch for {name}={version}")
    # Preserve upstream provenance in the newly installed DESCRIPTION.
    additions = [f"{field}: {value}" for field, value in record.items()
                 if (field.startswith("Remote") or field == "Repository") and field not in desc]
    original = (package / "DESCRIPTION").read_bytes().rstrip(b"\r\n")
    (package / "DESCRIPTION").write_bytes(original + b"\n" + "\n".join(additions).encode() + (b"\n" if additions else b""))
    return package, {"Package": name, "Version": version, "URL": url, "SHA256": digest}


def main():
    inventory, prefix = map(Path, sys.argv[1:])
    library = prefix / "lib/R/library"
    work = inventory.parent
    sources, logs = work / "sources", work / "logs"
    sources.mkdir(exist_ok=True)
    logs.mkdir(exist_ok=True)
    packages = read_inventory(inventory)
    manifest_path = work / "r-sources.jsonl"
    completed = {}
    if manifest_path.exists():
        for line in manifest_path.read_text().splitlines():
            entry = json.loads(line)
            completed[entry["Package"]] = entry
    installed = {}
    for path in library.glob("*/DESCRIPTION"):
        record = read_dcf(path)
        installed[record["Package"]] = record["Version"]
    pending = {}
    for name, record in packages.items():
        if record.get("Management", "").startswith("conda ("):
            if installed.get(name) != record["Version"]:
                raise ValueError(f"Conda's bundled R package does not match: {name}")
        elif (installed.get(name) == record["Version"]
              and completed.get(name, {}).get("Version") == record["Version"]
              and (not record.get("RemoteSha") or record["RemoteSha"] in completed[name]["URL"])):
            print(f"Retaining completed source installation: {name}={record['Version']}", flush=True)
        else:
            pending[name] = record
    missing = set().union(*(dependencies(p) for p in pending.values())) - packages.keys()
    if missing:
        raise ValueError(f"Required R packages have no pinned version: {sorted(missing)}")
    with manifest_path.open("a") as manifest:
        while pending:
            ready = sorted(n for n, p in pending.items() if dependencies(p) <= installed.keys())
            if not ready:
                raise ValueError(f"Unresolved R dependency cycle: {sorted(pending)}")
            for name in ready:
                record = pending[name]
                print(f"Installing {name}={record['Version']} ({len(pending)} remaining)", flush=True)
                package, source = prepare_source(record, sources)
                description = read_dcf(package / "DESCRIPTION")
                log_path = logs / f"{name}.log"
                try:
                    with log_path.open("w") as log:
                        # Honor the pkgbuild monorepo hook, e.g. SedonaDB's Rust workspace.
                        if (source["URL"].startswith("https://codeload.github.com/")
                                and description.get("Config/build/bootstrap", "").lower() == "true"):
                            if not (package / "bootstrap.R").is_file():
                                raise ValueError(f"Missing bootstrap.R for {name}")
                            subprocess.run([str(prefix / "bin/Rscript"), "--vanilla", "bootstrap.R"],
                                           cwd=package, stdout=log, stderr=subprocess.STDOUT, check=True)
                        subprocess.run([
                            str(prefix / "bin/R"), "CMD", "INSTALL", "--no-test-load",
                            "--no-multiarch", "--library=" + str(library), str(package),
                        ], stdout=log, stderr=subprocess.STDOUT, check=True)
                except subprocess.CalledProcessError:
                    print(log_path.read_text()[-16000:], file=sys.stderr)
                    raise
                actual = read_dcf(library / name / "DESCRIPTION")
                if actual["Version"] != record["Version"]:
                    raise ValueError(f"Installed version differs for {name}")
                installed[name] = actual["Version"]
                manifest.write(json.dumps(source, sort_keys=True) + "\n")
                manifest.flush()
                del pending[name]
                shutil.rmtree(sources / name)
    with (work / "r-installed.tsv").open("w") as handle:
        handle.write("Package\tVersion\n")
        for name, version in sorted(installed.items()):
            handle.write(f"{name}\t{version}\n")
    print("R installation complete. Runtime tests have not been run.", flush=True)


if __name__ == "__main__":
    main()
