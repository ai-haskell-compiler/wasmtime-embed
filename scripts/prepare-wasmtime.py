#!/usr/bin/env python3
"""Populate the ignored vendor tree from pinned Wasmtime release artifacts."""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
import shutil
import tarfile
import tempfile
import urllib.request
import zipfile
from pathlib import Path
from typing import BinaryIO


ROOT = Path(__file__).resolve().parent.parent
MANIFEST_PATH = ROOT / "wasmtime-artifacts.json"
VENDOR_ROOT = ROOT / "vendor" / "wasmtime"


def host_target() -> str:
    systems = {"Darwin": "darwin", "Linux": "linux", "Windows": "windows"}
    machines = {
        "arm64": "aarch64",
        "aarch64": "aarch64",
        "AMD64": "x86_64",
        "x86_64": "x86_64",
    }
    try:
        return f"{machines[platform.machine()]}-{systems[platform.system()]}"
    except KeyError as error:
        raise SystemExit(
            f"unsupported host platform: {platform.machine()}-{platform.system()}"
        ) from error


def copy_stream(source: BinaryIO, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.open("wb") as output:
        shutil.copyfileobj(source, output)


def copy_tar_entry(archive: tarfile.TarFile, name: str, destination: Path) -> None:
    member = archive.getmember(name)
    source = archive.extractfile(member)
    if source is None:
        raise RuntimeError(f"release entry is not a file: {name}")
    with source:
        copy_stream(source, destination)


def extract_tar(archive_path: Path, artifact: dict[str, str], output: Path) -> None:
    root = artifact["archive_root"]
    include_prefix = f"{root}/{artifact['include'].rstrip('/')}/"
    with tarfile.open(archive_path, "r:*") as archive:
        for member in archive.getmembers():
            if member.isfile() and member.name.startswith(include_prefix):
                relative = member.name.removeprefix(include_prefix)
                copy_tar_entry(archive, member.name, output / "include" / relative)
        copy_tar_entry(
            archive,
            f"{root}/{artifact['static_library']}",
            output / "lib" / artifact["static_library_name"],
        )
        copy_tar_entry(archive, f"{root}/LICENSE", output / "LICENSE")


def extract_zip(archive_path: Path, artifact: dict[str, str], output: Path) -> None:
    root = artifact["archive_root"]
    include_prefix = f"{root}/{artifact['include'].rstrip('/')}/"
    with zipfile.ZipFile(archive_path) as archive:
        for name in archive.namelist():
            if not name.endswith("/") and name.startswith(include_prefix):
                relative = name.removeprefix(include_prefix)
                with archive.open(name) as source:
                    copy_stream(source, output / "include" / relative)
        with archive.open(f"{root}/{artifact['static_library']}") as source:
            copy_stream(source, output / "lib" / artifact["static_library_name"])
        with archive.open(f"{root}/LICENSE") as source:
            copy_stream(source, output / "LICENSE")


def download(artifact: dict[str, str], destination: Path) -> None:
    digest = hashlib.sha256()
    with urllib.request.urlopen(artifact["url"]) as response:
        with destination.open("wb") as output:
            while chunk := response.read(1024 * 1024):
                digest.update(chunk)
                output.write(chunk)
    actual = digest.hexdigest()
    if actual != artifact["sha256"]:
        destination.unlink(missing_ok=True)
        raise RuntimeError(
            f"SHA-256 mismatch for {artifact['archive']}: "
            f"expected {artifact['sha256']}, got {actual}"
        )


def prepare(target: str, version: str, artifact: dict[str, str]) -> None:
    destination = VENDOR_ROOT / target
    stamp = destination / "ARTIFACT.json"
    expected_stamp = json.dumps(
        {"target": target, "version": version, "sha256": artifact["sha256"]},
        indent=2,
        sort_keys=True,
    ) + "\n"
    library = destination / "lib" / artifact["static_library_name"]
    if stamp.is_file() and library.is_file() and stamp.read_text() == expected_stamp:
        print(f"Wasmtime {version} for {target} is already prepared")
        return

    VENDOR_ROOT.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=f"wasmtime-{target}-") as temporary:
        temporary_path = Path(temporary)
        archive_path = temporary_path / artifact["archive"]
        staged = temporary_path / "output"
        print(f"Downloading Wasmtime {version} for {target}")
        download(artifact, archive_path)
        if artifact["archive"].endswith(".zip"):
            extract_zip(archive_path, artifact, staged)
        else:
            extract_tar(archive_path, artifact, staged)
        (staged / "ARTIFACT.json").write_text(expected_stamp)
        shutil.rmtree(destination, ignore_errors=True)
        staged.rename(destination)
    print(f"Prepared {destination.relative_to(ROOT)}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "target",
        nargs="?",
        default="host",
        help="target key, 'host' (default), or 'all'",
    )
    arguments = parser.parse_args()
    manifest = json.loads(MANIFEST_PATH.read_text())
    artifacts = manifest["targets"]
    requested = host_target() if arguments.target == "host" else arguments.target
    targets = sorted(artifacts) if requested == "all" else [requested]
    missing = [target for target in targets if target not in artifacts]
    if missing:
        available = ", ".join(sorted(artifacts))
        raise SystemExit(
            f"no pinned Wasmtime artifact for {', '.join(missing)}; "
            f"available targets: {available}"
        )
    for target in targets:
        prepare(target, manifest["version"], artifacts[target])


if __name__ == "__main__":
    main()
