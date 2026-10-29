"""Receipted stages: every run records inputs, code, sources, checks and outputs.

Adapted from recite/sig_fault scripts/research_pipeline.py at commit 1efbfd2172879c93b9c8a5c31f601d1e53f976a2.
"""

import csv
import datetime as dt
import hashlib
import json
import platform
import subprocess
import sys
import threading
from pathlib import Path

from scripts import openalex_client as openalex

ROOT = Path(__file__).resolve().parents[1]


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def read_csv(path):
    with Path(path).open(newline="", encoding="utf-8") as stream:
        return list(csv.DictReader(stream))


def write_csv(path, rows, fields):
    openalex.write_csv(path, rows, fields)


def write_json(path, value):
    openalex.write_json(path, value)


def unique(rows, keys):
    openalex.unique(rows, keys)


def fingerprint(path):
    path = Path(path)
    result = dict(
        path=str(path.relative_to(ROOT)), sha256=digest(path), bytes=path.stat().st_size
    )
    if path.suffix == ".csv" and "private-data" not in path.parts:
        result["rows"] = len(read_csv(path))
    return result


class Run:
    """A receipt records actual inputs, checks and outputs, including failed runs."""

    def __init__(self, study, stage, script, offline=False):
        self.study, self.stage, self.offline = study, stage, offline
        self._input_paths = set()
        self._lock = threading.Lock()
        self.data = ROOT / "data" / study
        self.cache = ROOT / "private-data" / study
        self.receipt_path = self.data / "receipts" / (stage + ".json")
        self.record = dict(
            schema_version=1,
            study=study,
            stage=stage,
            started_at=dt.datetime.now(dt.timezone.utc).isoformat(),
            command=[sys.executable, *sys.argv],
            python=platform.python_version(),
            git_commit=subprocess.check_output(
                ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
            ).strip(),
            inputs=[],
            outputs=[],
            code=[],
            parents=[],
            checks=[],
            sources=[],
            metrics={},
            unresolved=[],
            status="running",
        )
        for path in [Path(script), Path(__file__), ROOT / "scripts/openalex_client.py"]:
            self.record["code"].append(fingerprint(path.resolve()))

    def __enter__(self):
        write_json(self.receipt_path, self.record)
        return self

    def __exit__(self, kind, error, traceback):
        self.record["finished_at"] = dt.datetime.now(dt.timezone.utc).isoformat()
        self.record["status"] = "failed" if kind else "complete"
        if error:
            self.record["error"] = f"{kind.__name__}: {error}"
        write_json(self.receipt_path, self.record)
        return False

    def input(self, path):
        path = Path(path)
        relative = str(path.relative_to(ROOT))
        with self._lock:
            if relative not in self._input_paths:
                self.record["inputs"].append(fingerprint(path))
                self._input_paths.add(relative)
        return path

    def output(self, path):
        self.record["outputs"].append(fingerprint(path))
        return path

    def check(self, name, passed, detail):
        self.record["checks"].append(dict(name=name, passed=bool(passed), detail=detail))
        if not passed:
            raise ValueError(name + ": " + str(detail))

    def require(self, stage, study=None):
        path = ROOT / "data" / (study or self.study) / "receipts" / (stage + ".json")
        verify_receipt(path)
        receipt = json.loads(path.read_text())
        self.record["parents"].append(fingerprint(path))
        for item in receipt["outputs"]:
            self.input(ROOT / item["path"])

    def fetch(self, url, category, json_response=True):
        key = hashlib.sha256(url.encode()).hexdigest()
        suffix = ".json" if json_response else ".bin"
        path = self.cache / category / key[:2] / (key + suffix)
        sidecar = path.with_suffix(path.suffix + ".source.json")
        cached = path.exists()
        if not cached and self.offline:
            raise FileNotFoundError("Source is not cached: " + url)
        value = openalex.request(url, path, json_response=json_response)
        record = json.loads(sidecar.read_text())
        with self._lock:
            self.record["sources"].append(
                dict(path=str(path.relative_to(ROOT)), cache_hit=cached, **record)
            )
        return value


def verify_receipt(path, visiting=None):
    visiting = set() if visiting is None else set(visiting)
    path = Path(path)
    if path.resolve() in visiting:
        raise ValueError("Cyclic receipt dependency: " + str(path))
    visiting.add(path.resolve())
    record = json.loads(Path(path).read_text())
    if record["status"] != "complete":
        raise ValueError("Upstream stage is incomplete: " + str(path))
    for item in (
        record["inputs"] + record["outputs"] + record["code"] + record.get("parents", [])
    ):
        file = ROOT / item["path"]
        if not file.exists() or digest(file) != item["sha256"]:
            raise ValueError("Stale or changed artifact: " + item["path"])
    for parent in record.get("parents", []):
        verify_receipt(ROOT / parent["path"], visiting)
    if not record["checks"] or not all(x["passed"] for x in record["checks"]):
        raise ValueError("Receipt has no passing checks: " + str(path))
    return record
