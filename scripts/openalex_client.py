"""Cached, receipted HTTP access to OpenAlex and other public sources.

Adapted from recite/sig_fault scripts/pilot.py (request helpers) and
scripts/i4r_sources.py::fetch (rate-limit handling) at commit 1efbfd2172879c93b9c8a5c31f601d1e53f976a2.
"""

from __future__ import annotations

import csv
import datetime as dt
import email.utils
import hashlib
import json
import os
import re
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CACHE = ROOT / "private-data"
USER_AGENT = "propagation-of-error/1.0 (https://github.com/recite/propagation_of_error)"
NETWORK_LOCK = threading.Lock()
MAILTO = "gsood07@gmail.com"


def read_csv(path):
    with Path(path).open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def write_csv(path, rows, fields):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=fields, extrasaction="raise", lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(rows)


def write_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + "\n")


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def normalize_doi(value):
    value = (value or "").strip().lower()
    value = re.sub(r"^(https?://(dx\.)?doi\.org/|doi:)", "", value)
    return value if value.startswith("10.") else ""


def unique(rows, fields):
    keys = [tuple(row[field] for field in fields) for row in rows]
    if any(any(x in (None, "") for x in key) for key in keys):
        raise ValueError(f"Missing key: {fields}")
    if len(keys) != len(set(keys)):
        raise ValueError(f"Duplicate key: {fields}")


def openalex_api_key():
    """Load credentials from the environment or the user's private config."""
    key = os.environ.get("OPENALEX_API_KEY", "").strip()
    if key:
        return key
    config = Path(os.environ.get("XDG_CONFIG_HOME") or Path.home() / ".config")
    path = config / "openalex" / "api_key"
    if not path.is_file():
        return None
    return path.read_text().strip() or None


def api_url(endpoint, **params):
    params.setdefault("mailto", MAILTO)
    return "https://api.openalex.org/" + endpoint + "?" + urllib.parse.urlencode(params)


def _retry_after_seconds(header):
    try:
        return float(header)
    except (TypeError, ValueError):
        pass
    try:
        return max(1.0, email.utils.parsedate_to_datetime(header).timestamp() - time.time())
    except (TypeError, ValueError):
        return 3600.0


def request(url, path, json_response=True, timeout=40):
    """Return a cached response or fetch, cache, and record it with a sidecar.

    A 429 writes a retry-after file so a resumed run waits instead of hammering.
    Authentication tokens are never written to disk or printed.
    """
    path = Path(path)
    sidecar = path.with_suffix(path.suffix + ".source.json")
    if path.exists():
        payload = path.read_bytes()
        if sidecar.exists():
            record = json.loads(sidecar.read_text())
            if record["url"] != url or record["sha256"] != sha256(payload):
                raise ValueError("Cached response URL/hash mismatch: " + str(path))
        return json.loads(payload) if json_response else payload
    host = urllib.parse.urlparse(url).hostname
    headers = {"User-Agent": USER_AGENT}
    key = openalex_api_key() if host == "api.openalex.org" else None
    if key:
        headers["Authorization"] = "Bearer " + key
    blocked = CACHE / (host + "_retry_after.json")
    with NETWORK_LOCK:
        if blocked.exists():
            until = json.loads(blocked.read_text())["until"]
            if time.time() < until:
                raise RuntimeError(f"{host} rate limit: resume after {blocked.name}")
    for attempt in range(5):
        try:
            with urllib.request.urlopen(
                urllib.request.Request(url, headers=headers), timeout=timeout
            ) as response:
                payload = response.read()
            break
        except urllib.error.HTTPError as error:
            if error.code == 429:
                seconds = _retry_after_seconds(error.headers.get("Retry-After"))
                with NETWORK_LOCK:
                    CACHE.mkdir(parents=True, exist_ok=True)
                    blocked.write_text(
                        json.dumps({"until": time.time() + seconds, "seconds": seconds})
                    )
                raise
            if error.code not in {500, 502, 503, 504} or attempt == 4:
                raise
            time.sleep(min(2 ** (attempt + 1), 16))
        except (TimeoutError, urllib.error.URLError):
            if attempt == 4:
                raise
            time.sleep(min(2 ** (attempt + 1), 16))
    parsed = json.loads(payload) if json_response else payload
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(payload)
    sidecar.write_text(
        json.dumps(
            dict(
                url=url,
                retrieved_at=dt.datetime.now(dt.timezone.utc).isoformat(),
                sha256=sha256(payload),
                bytes=len(payload),
            ),
            indent=2,
        )
        + "\n"
    )
    return parsed


def work_key(work_id):
    return work_id.rsplit("/", 1)[-1]


def group_by_year(run, work_id, types="article|review", cutoff="2025-12-31"):
    """Annual counts of citing works of the given types; one API call per article."""
    url = api_url(
        "works",
        filter=f"cites:{work_key(work_id)},type:{types},to_publication_date:{cutoff}",
        group_by="publication_year",
    )
    payload = run.fetch(url, "grouped_citations")
    counts = {}
    for group in payload["group_by"]:
        if not group["key"].isdigit():
            raise ValueError("Non-year group key: " + str(group["key"]))
        counts[int(group["key"])] = int(group["count"])
    return counts, int(payload["meta"]["count"])


def batched_doi_lookup(run, dois, select, batch_size=50):
    """Resolve DOIs to OpenAlex works, 50 per call; unresolved DOIs are reported."""
    resolved = {}
    for start in range(0, len(dois), batch_size):
        batch = dois[start : start + batch_size]
        url = api_url("works", filter="doi:" + "|".join(batch), per_page=200, select=select)
        payload = run.fetch(url, "doi_lookups")
        if payload["meta"]["count"] > 200:
            raise ValueError("DOI batch exceeded one page")
        for work in payload["results"]:
            doi = normalize_doi(work.get("doi") or "")
            if doi in batch:
                resolved.setdefault(doi, []).append(work)
    return resolved
