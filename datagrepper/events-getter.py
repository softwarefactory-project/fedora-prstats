#!/bin/env python3

import json
import argparse
import requests
from typing import Dict, List
from dataclasses import dataclass, asdict

BASE_URL = "https://apps.fedoraproject.org/datagrepper/raw"
TOPIC = "org.fedoraproject.prod.pagure.pull-request.new"
PER_PAGE = 100


@dataclass
class QueryParams:
    rows_per_page: int
    delta: int
    topic: str
    page: int


@dataclass
class PR:
    project: str
    branch: str
    author: str
    date_created: str
    _id: str


def decode(message: Dict) -> PR:
    pr = message["msg"]["pullrequest"]
    return PR(
        project=pr["project"]["fullname"],
        author=pr["user"]["name"],
        _id=pr["id"],
        date_created=pr["date_created"],
        branch=pr["branch"],
    )


def get(base_url: str, params: QueryParams) -> Dict:
    print("Fetching page %s ..." % params.page)
    return requests.get(base_url, params=asdict(params)).json()


def crawl_pages(base_url, acc: List[Dict], since: int):
    read_pages = 0
    total_pages = 0
    delta = 3600 * 24 * since
    while (read_pages == 0) or (read_pages <= total_pages - 1):
        try:
            params = QueryParams(
                rows_per_page=PER_PAGE, delta=delta, topic=TOPIC, page=read_pages + 1
            )
            result = get(base_url, params)
            if not total_pages:
                total_pages = result["pages"]
                print(
                    "%s total pages to requests for %s messages"
                    % (total_pages, result["total"])
                )
            read_pages += 1
            acc.extend(result["raw_messages"])
        except Exception as exc:
            print("Unable to read from %s: %s" % (BASE_URL, exc))


def main() -> None:
    parser = argparse.ArgumentParser(description="Get Fedora distgit PR events")
    parser.add_argument("--since-days", type=int, help="Since N days")
    args = parser.parse_args()
    messages: List[Dict] = []
    crawl_pages(BASE_URL, acc=messages, since=args.since_days)
    json.dump(list(map(lambda x: asdict(decode(x)), messages)), open("prs.json", "w"))


if __name__ == "__main__":
    main()
