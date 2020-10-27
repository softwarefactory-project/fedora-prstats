#!bin/env python

import json
import requests
from typing import Dict, List
from dataclasses import dataclass, asdict

BASE_URL = "https://apps.fedoraproject.org/datagrepper/raw"
TOPIC = "org.fedoraproject.prod.pagure.pull-request.new"
PER_PAGE = 100
DELTA = 3600 * 24 * 31 * 2  #  2 Months


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


def crawl_pages(base_url, acc: List[Dict]):
    read_pages = 0
    total_pages = 0
    while (read_pages == 0) or (read_pages <= total_pages - 1):
        try:
            params = QueryParams(
                rows_per_page=PER_PAGE, delta=DELTA, topic=TOPIC, page=read_pages + 1
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


def main():
    messages = []
    crawl_pages(BASE_URL, acc=messages)
    json.dump(list(map(lambda x: asdict(decode(x)), messages)), open("prs.json", "w"))


if __name__ == "__main__":
    main()
