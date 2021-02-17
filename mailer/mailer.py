#!/bin/env python3

import sys
import argparse
import requests
from ruamel.yaml import YAML
from ruamel.yaml.scalarstring import PreservedScalarString as pss
from copy import deepcopy
from typing import List, Dict, Set

from pathlib import Path

INPUT = Path("../re-prstats/src/new-distgits-added.txt")
APIURL = "https://src.fedoraproject.org/extras/pagure_owner_alias.json"

yaml = YAML()
Registry = Dict[str, Set[str]]


def get_maintainer_aliases() -> Dict[str, List[str]]:
    print("Getting alias json ...")
    ret = requests.get(APIURL)
    ret.raise_for_status()
    return ret.json()["rpms"]


def get_maintainers(name: str, aliases: Dict[str, List[str]]) -> List[str]:
    print("Getting maintainers for %s ..." % name)
    return aliases.get("/".join(name.split("/")[1:]), [])


def register(registry: Registry, maints: List[str], distgit: str) -> Registry:
    nregistry = deepcopy(registry)
    for maint in maints:
        nregistry.setdefault(maint, set([])).add(distgit)
    return nregistry


def create_email(maint: str, distgits: Set[str]) -> Dict:
    distgits_str = ", ".join(distgits)
    to = "%s@fedoraproject.org" % maint
    subject = "Some distgit(s) you maintain have been added to Fedora Zuul CI"
    body = """
Dear Maintainer,

You have received this email because you are one of the maintainers of
the following distgit(s).

{distgits}

As stated in a recent email to the Fedora Devel list [1], the distgit(s)
above have been added to the Fedora Zuul CI configuration.

tl;dr: Zuul runs generic CI jobs for any Pull-Request open or updated.
Please see [2] for more informations.

If you prefer we revert that Zuul setting for your distgit(s), please let us
know by a reply to this email or open a PR to remove the distgit(s) from the
configuration file [3]. We apologize for the inconvenience.

[1]: https://lists.fedoraproject.org/archives/list/devel@lists.fedoraproject.org/thread/WXVI7S55KBWTR6ESECM63UBNELGGVJEL/#CBJRYUTIZKA6BENJHI4S4NMAIWBYPAGA
[2]: https://fedoraproject.org/wiki/Zuul-based-ci
[3]: https://pagure.io/fedora-project-config/blob/master/f/resources/fedora-distgits.yaml
"""
    return {
        "to": to,
        "subject": subject,
        "body": pss(body.format(distgits=distgits_str)),
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Create mails pool")
    parser.add_argument("--output-path", type=str, help="Output path")
    args = parser.parse_args()
    registry: Registry = {}
    outputpath = Path(args.output_path)
    outputpath.mkdir(parents=True, exist_ok=True)
    names = INPUT.read_text().splitlines()
    print("Read %s distgit names" % len(names))
    # names = names[:3]
    aliases = get_maintainer_aliases()
    for distgit in names:
        maints = get_maintainers(distgit, aliases)
        registry = register(registry, maints, distgit)
    for maint, distgits in registry.items():
        filename = Path(outputpath) / Path("%s.yaml" % maint)
        email = create_email(maint, distgits)
        print("Writting %s" % filename)
        with open(str(filename), "w") as fd:
            yaml.dump(email, fd)


if __name__ == "__main__":
    main()
