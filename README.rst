Fedora-prstats
==============


Current purpose of this project is to extract projects that received the most PR during the last 2 months then
update the Fedora Zuul CI distgit resources file.

It is composed of first a Python tool called datagrepper that output a Json file that contains the list of
PRs open since 2 months ago.

The second tool called re-prstats, takes that json as input and update the Fedora Zuul CI SF resources file
for distgits.


datagrepper
-----------

```
cd datagrepper
python -mvenv .venv
source .venv/bin/.activate
pip install -r requirements.txt
python ./events-getter.py
deactivate
```

This output a file called prs.json in the current directory.

re-prstats
----------

```
cd ../re-prstats
yarn install && yarn build
cd src
node prstats.bs.js
```

New file is available here ./fedora-distgits.yaml