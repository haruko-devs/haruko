#!/bin/sh

# Download lists of known proxy IPs from FireHOL.
# https://iplists.firehol.org/
# TODO: replace with update-ipsets:
# https://github.com/firehol/blocklist-ipsets/wiki/Downloading-IP-Lists

set -o errexit
set -o nounset

FIREHOL_DIR='/usr/local/share/firehol'
mkdir -p "${FIREHOL_DIR}"

NETSETS='firehol_anonymous'
for netset in ${NETSETS}; do
    curl -s -L -o "${FIREHOL_DIR}/${netset}.netset" "https://iplists.firehol.org/files/${netset}.netset"
done
