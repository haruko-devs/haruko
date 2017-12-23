#!/bin/sh

set -o errexit
set -o nounset

FIREHOL_DIR='/usr/local/share/firehol'
mkdir -p "${FIREHOL_DIR}"

NETSETS='firehol_anonymous'
for netset in ${NETSETS}; do
    curl -s -L -o "${FIREHOL_DIR}/${netset}.netset" "https://iplists.firehol.org/files/${netset}.netset"
done
