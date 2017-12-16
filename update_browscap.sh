#!/bin/sh

# Updater for Browscap data files.
# https://browscap.org/

set -o errexit
set -o nounset

BROWSCAP_DIR='/usr/local/share/browscap'
mkdir -p "${BROWSCAP_DIR}"

VERSION_URL='https://browscap.org/version-number'
VERSION_FILE="${BROWSCAP_DIR}/version.txt"
if [ -f "${VERSION_FILE}" ]; then
    LOCAL_VERSION=$(cat "${VERSION_FILE}")
else
    LOCAL_VERSION='nothing'
fi
REMOTE_VERSION=$(curl -s -L "${VERSION_URL}")
if [ "${LOCAL_VERSION}" = "${REMOTE_VERSION}" ]; then
    echo "Already up to date: current version is ${REMOTE_VERSION}." >&2
    exit 0
fi
echo "Updating from ${LOCAL_VERSION} to ${REMOTE_VERSION}." >&2
printf '%s' "${REMOTE_VERSION}" > "${VERSION_FILE}"

ZIP_DATA_URL='https://browscap.org/stream?q=BrowsCapZIP'
ZIP_DATA_FILE="${BROWSCAP_DIR}/browscap.zip"
curl -s -L -o "${ZIP_DATA_FILE}" "${ZIP_DATA_URL}"
echo "Browscap data version ${REMOTE_VERSION} downloaded to ${ZIP_DATA_FILE}." >&2
