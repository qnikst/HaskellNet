#!/usr/bin/env bash

set -x
set -e

HERE=$(cd `dirname $0`; pwd)

DOVECOT_VERSION="2.0.16"

DOVECOT_NAME="dovecot-${DOVECOT_VERSION}"
DOVECOT_PKG="${DOVECOT_NAME}.tar.gz"
DOVECOT_SRC_URL="http://dovecot.org/releases/2.0/$DOVECOT_PKG"
DOVECOT_INSTALL="$HERE/dovecot"

mkdir -p $DOVECOT_INSTALL

wget -c $DOVECOT_SRC_URL
tar -xvf $DOVECOT_PKG

pushd $DOVECOT_NAME
./configure --prefix=$DOVECOT_INSTALL
make install
popd

cp -R dovecot-config/* $DOVECOT_INSTALL/etc/dovecot/