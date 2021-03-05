#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_certifi.sh PATH_TO_CERTIFI"
  exit 1
fi

REBAR3_TOP=$(pwd)
export REBAR3_TOP
pushd "$1"
cp src/certifi.erl $REBAR3_TOP/src/vendored/r3_certifi.erl
cp LICENSE $REBAR3_TOP/src/vendored/CERTIFI_LICENSE
cp priv/cacerts.pem $REBAR3_TOP/priv/
popd

sed -i s/module\(certifi\)./module\(r3_certifi\)./g $REBAR3_TOP/src/vendored/r3_certifi.erl
sed -i s/priv_dir\(certifi\)/priv_dir\(rebar\)/g $REBAR3_TOP/src/vendored/r3_certifi.erl
