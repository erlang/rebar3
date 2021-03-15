#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_hex_core.sh PATH_TO_HEX_CORE"
  exit 1
fi

REBAR3_TOP=$(pwd)
export REBAR3_TOP
pushd "$1"
touch proto/* # force re-generation of protobuf elements
TARGET_ERLANG_VERSION=20
export TARGET_ERLANG_VERSION
rebar3 as dev compile
./vendor.sh src r3_
find src -regex '.*r3_.*' -exec mv -f {} "$REBAR3_TOP/src/vendored" \;
popd
