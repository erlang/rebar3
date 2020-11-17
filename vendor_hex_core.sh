#!/bin/bash
set -e

if [[ -z "$1" ]]; then
  echo "Usage: vendor_hex_core.sh PATH_TO_HEX_CORE"
  exit 1
fi

source_dir=$1/src
target_dir=src/vendored
prefix=r3_
hex_core_version=`cat $source_dir/hex_core.hrl | grep HEX_CORE_VERSION | cut -d'"' -f2`

filenames="hex_core.hrl \
           hex_core.erl \
           hex_erl_tar.erl \
           hex_erl_tar.hrl \
           hex_filename.erl \
           hex_pb_names.erl \
           hex_pb_package.erl \
           hex_pb_signed.erl \
           hex_pb_versions.erl \
           hex_tarball.erl \
           hex_registry.erl \
           hex_http_httpc.erl \
           hex_http.erl \
           hex_repo.erl \
           hex_api.erl \
           hex_api_key.erl \
           hex_api_package.erl \
           hex_api_package_owner.erl \
           hex_api_release.erl \
           hex_api_user.erl \
           safe_erl_term.xrl"

search_to_replace="hex_core: \
                   hex_core) \
                   hex_core.hrl \
                   hex_erl_tar \
                   hex_filename \
                   hex_pb_names \
                   hex_pb_package \
                   hex_pb_signed \
                   hex_pb_versions \
                   hex_registry \
                   hex_tarball \
                   hex_http \
                   hex_repo \
                   hex_api \
                   safe_erl_term"

rm -f $target_dir/$prefix*

for filename in $filenames; do
  source_path=$source_dir/$filename
  target_path=$target_dir/$prefix$filename

  echo "%% Vendored from hex_core v$hex_core_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $source_path >> $target_path

  for word in $search_to_replace; do
    sed -i.bak s/$word/$prefix$word/g $target_path
    rm $target_path.bak
  done
done
