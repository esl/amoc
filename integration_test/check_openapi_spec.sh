#!/bin/bash

#the below settings are based on:
#http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

temp_dir="$(mktemp -d "/tmp/openapi.XXXXXXXXX")"
cp -v ./src/rest_api/openapi.yaml "$temp_dir"

rm_temp_dir() {
    #run as root if required, make it work for Travis CI (under linux)
    rm -rf "$temp_dir" || sudo rm -rf "$temp_dir"
}

openapi-generator() {
    docker run --rm -v "${temp_dir}:/local" \
           openapitools/openapi-generator-cli:v4.3.1 "$@" || {
        rm_temp_dir
        echo -en "\nopenapi-generator command failed:\n\t"
        echo "$@"
        exit 1
    }
}

echo -e "\nvalidate YAML OpenAPI spec:"
openapi-generator validate -i /local/openapi.yaml

echo -e "\ngenerate stub server using YAML OpenAPI spec:"
openapi-generator generate -g erlang-server \
  --additional-properties=packageName=amoc_rest \
  -i /local/openapi.yaml -o /local/tmp

echo -e "\ncheck that priv/openapi.json is the same as just generated one:"
diff -sq priv/openapi.json "${temp_dir}/tmp/priv/openapi.json" || {
    rm_temp_dir
    echo -e "\ndiff failed, priv/openapi.json is not updated"
    exit 1
}

rm_temp_dir
