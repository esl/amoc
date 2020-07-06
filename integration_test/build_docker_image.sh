#!/bin/bash

# the below settings are based on:
#    http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

git_root="$(git rev-parse --show-toplevel)"
cd "$git_root"

otp_vsn="${TRAVIS_OTP_RELEASE:-22.3.4}-1"
echo "ERLANG/OTP '${otp_vsn}'"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg "otp_vsn=${otp_vsn}" \
	.
