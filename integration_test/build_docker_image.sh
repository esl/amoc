#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "$git_root"

otp_vsn="${TRAVIS_OTP_RELEASE:-22.3.4}-1"
echo "ERLANG/OTP '${otp_vsn}'"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg "otp_vsn=${otp_vsn}" \
	.
