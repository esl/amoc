#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "$git_root"

otp_vsn="${OTP_RELEASE:-25.3}"
echo "ERLANG/OTP '${otp_vsn}'"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg "otp_vsn=${otp_vsn}" \
	.
