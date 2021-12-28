#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "$git_root"

otp_vsn="${OTP_RELEASE:-24.0}"
rebar_vsn="${REBAR_RELEASE:-3.16.1}"
echo "ERLANG/OTP '${otp_vsn}'"
echo "REBAR '${rebar_vsn}'"

docker build \
	-f Dockerfile \
	-t amoc:latest \
	--build-arg "otp_vsn=${otp_vsn}" \
	--build-arg "rebar_vsn=${rebar_vsn}" \
	.
