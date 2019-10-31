-module(amoc_config_validation).

-compile(export_all).

%% API

positive_integer(I) -> is_integer(I) andalso I > 0.

nonnegative_integer(I) -> is_integer(I) andalso I >= 0.

bitstring(Bitstring) -> is_bitstring(Bitstring).
