#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode

####################
## amoc functions ##
####################
update_settings() {
    amoc_eval "$1" "amoc_dist:update_settings(${2}, ${3})."
}

amoc_eval amoc-worker-1 "binary_to_list(amoc_config:get(global_test1))." | contains_all "test_value1"
amoc_eval amoc-worker-1 "atom_to_list(amoc_config:get(global_test2))." | contains_all "undefined"
amoc_eval amoc-worker-1 "binary_to_list(amoc_config:get(test1))." | contains_all "test_value2"
amoc_eval amoc-worker-1 "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

amoc_eval amoc-worker-2 "binary_to_list(amoc_config:get(global_test1))." | contains_all "test_value1"
amoc_eval amoc-worker-2 "atom_to_list(amoc_config:get(global_test2))." | contains_all "undefined"
amoc_eval amoc-worker-2 "binary_to_list(amoc_config:get(test1))." | contains_all "test_value2"
amoc_eval amoc-worker-2 "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

amoc_eval amoc-master "binary_to_list(amoc_config:get(global_test1))." | contains_all "test_value1"
amoc_eval amoc-master "atom_to_list(amoc_config:get(global_test2))." | contains_all "undefined"
amoc_eval amoc-master "binary_to_list(amoc_config:get(test1))." | contains_all "test_value2"
amoc_eval amoc-master "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

update_settings amoc-master "[{global_test2, <<\"test_value3\">>}, {test2, <<\"test_value4\">>}]" "[node()]"

amoc_eval amoc-master "binary_to_list(amoc_config:get(global_test2))." | contains_all "test_value3"
amoc_eval amoc-master "binary_to_list(amoc_config:get(test2))." | contains_all "test_value4"

amoc_eval amoc-worker-1 "binary_to_list(amoc_config:get(global_test2))."  | contains_all "test_value3"
amoc_eval amoc-worker-1 "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

amoc_eval amoc-worker-2 "binary_to_list(amoc_config:get(global_test2))." | contains_all "test_value3"
amoc_eval amoc-worker-2 "atom_to_list(amoc_config:get(test2))." | contains_all "undefined"

update_settings amoc-master "[{test2, <<\"test_value5\">>}]" "nodes()"

amoc_eval amoc-worker-1 "binary_to_list(amoc_config:get(test2))."  | contains_all "test_value5"
amoc_eval amoc-worker-2 "binary_to_list(amoc_config:get(test2))." | contains_all "test_value5"

update_settings amoc-master "[{global_test2, <<\"test_value6\">>}]" "nodes()" | \
    contains_all "changing_global_parameters_on_a_slave_node" "error"

amoc_eval amoc-master "binary_to_list(amoc_config:get(global_test2))."  | contains_all "test_value3"
amoc_eval amoc-worker-1 "binary_to_list(amoc_config:get(global_test2))."  | contains_all "test_value3"
amoc_eval amoc-worker-2 "binary_to_list(amoc_config:get(global_test2))."  | contains_all "test_value3"
