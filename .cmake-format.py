# Copyright (c) assignUser
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
with section("format"):
    disable = False
    line_width = 100
    tab_size = 2
    max_subgroups_hwrap = 2
    max_pargs_hwrap = 2
    separate_ctrl_name_with_space = False
    separate_fn_name_with_space = False
    dangle_parens = True
    dangle_align = "prefix"
    min_prefix_chars = 4
    max_lines_hwrap = 2
    command_case = "unchanged"
    keyword_case = "unchanged"
    always_wrap = ["set_target_properties", "target_sources", "target_link_libraries"]

with section("markup"):
    # Prevent formatting of license header
    first_comment_is_literal = True
