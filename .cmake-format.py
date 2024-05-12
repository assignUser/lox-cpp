# SPDX-License-Identifier: Apache-2.0
#
# SPDX-FileCopyrightText: Copyright (c) assignUser
with section("format"):
    disable = False
    line_width = 100
    tab_size = 2
    max_subgroups_hwrap = 2
    max_pargs_hwrap = 4
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
    disable = True
    # Prevent formatting of license header
    first_comment_is_literal = True
