#!/bin/sh

find -name '*.c' -exec sed -i '/)$/N;s#)\n{#)\n{\n\tprintk(\"my_debug:
%s %d %s\\n\", __FILE__, __LINE__, __FUNCTION__);#' {} \;
