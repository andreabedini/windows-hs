# Classify the assertions of a C file of layout checks (cLayoutChecks)
# from the errors gcc -fsyntax-only reported for it.
#
# Usage: awk -v c=FILE.c -f classify-layout-errors.awk FILE.c GCC.log
#
# Each assertion is one line, labelled "T size", "T alignment" or
# "T.field". An assertion is
#   - mismatched if it failed although its type is declared and its line
#     has no other error: the generator's layout differs from the header's;
#   - unchecked if the headers do not declare its type or field, or cannot
#     be compiled as C (mingw-w64 lacks some types, and some headers are
#     C++ only): gcc reports an undeclared name only at its first use, so
#     every assertion about that type is unchecked;
#   - passed otherwise.
# Prints "passed unchecked mismatched" on the first line, then one line per
# mismatched assertion.

# The text between the first two single quotes of s.
function quoted(s,    i) {
  i = index(s, "'")
  if (i == 0) return ""
  s = substr(s, i + 1)
  return substr(s, 1, index(s, "'") - 1)
}

FNR == NR {
  if (index($0, "_Static_assert(") == 1) {
    label = $0
    sub(/^[^"]*"/, "", label)
    sub(/".*$/, "", label)
    labels[FNR] = label
  }
  next
}

# file:line:column: error: message
index($0, c ":") == 1 && index($0, ": error: ") > 0 {
  rest = substr($0, length(c) + 2)
  line = substr(rest, 1, index(rest, ":") - 1) + 0
  msg = substr($0, index($0, ": error: ") + 9)
  if (index(msg, "static assertion failed") == 1) failed[line] = 1
  else other[line] = 1
}

/: error: .*' undeclared/ || /: error: unknown type name '/ {
  undeclared[quoted(substr($0, index($0, ": error: ") + 9))] = 1
}

END {
  for (line in labels) {
    type = labels[line]
    sub(/[ .].*$/, "", type)
    if ((type in undeclared) || (line in other)) unchecked++
    else if (line in failed) bad[++mismatched] = labels[line]
    else passed++
  }
  printf "%d %d %d\n", passed, unchecked, mismatched
  for (i = 1; i <= mismatched; i++) print bad[i]
}
