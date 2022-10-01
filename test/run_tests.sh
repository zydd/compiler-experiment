set -euo pipefail

path=$(dirname $0)

cargo=cargo.exe
target="$(dirname $0)/../target/debug/lisp.exe"

$cargo build

for test in $path/test_*.ss; do
    # echo -e "* \e[;34m$test\e[0m"
    echo -n "$target "
    >/dev/null 2>&1 $target $test && echo -e "$test \e[;32mOK\e[0m" || echo -e "$test \e[;31mFAIL\e[0m"
done
