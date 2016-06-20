make > /dev/null
echo "(6 * 6) * (6 * 6) = $(./bin/system-t sample2.t | grep -o s | wc -l)"
