set -e

export LANG="C.UTF-8"

# e.g. bench_generator
name=$1

versions="
  9.2
  9.4
  9.6
  9.8
  9.10
  9.12
  9.14
  "

exts="
  csv
  svg
  "

for v in $versions; do
  for ext in $exts; do
    if [[ -f "ubuntu-latest_${v}_ci.${ext}" ]]; then
      mv "ubuntu-latest_${v}_ci.${ext}" "baseline_ubuntu-latest_${v}_ci.${ext}"
      echo "Moved ubuntu-latest_${v}_ci.${ext}"
    fi
  done
done
