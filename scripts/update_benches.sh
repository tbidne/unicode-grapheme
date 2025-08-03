set -e

export LANG="C.UTF-8"

# e.g. bench_generator
name=$1

versions="
  9.2.8
  9.4.8
  9.6.6
  9.8.4
  9.10.2
  9.12.2
  "

for v in $versions; do
  echo "*** $v ***"
  unzip "${name}_artifacts_ubuntu-latest_$v.zip"
  mv "ubuntu-latest_${v}_ci.csv" "baseline_ubuntu-latest_${v}_ci.csv"
  mv "ubuntu-latest_${v}_ci.svg" "baseline_ubuntu-latest_${v}_ci.svg"
  charon d "${name}_artifacts_ubuntu-latest_$v.zip"
  echo ""
done
