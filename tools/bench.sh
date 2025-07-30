set -e

export LANG="C.UTF-8"

breaker=1
generator=0

if [[ $1 == 'generator' ]]; then
  generator=1
  breaker=0
elif [[ $1 == 'breaker' ]]; then
  generator=0
  breaker=1
elif [[ $1 == 'all' ]]; then
  generator=1
  breaker=1
elif [[ -z $1 ]]; then
  # Default
  breaker=1
  generator=0
else
  echo "Unrecognized arg. Wanted one of [all|breaker|generator]. received: '$1'"
  exit 1
fi

# NOTE: Despite appearances, the two benchmarks suites do not clash because
#
#   - The cwd for unicode-grapheme is the project home ./, hence the directory
#     is ./benchmarks.
#
#   - The cwd for unicode-grapheme-generator is
#     ./lib/unicode-grapheme-generator, hence the directory is
#     is ./lib/unicode-grapheme-generator/benchmarks.

if [[ 1 == $generator ]]; then
  echo "*** Benching generator ***"
  cabal bench unicode-grapheme-generator:bench:benchmarks --benchmark-options \
    '+RTS -T -RTS
    -t100
    --csv benchmarks/bench.csv
    --svg benchmarks/bench.svg'
fi

if [[ 1 == $breaker ]]; then
  echo "*** Benching breaker ***"
  cabal bench unicode-grapheme:bench:benchmarks --benchmark-options \
    '+RTS -T -RTS
    -t100
    --csv benchmarks/bench.csv
    --svg benchmarks/bench.svg'
fi
