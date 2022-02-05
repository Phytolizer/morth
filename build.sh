#!/usr/bin/env bash
set -euo pipefail

arg="$1"
shift 1

case "${arg}" in
    rs)
        cargo run --manifest-path porth/Cargo.toml "$@"
        ;;
    cpp)
        cmake --build build
        build/Morth "$@"
        ;;
    rb)
        ruby morth.rb "$@"
        ;;
    rkt)
        racket morth.rkt "$@"
        ;;
    hs)
        ghc Morth
        ./Morth
        ;;
    *)
        echo "error: unknown language '${arg}'" >&2
        exit 1
        ;;
esac

