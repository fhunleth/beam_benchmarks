# Changelog

## v0.2.0

* New features
  * Added benchmarks from [Benchmarks Game](https://madnight.github.io/benchmarksgame/)
  * Added common results struct for non-Estone tests. This makes is much easier
    to extract timing info programmatically especially since the return values
    for many tests aren't that important after they've been checked for correctness.
  * Moved tests around to modules since the `BeamBenchmarks` module became
    really big.

## v0.1.0

Initial release. Benchmarks should all compile and run on Elixir 1.18 and
Erlang/OTP 27.
