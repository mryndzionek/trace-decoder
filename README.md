# trace-decoder

A decoder for [esm](https://github.com/mryndzionek/esm) tracing subsystem.
Basically replacement for [this](https://github.com/mryndzionek/esm/blob/master/python/Trace.py)
Python script. Haskell is my language of choice for those kind of things these days.
Here are the reasons:
 - Haskell is more "dataflowy" due to laziness and that's a huge win for
	stream processing systems (deterministic concurrency, very important for embedded system testing)
 - many libraries/frameworks available providing yet more useful
	streaming semantics ([streamly](https://github.com/composewell/streamly))
 - strong static typing is very important for codebases that
	grow/change a lot (most code used for testing)
 - many modules available and easy C/C++ interoperability

There is a `default.nix` file provided for builiding with [static-haskell-nix](https://github.com/nh2/static-haskell-nix)
(Nix needs to be installed).

```sh
$(nix-build --no-link -A fullBuildScript)
```

:warning: The compilation from scratch takes hours. On a `i3-4330 CPU @ 3.50GHz` it took around 7h.

Compiled binaries can be found in `releases` section.
