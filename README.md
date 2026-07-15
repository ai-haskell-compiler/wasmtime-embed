# wasmtime-embed

An experimental high-level Haskell binding to the Wasmtime C API. It currently
implements the Wasmtime Book's hello-world and GCD flows: compile or load a
module, instantiate it, find function exports, and call them. The hello example
also demonstrates creating a no-argument host function.

The native dependency is Wasmtime 46.0.1's C API. Native artifacts are
kept out of Git and pinned by URL and SHA-256 in `wasmtime-artifacts.json`.
Published source distributions contain the pinned artifacts themselves, so
installing a release does not download or discover native dependencies.

Both examples load readable WebAssembly text and compile it at runtime.
`deserializeModule` remains available for trusted serialized modules produced
by the matching Wasmtime version and target.

```sh
python3 scripts/prepare-wasmtime.py
cabal build
cabal run hello
cabal run gcd
cabal test
```

The explicit preparation step is needed only when building a Git checkout. It
downloads the artifact for the host into the ignored `vendor/wasmtime` cache
and verifies its checksum. A source distribution downloaded from Hackage is
already self-contained.

The example programs and their WAT modules belong to the separate local
`wasmtime-embed-examples` package. The root `cabal.project` includes that
package for convenient development, while the publishable `wasmtime-embed`
package still contains only its library and test suite.

## Dependencies

The Haskell library depends only on `base` and `bytestring`. Its raw C API is
written by hand. `hsc2hs` is used at build time to obtain the exact sizes,
alignments, offsets, and constants from Wasmtime's pinned headers. `c2hs` is not
needed.

Using a small handwritten raw layer keeps the public API independent of C
details while avoiding a large generated binding. As coverage grows, more raw
declarations can be added from the headers one feature at a time. The ABI facts
should continue to come from `hsc2hs`; hard-coding union layouts would make the
binding unnecessarily platform-specific and brittle.

The artifact manifest currently contains Apple Silicon macOS and x86_64 Linux.
Supporting a new platform means adding one manifest entry and a Cabal/Setup
target mapping; Git history does not grow with either new targets or Wasmtime
upgrades.

## Releases

Create self-contained source distributions with:

```sh
./scripts/make-sdist.sh --output-directory=release
```

This fetches and verifies every artifact in the manifest before asking Cabal to
package the library and examples. The resulting library tarball is suitable for
Hackage and builds without network access. Tags named `v*` run the same process
and attach the tarballs to a GitHub release.

Wasmtime is licensed separately under Apache-2.0 with the LLVM exception. Each
prepared artifact and published source distribution includes Wasmtime's release
license; include the applicable notices when redistributing a statically linked
executable.
