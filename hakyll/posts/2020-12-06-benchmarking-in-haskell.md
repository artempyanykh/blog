---
title: Benchmarking in Haskell (with a guest appearance from Rust)
category: haskell, rust, performance
---

Last time we implemented 2 different solutions for a coding interview problem: one that uses Data.Map and another one with MArray and ST. This time we'll benchmark those solution (and the one guest solution) and see how they compare to each other and to yet another solution written in Rust.

<!--more-->

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/LS6JWwPddro" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

--- 

- Source code: [link to Github](https://github.com/artempyanykh/blog/tree/master/lab/benchmark-haskell).
- Cabal documentation about benchmarks: [link to docs](https://cabal.readthedocs.io/en/latest/cabal-package.html#benchmarks).
- Criterion documentation: [link to Hackage](https://hackage.haskell.org/package/criterion).
- `random-shuffle` documentation: [link to Hackage](https://hackage.haskell.org/package/random-shuffle-0.0.4).
- Guest solution by Radu Grigore: [link to GitHub Gist](https://gist.github.com/rgrig/a8f18a7060171d8d41597c85bee4a3eb).