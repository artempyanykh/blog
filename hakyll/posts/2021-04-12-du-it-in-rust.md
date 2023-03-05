---
title: "du it in Rust: async, tokio, streams, and surprises about perf"
category: rust, perf
---

"Rewrite it in Rust!" really picked up some steam and for a good reason: you get safety,
expressiveness... and often better performance too. One such project is [a rewrite][rust-coreutils]
of GNU coreutils in Rust. `coreutils` has been developed and improved over many years, so it'd be
interesting to see where the rewrite will land performance-wise.

In the meantime, I decided to **take the venerable `du` utility and re-implement it in
Rust**. Moreover, I wanted to take full advantage of Rust's "fearless concurrency" and use `async`
and `tokio` to extract more performance in a multi-core environment.

<!--more-->

In this video, I implement a toy clone of `du` in Rust in 3 different ways:

- using standard library and blocking APIs,
- using async and tokio, but doing naive sequential processing,
- using more advances async features (streams, `FuturesUnordered`, select) to extract more parallelism out of the async code.

Finally, all versions are benchmarked on Windows, WSL2, and Linux with surprising performance results!

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/35v9BO1g_mA" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

The full set of benchmark results is available on [the GitHub][rdu], but a very short summary is that:

1. Synchronous file IO is very fast on Linux and it's hard to beat `du`'s performance when the disk
   cache is warm. However, walking the directory tree in parallel can be beneficial for cases when
   your files are not on the disk and need to be materialized.
2. Performance depends heavily on the underlying file system. Therefore, there's no "one size fits
   all" solution. To build "the fastest cross-platform `du`", you'd need to tune and benchmark the
   implementation for each file system separately.


[rust-coreutils]: https://github.com/uutils/coreutils
[rdu]: https://github.com/artempyanykh/rdu
