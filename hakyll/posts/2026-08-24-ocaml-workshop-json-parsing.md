---
title: "OCaml Workshop '26: JSON parsing in OxCaml: fast, but not too fast"
redirect: /talks/ocaml-workshop-2026.html
---

OxCaml provides stack allocation, unboxed types and layouts, SIMD vector types, and bit-manipulation intrinsics. These features promise more explicit control over allocation, representation, and SIMD code than OCaml programmers usually have. In our parser, that control was valuable, but the result was not C++ in OCaml syntax; it was a faster OCaml program with OCaml-shaped tradeoffs.

This talk describes lessons learned while implementing a simdjson-style JSON parser in OxCaml. The resulting parser is over 5x faster than existing OCaml parsers such as Yojson and Jsont, while still exposing a conventional `Json.t` API that users can pattern match on. In our benchmarks, the convenient tree API reaches 76% of RapidJSON's throughput, though it remains roughly 4x slower than the C++ simdjson implementation; a lower-level tape representation edges past RapidJSON and reaches 38% of simdjson throughput.

We report which features worked directly, where integration required extra care, and which language and library extensions would help downstream users build performance-critical OCaml applications.

<!--more-->

[View the slides](/talks/ocaml-workshop-2026.html)
