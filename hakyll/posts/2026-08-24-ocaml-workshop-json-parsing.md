---
title: "JSON parsing in OxCaml: fast, but not too fast"
---

<div class="ocaml-report">

*OCaml Workshop 2026: [paper (PDF)](/talks/ocaml-workshop-2026.pdf) · [talk slides](/talks/ocaml-workshop-2026.html). This post is the less filtered, longer-form version.*

**TL;DR:** I built [simdjson-oxcaml][simdjson-oxcaml] using OxCaml's performance features. It is 5× faster than existing OCaml JSON parsers. I can make it 10× faster when user-defined unboxed variants land.

❤️ Unboxed types and SIMD are brilliant. More, please!

🤔 Ecosystem maturity and support for non-value layouts are rough.

💀 Templates are questionable. Layout polymorphism when?

Oh, and this thing about a GC'd language with an escape hatch when you need more performance? Yeah, I'm really not sure.

<!--more-->

<style>
/* OCaml Workshop 2026 report --------------------------------------------- */
.ocaml-report {
  --report-red: #9b2c2c;
  --report-blue: #285f74;
  --report-amber: #8a5a16;
  --report-violet: #654b78;
  --report-line: #b9b9b9;
  --report-paper: #fafafa;
}

.ocaml-report h1 { margin-top: 4.2rem; border-bottom: 0.1rem solid #999; padding-bottom: 0.35rem; }
.ocaml-report h2 { margin-top: 3.2rem; }
.ocaml-report h3 { margin-top: 2.7rem; }
.ocaml-report h4 { margin-top: 2.2rem; font-size: 1.9rem; }
.ocaml-report .report-table { width: min(94vw, 94rem); margin: 2.5rem 0 2.8rem 50%; transform: translateX(-50%); overflow-x: auto; }
.ocaml-report table { width: 100%; min-width: 64rem; margin: 0; border-collapse: collapse; font-size: 1.55rem; font-variant-numeric: tabular-nums; }
.ocaml-report th, .ocaml-report td { padding: 0.75rem 1.2rem; border: 0; border-bottom: 0.1rem solid #ccc; text-align: right; white-space: nowrap; }
.ocaml-report th:first-child, .ocaml-report td:first-child { text-align: left; }
.ocaml-report thead { border-top: 0.2rem solid #333; border-bottom: 0.12rem solid #333; background: #f4f4f4; }
.ocaml-report thead th { font-weight: 600; }
.ocaml-report tbody tr:last-child { border-bottom: 0.2rem solid #333; }
.ocaml-report tbody tr:last-child td { border-bottom: 0; }
.ocaml-report tbody tr:nth-child(even) { background: #fafafa; }
.ocaml-report .table-caption { max-width: 67rem; margin: 0.9rem auto 0; text-align: center; font: italic 1.45rem/1.35 "Droid Serif", serif; color: #444; }

.report-figure { width: min(88vw, 84rem); margin: 3rem 0 3.5rem 50%; padding: 2.2rem 2rem 1.4rem; transform: translateX(-50%); border: 0.1rem solid #bbb; background: var(--report-paper); box-sizing: border-box; text-align: left; overflow: hidden; }
.report-figure.wide { width: min(94vw, 102rem); margin-left: 50%; transform: translateX(-50%); }
.report-figure figcaption { max-width: 70rem; margin: 1.4rem auto 0; padding-top: 1rem; border-top: 0.1rem solid #ccc; text-align: center; font: italic 1.4rem/1.4 "Droid Serif", serif; color: #444; }
.report-figure code { color: #174d61; }

.pipeline, .flow, .buffer-flow { display: flex; align-items: center; justify-content: center; gap: 1rem; min-height: 6rem; font: 600 1.35rem "Source Code Pro", monospace; }
.pipeline > p, .byte-strip > p, .tape-strip > p { display: contents; }
.pipeline span, .flow span, .buffer-flow span { padding: 0.8rem 1rem; border: 0.1rem solid #888; background: white; text-align: center; }
.pipeline span.emph { border-width: 0.2rem; }
.pipeline .down { display: none; }

.byte-strip, .lanes { display: grid; grid-template-columns: repeat(15, minmax(3.2rem, 1fr)); max-width: 72rem; margin: auto; font: 1.7rem "Source Code Pro", monospace; text-align: center; }
.byte-strip span, .lanes span, .lanes b { padding: 0.4rem 0; border-right: 0.1rem solid #ddd; }
.byte-strip small { display: block; margin-bottom: 0.5rem; color: #777; font-size: 1rem; }
.byte-strip b, .lanes b { color: var(--report-red); }

.tape-strip { display: grid; grid-template-columns: repeat(9, minmax(6.5rem, 1fr)); gap: 0.4rem; min-width: 65rem; font: 1.3rem "Source Code Pro", monospace; text-align: center; overflow-x: auto; }
.tape-strip span { min-width: 5rem; padding: 0.5rem 0.25rem; border: 0.1rem solid #888; background: white; }
.tape-strip span.container { background: #e5e5e5; font-weight: bold; }
.tape-strip small, .tape-strip i { display: block; color: #666; font-size: 1rem; font-style: normal; font-weight: normal; }
.tape-strip i { margin-top: 0.5rem; color: var(--report-blue); }

.academic-chart { --label: 25rem; --number: 5.5rem; position: relative; display: grid; gap: 0.65rem; max-width: 90rem; margin: auto; padding-top: 2.5rem; font-size: 1.45rem; }
.academic-chart::before { content: ""; position: absolute; inset: 2.1rem var(--number) 0 var(--label); background: repeating-linear-gradient(to right, #ccc 0 0.1rem, transparent 0.1rem 25%); border-left: 0.1rem solid #333; }
.academic-chart .axis { position: absolute; top: 0; left: var(--label); right: var(--number); display: flex; justify-content: space-between; color: #555; }
.chart-row { position: relative; display: grid; grid-template-columns: var(--label) minmax(12rem, 1fr) var(--number); align-items: center; min-height: 2.4rem; }
.chart-row > span { padding-right: 1rem; text-align: right; line-height: 1.1; }
.chart-row small { display: block; color: #666; font-size: 1.2rem; }
.chart-row i { width: var(--value); height: 1.35rem; background: #d1d1d1; border: 0.1rem solid #777; box-sizing: border-box; }
.chart-row i.ox-light { background: #b7c6cb; }
.chart-row i.ox { background: #718f99; }
.chart-row i.ox-dark { background: #315f70; }
.chart-row > b { padding-left: 0.7rem; font-weight: normal; }
.academic-chart.compact { --label: 15rem; max-width: 60rem; }

.memory-layout { display: grid; grid-template-columns: 1.5fr 1fr; gap: 3rem; align-items: center; max-width: 66rem; margin: auto; }
.memory-layout > div { display: grid; grid-template-columns: 1fr auto auto; gap: 0.7rem; align-items: center; text-align: center; }
.memory-layout strong, .memory-layout > div > small { grid-column: 1 / -1; }
.memory-layout span { padding: 0.8rem; border: 0.1rem solid #777; background: white; }
.memory-layout .stacked hr { margin: 0.6rem -0.8rem; border: 0; border-top: 0.1rem solid #aaa; }

.simd-figure { overflow-x: auto; }
.simd-figure .lanes { grid-template-columns: repeat(16, minmax(2.4rem, 1fr)); min-width: 49rem; border: 0.1rem solid #aaa; }
.simd-figure .lane-label, .simd-figure .lane-op { margin: 0.5rem auto; text-align: center; font: 1.2rem "Source Code Pro", monospace; color: #555; }
.simd-figure .mask { background: #eee; }

.allocation-diagram { display: grid; grid-template-columns: 1.2fr 1fr; gap: 2rem; align-items: stretch; }
.allocation-diagram pre { margin: 0; padding: 1rem; overflow-x: auto; background: #f0f0f0; font-size: 1.25rem; }
.allocation-diagram > div { display: grid; gap: 1rem; }
.allocation-diagram > div span { display: flex; flex-direction: column; justify-content: center; padding: 0.8rem; border: 0.1rem solid #888; background: white; text-align: center; }
.allocation-diagram small { color: #666; }

.ratio-bars, .instruction-compare { display: grid; gap: 1.1rem; max-width: 65rem; margin: auto; }
.ratio-bars > div, .instruction-compare > div { display: grid; grid-template-columns: 14rem 1fr 15rem; align-items: center; gap: 1rem; }
.ratio-bars i, .instruction-compare i { width: var(--value); height: 2.2rem; border: 0.1rem solid #666; background: #c9c9c9; }
.ratio-bars b, .instruction-compare b { font-weight: normal; }
.instruction-compare i.ox { background: #718f99; }

.schedule { display: grid; gap: 1rem; font: 1.15rem "Source Code Pro", monospace; }
.schedule > div { display: grid; grid-template-columns: 8rem repeat(3, 1fr); align-items: center; }
.schedule span { padding: 0.8rem; border: 0.1rem solid #999; text-align: center; }
.schedule .vector { background: #e5edf0; }
.schedule .scalar { background: #eee; }
.schedule .overlap { margin-left: -30%; margin-right: 30%; border-color: var(--report-red); }

.speculative { display: flex; max-width: 52rem; margin: 1rem auto 0; height: 2.4rem; font: 1rem "Source Code Pro", monospace; text-align: center; }
.speculative b { width: 45%; padding: 0.45rem; border: 0.1rem solid #666; background: #d9e5e8; }
.speculative i { flex: 1; padding: 0.45rem; border: 0.1rem dashed #999; font-style: normal; color: #666; }

.index-compare, .mode-compare { display: grid; grid-template-columns: 1fr 1fr; gap: 1.5rem; }
.index-compare > div, .mode-compare > div { padding: 1rem; border: 0.1rem solid #999; background: white; text-align: center; }
.index-compare span { display: flex; justify-content: center; margin: 0.8rem 0; }
.index-compare code { padding: 0.45rem; border: 0.1rem solid #aaa; }
.mode-compare span { display: block; margin-top: 0.5rem; }
.mode-compare em { display: block; margin-top: 0.8rem; color: var(--report-red); }
.capacity { display: grid; gap: 0.5rem; max-width: 60rem; margin: 1.4rem auto 0; font-size: 1.2rem; }
.capacity span { display: grid; grid-template-columns: 7rem 1fr 8rem; align-items: center; gap: 0.7rem; }
.capacity i { width: var(--value); min-width: 0.4rem; height: 1rem; background: #777; }

.buffer-flow span { position: relative; }
.buffer-flow small { display: block; color: var(--report-red); }

.assembly-comparison { width: min(96vw, 116rem); overflow-x: auto; }
.assembly-grid { display: grid; grid-template-columns: 43rem 58rem; justify-content: center; gap: 1.6rem; min-width: 102.6rem; }
.assembly-grid section { min-width: 0; border: 0.1rem solid #aaa; background: white; }
.assembly-grid header { display: flex; justify-content: space-between; padding: 0.7rem 0.9rem; border-bottom: 0.1rem solid #aaa; background: #eee; font-size: 1.25rem; }
.assembly-grid header b { font-weight: normal; color: #555; }
.assembly-grid pre { margin: 0; padding: 0.8rem 0; overflow: hidden; background: white; font-size: 1.15rem; line-height: 1.38; }
.assembly-grid code > span { display: block; padding: 0 0.7rem; color: #222; }
.assembly-grid .label { font-weight: bold; }
.assembly-grid .useful { border-left: 0.35rem solid var(--report-blue); background: #edf3f5; }
.assembly-grid .runtime { border-left: 0.35rem solid var(--report-amber); background: #f6f0e7; }
.assembly-grid .poll { border-left: 0.35rem solid var(--report-violet); background: #f1edf4; }
.assembly-legend { display: flex; justify-content: center; gap: 1.5rem; margin-top: 1rem; font-size: 1.15rem; }
.assembly-legend span::before { content: ""; display: inline-block; width: 0.8rem; height: 0.8rem; margin-right: 0.4rem; }
.assembly-legend .useful::before { background: var(--report-blue); }
.assembly-legend .runtime::before { background: var(--report-amber); }
.assembly-legend .poll::before { background: var(--report-violet); }

@media (max-width: 799px) {
  .report-figure { width: calc(100vw - 1.6rem); margin-left: 50%; padding: 1.2rem 0.8rem 1rem; }
  .report-figure.wide { width: calc(100vw - 1.6rem); margin-left: 50%; transform: translateX(-50%); }
  .ocaml-report .report-table { width: calc(100vw - 1.6rem); }
  .pipeline, .flow, .buffer-flow { flex-direction: column; }
  .pipeline b, .flow b, .buffer-flow b { transform: rotate(90deg); }
  .byte-strip { grid-template-columns: repeat(15, 2.8rem); overflow-x: auto; justify-content: start; }
  .academic-chart { --label: 11rem; --number: 4.4rem; font-size: 1rem; }
  .academic-chart.compact { --label: 10rem; }
  .chart-row small { display: none; }
  .memory-layout, .allocation-diagram, .index-compare, .mode-compare, .assembly-grid, .packing-compare { grid-template-columns: 1fr; }
  .assembly-grid { min-width: 0; }
  .schedule > div { grid-template-columns: 6.5rem 1fr; }
  .schedule span { grid-column: 2; }
  .schedule .overlap { margin: 0; }
  .ratio-bars > div, .instruction-compare > div { grid-template-columns: 9rem 1fr; }
  .ratio-bars b, .instruction-compare b { grid-column: 2; }
  .assembly-legend { flex-direction: column; gap: 0.4rem; }
}

/* Refined report figures: source-faithful web adaptations. */
.indexed-json { max-width: 72rem; margin: 0.5rem auto; overflow-x: auto; font-family: "Source Code Pro", monospace; }
.indexed-row { display: grid; grid-template-columns: repeat(15, minmax(3.5rem, 1fr)); min-width: 60rem; text-align: center; }
.indexed-row.indices { color: #777; font-size: 1.05rem; }
.indexed-row.indices span { padding-bottom: 0.5rem; }
.indexed-row.chars { border: 0.1rem solid #aaa; background: white; font-size: 1.75rem; }
.indexed-row.chars span, .indexed-row.chars b { padding: 0.65rem 0; border-right: 0.1rem solid #ddd; }
.indexed-row.chars > :last-child { border-right: 0; }
.indexed-row.chars b { color: var(--report-red); background: #f8eeee; }

.tape-svg { display: block; width: 100%; height: auto; overflow: visible; font-family: "Source Code Pro", monospace; }
.tape-svg .tape-cells rect { width: 80px; height: 60px; fill: #fff; stroke: #777; stroke-width: 1.5; }
.tape-svg .tape-cells .container rect { fill: #e6e6e6; }
.tape-svg .tape-cells text { text-anchor: middle; font-size: 13px; }
.tape-svg .tape-cells .idx { fill: #777; font-size: 10px; }
.tape-svg .tape-links path { fill: none; stroke: var(--report-blue); stroke-width: 2; marker-end: url(#tape-arrow); }
.tape-svg marker path { fill: var(--report-blue); }
.tape-svg .tape-labels text { fill: #555; text-anchor: middle; font-size: 10px; stroke: white; stroke-width: 5px; stroke-linejoin: round; paint-order: stroke fill; }

.packing-compare { display: grid; grid-template-columns: 1fr 1fr; gap: 3rem; max-width: 72rem; margin: auto; align-items: start; }
.packing-compare section { display: grid; justify-items: center; gap: 0.5rem; }
.pointer-slots, .elem-slots, .mask-slots { display: flex; justify-content: center; gap: 0.5rem; }
.pointer-slots span, .elem-slots span, .mask-slots span { min-width: 6rem; padding: 0.6rem 0.8rem; border: 0.1rem solid #777; background: #fafafa; text-align: center; }
.mask-slots span { min-width: 10rem; background: #e5e5e5; }
.pointer-arrows { display: flex; width: 13rem; justify-content: space-around; height: 1.5rem; color: var(--report-blue); font-size: 1.8rem; line-height: 1; }
.packing-compare small { max-width: 25rem; text-align: center; }

@media (max-width: 799px) {
  .packing-compare { grid-template-columns: 1fr; }
}

.stack-demo { display: grid; grid-template-columns: 1.05fr 1fr; gap: 2rem; align-items: stretch; }
.stack-demo > pre { margin: 0; padding: 1.2rem; overflow-x: auto; background: #f0f0f0; font-size: 1.2rem; }
.stack-memory { display: grid; grid-template-rows: 1fr 1.25fr; gap: 1rem; }
.stack-memory section { display: grid; grid-template-rows: auto 1fr auto; gap: 0.5rem; text-align: center; }
.stack-memory section > strong { font: 600 1.2rem "Source Code Pro", monospace; }
.stack-memory section > small { color: #666; }
.heap-region, .stack-region { display: grid; place-items: center; padding: 0.8rem; border: 0.12rem solid #777; background: white; font-style: normal; }
.stack-region > em { margin-bottom: 0.5rem; color: #555; font-style: normal; }
.stack-region > span { display: grid; grid-template-columns: 1fr 1fr; border: 0.15rem solid var(--report-blue); background: #edf3f5; }
.stack-region > span > code { grid-column: 1 / -1; padding: 0.35rem; border-bottom: 0.1rem solid #999; }
.stack-region > span > b { padding: 0.35rem 0.7rem; font-weight: normal; }
.stack-region > span > b + b { border-left: 0.1rem solid #999; }

.schedule .overlap { margin: 0; border-color: var(--report-red); }

.vertical-chart { display: grid; grid-template-columns: 2.5rem 4.5rem 1fr; grid-template-rows: 28rem auto; max-width: 68rem; margin: 0 auto; }
.vertical-chart .y-label { align-self: center; transform: rotate(-90deg); white-space: nowrap; font-size: 1.15rem; }
.vertical-chart .y-ticks { display: flex; flex-direction: column; justify-content: space-between; padding: 0 0.7rem 2.8rem 0; text-align: right; color: #555; font-size: 1rem; }
.vertical-chart .plot { position: relative; display: flex; align-items: stretch; justify-content: space-around; gap: 2rem; padding: 0 1.5rem 2.8rem; border-left: 0.1rem solid #555; border-bottom: 0.1rem solid #555; }
.vertical-chart .v-grid { position: absolute; inset: 0 0 2.8rem; background: repeating-linear-gradient(to bottom, #ccc 0 0.1rem, transparent 0.1rem 25%); }
.vertical-chart .bar-group { position: relative; display: flex; align-items: end; justify-content: center; gap: 0.6rem; width: 25%; }
.vertical-chart .bar-group i { position: relative; width: 2.8rem; height: var(--value); border: 0.1rem solid #666; background: #d3d3d3; box-sizing: border-box; }
.vertical-chart .bar-group i.after { background: #526d78; }
.vertical-chart .bar-group i b { position: absolute; top: -1.6rem; left: 50%; transform: translateX(-50%); font-size: 0.9rem; font-style: normal; font-weight: normal; }
.vertical-chart .bar-group > span { position: absolute; bottom: -2.2rem; left: 50%; transform: translateX(-50%); font: 1rem "Source Code Pro", monospace; white-space: nowrap; }
.vertical-chart .vertical-legend { grid-column: 2 / 4; display: flex; justify-content: center; gap: 2rem; padding-top: 1.2rem; font-size: 1.1rem; }
.vertical-legend span::before { content: ""; display: inline-block; width: 1rem; height: 1rem; margin-right: 0.45rem; border: 0.1rem solid #666; background: #d3d3d3; vertical-align: -0.1rem; }
.vertical-legend span.after::before { background: #526d78; }

.assembly-comparison { width: min(94vw, 98rem); }
.assembly-grid { grid-template-columns: 1fr 1fr; gap: 1.2rem; min-width: 0; }
.assembly-grid pre { overflow-x: auto; font-size: 0.98rem; }

.value-buffer-diagram .buffer-json { margin-bottom: 1.4rem; font: 600 1.3rem "Source Code Pro", monospace; }
.value-buffer-diagram .buffer-json b { color: var(--report-red); }
.buffer-sequence { display: grid; grid-template-columns: 1fr auto 1fr auto 1fr; gap: 1rem; align-items: center; }
.buffer-sequence > section { align-self: stretch; padding: 0.9rem; border: 0.1rem solid #aaa; background: white; }
.buffer-sequence > section > strong, .buffer-sequence > section > small { display: block; }
.buffer-sequence > section > strong { color: var(--report-blue); font: 600 1.15rem "Source Code Pro", monospace; }
.buffer-sequence > section > small { min-height: 3rem; margin-top: 0.5rem; color: #666; }
.buffer-sequence > b { color: var(--report-red); font-size: 2rem; }
.buffer-cells { position: relative; display: flex; margin-top: 1.8rem; padding-top: 1.8rem; }
.buffer-cells span { flex: 1; display: grid; place-items: center; min-height: 4rem; border: 0.1rem solid var(--report-blue); background: #edf3f5; font: 1rem "Source Code Pro", monospace; text-align: center; }
.buffer-cells span + span { border-left: 0; }
.buffer-cells span.empty { color: #999; background: #fafafa; }
.buffer-cells.exact span { border-color: var(--report-red); background: #f8eeee; }
.buffer-cells i { position: absolute; top: 0; color: var(--report-red); font: 0.9rem "Source Code Pro", monospace; }
.buffer-cells i.mark { left: 0; }
.buffer-cells i.end { right: 0; }
.buffer-sequence section > p { margin: 1rem 0 0; color: #555; font-size: 1.05rem; text-align: left; }

.highlighted-listing { margin: 1.5rem 0; overflow-x: auto; background: #f8f8f8; }
.highlighted-listing pre { margin: 0; padding: 0.8rem 0; min-width: max-content; font-size: 1.35rem; }
.highlighted-listing code > span, .highlighted-listing code > mark { display: block; padding: 0 1rem; color: #222; background: transparent; }
.highlighted-listing code > mark { border-left: 0.35rem solid #777; background: #e5e5e5; }
.highlighted-listing .kw { color: #204a87; font-weight: bold; }
.highlighted-listing .dv { color: #0000cf; font-weight: normal; }

@media (max-width: 414px) {
  .stack-demo { grid-template-columns: 1fr; }
  .buffer-sequence { grid-template-columns: 1fr; }
  .buffer-sequence > b { transform: rotate(90deg); text-align: center; }
  .vertical-chart { grid-template-columns: 2rem 3.5rem 1fr; }
  .vertical-chart .plot { gap: 0.5rem; padding-left: 0.4rem; padding-right: 0.4rem; }
  .vertical-chart .bar-group i { width: 1.8rem; }
  .vertical-chart .bar-group > span { font-size: 0.8rem; }
}
</style>

[OxCaml][oxcaml][^fn-oxcaml] is Jane Street's performance-oriented version of OCaml, with features inspired in part by Rust.[^fn_rec_impact] OxCaml had been on my radar, but Anil Madhavapeddy's post about a [zero-allocation HTTP server][httpz] was what made me take a closer look. Fortunately, I also had a particular problem in mind: make JSON parsing go brr by implementing a [simdjson][simdjson]-style parser in native OxCaml.

I didn't want to copy simdjson blindly. Its borrowed DOM is a natural fit for its reusable tape, but I wanted an API that felt natural in OCaml: an ordinary, owned `Json.t` variant that callers could easily pass around and pattern-match on. That output contract introduced its own constraints and design choices, particularly around allocation and the garbage collector.

So [simdjson-oxcaml][simdjson-oxcaml][^fn-simdjson-oxcaml] exposes two APIs. The main one returns that owned `Json.t`; a lower-level borrowed tape isolates the cost of materializing the tree and makes comparison with C++ simdjson fairer. On simdjson's benchmark corpus, the owned parser reaches about 730 MB/s—roughly five times the throughput of existing OCaml parsers—and the tape reaches 1.25 GB/s. That's the *fast* part. The *not too fast* part is that even the borrowed tape remains 2.7 times slower than the original C++ implementation.

SIMD vectors, unboxed types, and bit-manipulation intrinsics let me express the algorithm directly in OxCaml. Making it fast still required profiling, benchmarking, and reading disassembly. CPU-level techniques transferred from C++. Allocation strategies had to fit OCaml's garbage collector. Some combinations of the new features also needed extensive annotations or library workarounds.

# A simdjson primer

[Simdjson][simdjson-paper] separates byte classification from grammar parsing (the figure below).[^fn-simdjson-paper] Its scanner produces a structural index. A top-down parser follows that index and writes the parsed document to a flat tape. The library’s DOM API is a navigation layer over the tape rather than a separately allocated tree.

<figure id="fig-parser-pipeline" class="report-figure ">
<div class="pipeline" role="img" aria-label="Raw bytes flow through a SIMD scanner to a structural index, then through a top-down parser to a tape; the DOM is a view over that tape.">
<span>raw bytes</span><b>→</b><span>SIMD scanner</span><b>→</b><span>structural index</span><b>→</b><span>top-down parser</span><b>→</b><span class="emph">tape</span><b class="down">↓</b><span class="emph">DOM view</span>
</div>
<figcaption>The simdjson architecture. The DOM API navigates a tape stored in memory owned by the parser.</figcaption>
</figure>

## Scanner and structural index

The scanner processes 64-byte blocks. It tracks quotes and escapes, validates UTF-8, and classifies punctuation, whitespace, and the first byte of each number or literal. It uses bit masks to track strings and vector table lookups to classify bytes.

The output is a *structural index*: byte positions at which parsing decisions can occur. In the figure below, it marks punctuation, the opening key quote, and the starts of `12` and `true`. It does not parse either atom; it lets the parser skip irrelevant positions.

<figure id="fig-structural-example" class="report-figure ">
<div class="indexed-json" role="img" aria-label="Byte positions 0 through 14 aligned over the JSON input object x containing the array 12 and true. Structural punctuation, the opening key quote, and scalar starts are highlighted.">
<div class="indexed-row indices"><span>0</span><span>1</span><span>2</span><span>3</span><span>4</span><span>5</span><span>6</span><span>7</span><span>8</span><span>9</span><span>10</span><span>11</span><span>12</span><span>13</span><span>14</span></div>
<div class="indexed-row chars"><b>{</b><b>&quot;</b><span>x</span><span>&quot;</span><b>:</b><b>[</b><b>1</b><span>2</span><b>,</b><b>t</b><span>r</span><span>u</span><span>e</span><b>]</b><b>}</b></div>
</div>
<figcaption>The scanner marks positions where parsing decisions can occur. Red characters are structural or pseudo-structural; they are not parsed values.</figcaption>
</figure>

C++ simdjson expands those masks into a flat array of 32-bit byte positions. The parser can then jump directly from one relevant position to the next.

## Parser and tape

The second stage is a top-down parser over the structural stream. It recurses into containers and dispatches quotes and atoms to string, number, or literal routines. SIMD remains useful in string parsing. Grammar and numbers are mostly scalar.

The tape stores parsed entries in document order. An opening container points to the first entry after its scope, and its closing entry points back to the opening entry (the figure below). Decoded strings live in a side byte buffer. These links let a DOM handle skip a complete value or iterate over a container without allocating a tree node for each value.

<figure id="fig-tape-layout" class="report-figure ">
<svg class="tape-svg" viewBox="0 0 900 260" role="img" aria-labelledby="tape-title tape-desc">
<title id="tape-title">Tape entries and container links</title><desc id="tape-desc">Nine tape cells in document order. Arrows above link opening root, object and array entries to the first entry after their scope. Arrows below link closing entries back to their openings.</desc>
<defs><marker id="tape-arrow" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="6" markerHeight="6" orient="auto"><path d="M0 0 L10 5 L0 10 z"/></marker></defs>
<g class="tape-links"><path d="M50 90 C90 0 810 0 850 90"/><path d="M150 90 C190 26 810 26 850 90"/><path d="M350 90 C390 50 710 50 750 90"/></g>
<g class="tape-labels"><text x="450" y="16">after root</text><text x="505" y="40">after object</text><text x="550" y="64">after array</text></g>
<g class="tape-cells">
<g class="container" transform="translate(10 90)"><rect/><text class="idx" x="40" y="17">0</text><text x="40" y="43">Root</text></g><g class="container" transform="translate(110 90)"><rect/><text class="idx" x="40" y="17">1</text><text x="40" y="43">{</text></g><g transform="translate(210 90)"><rect/><text class="idx" x="40" y="17">2</text><text x="40" y="43">&quot;x&quot;</text></g><g class="container" transform="translate(310 90)"><rect/><text class="idx" x="40" y="17">3</text><text x="40" y="43">[</text></g><g transform="translate(410 90)"><rect/><text class="idx" x="40" y="17">4</text><text x="40" y="43">Int 12</text></g><g transform="translate(510 90)"><rect/><text class="idx" x="40" y="17">5</text><text x="40" y="43">True</text></g><g class="container" transform="translate(610 90)"><rect/><text class="idx" x="40" y="17">6</text><text x="40" y="43">]</text></g><g class="container" transform="translate(710 90)"><rect/><text class="idx" x="40" y="17">7</text><text x="40" y="43">}</text></g><g class="container" transform="translate(810 90)"><rect/><text class="idx" x="40" y="17">8</text><text x="40" y="43">Root</text></g>
</g>
<g class="tape-links"><path d="M650 150 C610 208 390 208 350 150"/><path d="M750 150 C710 232 190 232 150 150"/><path d="M850 150 C810 254 90 254 50 150"/></g>
<g class="tape-labels"><text x="500" y="200">open array</text><text x="450" y="224">open object</text><text x="450" y="248">root</text></g>
</svg>
<figcaption>Tape entries for <code>{&quot;x&quot;:[12,true]}</code>. Opening entries point past their scope; closing entries point back to the matching opening entry. Payload packing is simplified.</figcaption>
</figure>

Simdjson’s DOM values are lightweight handles into the tape. The parser owns and reuses that storage, so parsing another document with the same parser invalidates the previous document and any values obtained from it ([simdjson DOM API][simdjson-dom]).[^fn-simdjson-dom]

```cpp
simdjson::dom::parser parser;
auto doc = parser.parse(input).value();
auto name = doc["name"];

auto next = parser.parse(next_input).value();
// doc and name are now invalid
```

Simdjson benchmarks one warmed parser that reuses these buffers.

# The simdjson-oxcaml parser

## APIs and result

I applied this architecture in OxCaml, but made the main result an owned algebraic data type:

```ocaml
module Json : sig
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float#
    | String of string
    | Object of assoc array
    | Array of t array
  and assoc = { key : string; value : t }
end
```

The returned `Json.t` is independent of the parser: code can pattern-match on it and parse another document without invalidating it. A second API exposes the borrowed tape. The two paths share the scanner and parser, so their difference measures tree materialization.

The chart below gives the main result. The reusable and one-shot DOM rows return the same `Json.t`. Only the former keeps grown scratch buffers between calls. The benchmarking section at the end describes the setup.

<figure id="fig-throughput" class="report-figure wide">
<div class="academic-chart" role="img" aria-label="Horizontal throughput chart. Jsont 140, Yojson 150, OxCaml one-shot DOM 660, reusable DOM 730, RapidJSON 1060, OxCaml tape 1250, sajson 1650, and C++ simdjson 3350 megabytes per second.">
<div class="axis"><span>0</span><span>1,000</span><span>2,000</span><span>3,000 MB/s</span></div>
<div class="chart-row"><span>Jsont <small>OCaml · DOM · one-shot</small></span><i style="--value:4.18%"></i><b>140</b></div>
<div class="chart-row"><span>Yojson.Safe <small>OCaml · DOM · one-shot</small></span><i style="--value:4.48%"></i><b>150</b></div>
<div class="chart-row"><span>simdjson-ox <small>OxCaml · DOM · one-shot</small></span><i class="ox-light" style="--value:19.7%"></i><b>660</b></div>
<div class="chart-row"><span>simdjson-ox <small>OxCaml · DOM · reusable</small></span><i class="ox" style="--value:21.8%"></i><b>730</b></div>
<div class="chart-row"><span>RapidJSON <small>C++ · DOM · one-shot</small></span><i style="--value:31.6%"></i><b>1,060</b></div>
<div class="chart-row"><span>simdjson-ox <small>OxCaml · tape · reusable</small></span><i class="ox-dark" style="--value:37.3%"></i><b>1,250</b></div>
<div class="chart-row"><span>sajson <small>C++ · tape · one-shot</small></span><i style="--value:49.3%"></i><b>1,650</b></div>
<div class="chart-row"><span>C++ simdjson <small>C++ · tape · reusable</small></span><i style="--value:100%"></i><b>3,350</b></div>
</div>
<figcaption>Geometric-mean throughput over seven inputs, rounded to the nearest 10 MB/s. DOM denotes a materialized tree; parser-owned flat storage is labelled tape, including simdjson's DOM API.</figcaption>
</figure>

The owned parser is five times faster than the existing OCaml rows; the tape is faster again but remains well behind C++ simdjson. The features behind the improvement are standard in systems languages. A five-fold gain from adding them suggests that regular OCaml can leave meaningful performance untapped when code cannot express the representations or operations it needs.

# What OxCaml made possible

Four OxCaml features let me implement the complete parser in OCaml, including the SIMD-heavy scanner and string decoder.

## Unboxed types

Unboxed types carry data outside the ordinary OCaml *value* layout. In the [OCaml value representation][ocaml-representation],[^fn-ocaml-representation] a standalone `float` normally points to a heap block; `float#` carries the payload directly (the figure below). Unboxed products and records hold scanner state and results without intermediate blocks. Built-in variants such as `or_null` similarly avoid an option block.

<figure id="fig-unboxed-layout" class="report-figure ">
<div class="memory-layout" role="img" aria-label="An ordinary float is a pointer to a heap block containing a header and payload; float sharp carries the payload directly."><div><strong>ordinary <code>float</code></strong><span>pointer</span><b>→</b><span class="stacked">header <small>Double_tag</small><hr>64-bit payload</span><small>two-word heap block</small></div><div><strong><code>float#</code></strong><span>64-bit payload</span><small>no pointer or header</small></div></div>
<figcaption>Source-level representation of boxed and unboxed floating-point values on a 64-bit target. Optimizations may eliminate particular boxes.</figcaption>
</figure>

## SIMD support

OxCaml exposes built-in SIMD vector types. Loading 16 bytes produces one `int8x16#`; comparing it with a vector containing 16 quote bytes performs the same comparison in every lane (the figure below). Four such loads cover one 64-byte scanner block.

<figure id="fig-simd-lanes" class="report-figure ">
<div class="simd-figure" role="img" aria-label="Sixteen input byte lanes compared with sixteen quote bytes, producing matching lanes at positions 1, 3, 7, and 9."><div class="lane-label">input</div><div class="lanes"><span>{</span><b>&quot;</b><span>x</span><b>&quot;</b><span>:</span><span>[</span><span>1</span><b>&quot;</b><span>a</span><b>&quot;</b><span>,</span><span>t</span><span>r</span><span>u</span><span>e</span><span>]</span></div><div class="lane-op">compare each lane with <code>'"'</code> ↓</div><div class="lanes mask"><span>0</span><b>1</b><span>0</span><b>1</b><span>0</span><span>0</span><span>0</span><b>1</b><span>0</span><b>1</b><span>0</span><span>0</span><span>0</span><span>0</span><span>0</span><span>0</span></div></div>
<figcaption>A 16-lane byte comparison. Each output lane records whether the input byte is a quote; a movemask later reduces the vector result to bits.</figcaption>
</figure>

## Bit-manipulation intrinsics

Population count and count-trailing-zeros intrinsics count structural positions and extract the next set bit from a mask. When lowered correctly, each operation becomes a single CPU instruction: `popcnt` or `tzcnt`.

## Stack allocation and allocation checks

`stack_` allocates a local value on the stack instead of the GC-managed heap. In the example below, both records are allocated, but the stack-allocated record is reclaimed when the function returns.

<figure id="fig-stack-allocation" class="report-figure ">
<div class="stack-demo"><pre><code class="sourceCode ocaml"><span class="kw">type</span> point = { x : <span class="dt">int</span>; y : <span class="dt">int</span> }

<span class="kw">let</span> make_points () =
  <span class="kw">let</span> heap = { x = <span class="dv">1</span>; y = <span class="dv">2</span> } <span class="kw">in</span>
  <span class="kw">let</span> local = stack_ { x = <span class="dv">3</span>; y = <span class="dv">4</span> } <span class="kw">in</span>
  print_int (local.x + local.y);
  heap</code></pre><div class="stack-memory"><section><strong><code>heap</code> · returned</strong><div class="heap-region">GC-managed heap<br><code>{ x = 1; y = 2 }</code></div><small>returned value must survive the call</small></section><section><strong><code>local</code> · used here</strong><div class="stack-region"><em><code>make_points</code> region</em><span><code>stack_ point</code><b>x = 3</b><b>y = 4</b></span></div><small>stack region is reclaimed on return</small></section></div></div>
<figcaption>Heap and stack allocation in one function. Stack allocation changes where a local value is stored; it does not remove the allocation.</figcaption>
</figure>

`[@zero_alloc]` verifies that a function does not allocate on the OCaml heap. `[@zero_alloc opt]` performs the same check after optimization. I use the latter where inlining and scalar replacement can remove an intermediate aggregate. Later, I show why eliminating an aggregate mattered more than moving it to the stack.

# Making the parser fast

The first faithful scanner implementation ran at 400 MB/s on `twitter.json`; adding parsing on top would only make it slower. The current tape parser reaches 1.4 GB/s on the same input. Benchmarks, profiles, and generated code showed that the scanner loop allocated, small helpers remained calls, and some intrinsics became software helpers.

The work fell into three themes:

1.  CPU-level techniques transferred directly from C++;

2.  allocation and representation had to fit a generational collector; and

3.  some combinations of new features needed compiler annotations or library workarounds.

The final scanner produces packed structural-index masks at 6 GB/s. A baseline that only traverses the input with the same SIMD loads and computes a checksum reaches 22 GB/s (the figure below); this is the approximate throughput ceiling for the scanner on this machine. Classification, UTF-8 validation, quote tracking, and mask construction consume most of that headroom.

<figure id="fig-scanner-ceiling" class="report-figure ">
<div class="ratio-bars" role="img" aria-label="Scanner throughput 6 gigabytes per second versus load and checksum ceiling 22 gigabytes per second."><div><span>packed scanner</span><i style="--value:27.3%"></i><b>6 GB/s</b></div><div><span>load + checksum</span><i style="--value:100%"></i><b>22 GB/s</b></div></div>
<figcaption>Final packed structural-index production against a baseline that performs the same SIMD loads and checksum without classification.</figcaption>
</figure>

## CPU-level techniques transferred from C++

The usual CPU-level techniques worked: unroll common cases, outline rare ones, fuse passes, and interleave independent work. I kept changes only when benchmarks and profiles showed an improvement. The following examples show how these techniques appeared in the scanner and parser.

### Interleaving scalar and vector work

Writing a structural mask also runs `popcnt` and updates the index count. The original loop did this after classifying block N. The revised loop loads N, writes and counts the mask for N-1, then classifies N (the figure below). This exposes scalar work while vector loads are in flight.

<figure id="fig-delayed-mask-write" class="report-figure ">
<div class="schedule"><div><strong>immediate</strong><span class="vector">4 × 16-byte SSE loads<br>block N</span><span class="vector">SIMD classify N</span><span class="scalar">store + <code>popcnt</code> N</span></div><div><strong>delayed</strong><span class="vector">4 × 16-byte SSE loads<br>block N</span><span class="scalar">store + <code>popcnt</code> N−1</span><span class="vector">SIMD classify N</span></div></div>
<figcaption>Delaying structural-mask writes by one block exposes overlap between scalar bookkeeping and the next block's vector loads. On <code>citm_catalog</code>, the delayed schedule took 1.70 s versus 1.76 s for the immediate schedule and retired slightly more instructions.</figcaption>
</figure>

Backend stalls fell from 25% to 22%, even though the new loop completed (“retired”) slightly more instructions. Exposing independent work mattered more than minimizing the count.

### Fusing string scanning and copying

While parsing a JSON string, the parser must find its closing quote, decode any escapes, and store the decoded bytes. For the tape, those bytes go into parser-owned storage. C++ simdjson’s [`copy_and_find`][simdjson-copy-and-find] loop[^fn-simdjson-copy-and-find] copies each SIMD block while looking for the closing quote or a backslash. My first implementation had preserved the SIMD search but lost the fusion: it found the end of an ordinary span, then passed that span to `Stdlib.Buffer.add_substring` to read and copy it a second time.

On `twitter`, decoded strings average 15–20 bytes, and `add_substring`’s checks and separate runtime blit accounted for 8% of sampled cycles. A reusable `Bytes.t` exposed the write position and let each 16-byte iteration:

1.  load the block from the JSON input;

2.  store all 16 bytes at the current output position;

3.  compute the special-byte mask and find its first set bit; and

4.  advance the output position by only the bytes before that bit.

The store is speculative. Bytes after a quote or backslash are written but not committed and are overwritten later. A quote ends the string; a backslash enters the scalar escape decoder (the figure below).

<figure id="fig-string-copy" class="report-figure ">
<div class="flow"><span>load 16 input bytes</span><b>→</b><span>store 16 bytes</span><b>→</b><span>find quote / <code>&#92;</code></span><b>→</b><span>commit prefix</span></div><div class="speculative"><b>committed bytes</b><i>speculatively written; overwritten later</i></div>
<figcaption>Restoring simdjson's <code>copy_and_find</code> structure. Each loaded SIMD block is stored immediately, but only the prefix before a quote or backslash is committed.</figcaption>
</figure>

The chart below shows gains of 17–28% on string-heavy fixtures. The owned path still uses `String.sub` for ordinary strings because an owned OCaml string requires a final allocation and copy.

<figure id="fig-string-buffer" class="report-figure ">
<div class="vertical-chart" role="img" aria-label="Grouped vertical bar chart. Twitter improves from 1250 to 1470 MB/s, apache builds from 1230 to 1570, and citm catalog from 1830 to 2040."><div class="y-label">Throughput (MB/s)</div><div class="y-ticks"><span>2,000</span><span>1,500</span><span>1,000</span><span>500</span><span>0</span></div><div class="plot"><div class="v-grid"></div><div class="bar-group"><i class="before" style="--value:54.3%"><b>1,250</b></i><i class="after" style="--value:63.9%"><b>1,470</b></i><span>twitter</span></div><div class="bar-group"><i class="before" style="--value:53.5%"><b>1,230</b></i><i class="after" style="--value:68.3%"><b>1,570</b></i><span>apache_builds</span></div><div class="bar-group"><i class="before" style="--value:79.6%"><b>1,830</b></i><i class="after" style="--value:88.7%"><b>2,040</b></i><span>citm_catalog</span></div></div><div class="vertical-legend"><span class="before">Stdlib.Buffer</span><span class="after">Flat buffer + fused copy</span></div></div>
<figcaption>Effect of the flat decoded-string buffer and fused scan-and-copy on string-heavy fixtures (OxCaml tape, reusable).</figcaption>
</figure>

### The decimal-integer loop

The standard OCaml number-conversion functions were far too slow for this parser, so it uses a dedicated integer parser and the Clinger and Lemire fast paths for floating-point conversion ([Lemire 2021][lemire-number]).[^fn-lemire-number] Decimal integers start with an eight-digit SWAR (“SIMD within a register”) fast path. A scalar loop handles the remaining digits. The comparison below shows 19 instructions per digit in OxCaml against eight in C++. Both compute `v = 10v + d`. OxCaml also handles tagged positions and digits, an explicit bound, and a runtime poll—the safe-point check that lets the runtime interrupt long-running native code.

<figure id="fig-digit-assembly" class="report-figure wide assembly-comparison">
<div class="assembly-grid">
<section><header><strong>C++ simdjson</strong><b>8 instructions</b></header><pre><code><span class="label">.loop:</span>
<span class="useful"> imul   $0xa,%rdx,%rdx   # value *= 10</span>
<span class="useful"> movzbl %cl,%ecx         # digit</span>
<span class="useful"> add    $0x1,%rax        # next byte</span>
<span class="useful"> add    %rcx,%rdx        # value += digit</span>
<span class="useful"> movzbl (%rax),%r12d     # load byte</span>
<span class="useful"> lea    -0x30(%r12),%ecx # byte - '0'</span>
<span class="useful"> cmp    $0x9,%cl         # digit?</span>
<span class="useful"> jbe    .loop</span></code></pre></section>
<section><header><strong>OxCaml tape</strong><b>19 instructions</b></header><pre><code><span class="label">.loop:</span>
<span class="runtime"> mov    %rsi,%rax</span>
<span class="runtime"> sar    $1,%rax              # untag pos</span>
<span class="useful"> movzbq 0(%rbp,%rax),%rax    # load byte</span>
<span class="runtime"> lea    -0x5f(%rax,%rax),%r8 # tagged digit</span>
<span class="useful"> cmp    $1,%r8</span>
<span class="useful"> jl     .done</span>
<span class="useful"> cmp    $0x13,%r8</span>
<span class="useful"> jg     .done                # digit range</span>
<span class="runtime"> add    $2,%rsi              # tagged pos++</span>
<span class="runtime"> cmp    %r13,%rsi</span>
<span class="runtime"> setl   %al</span>
<span class="runtime"> movzbq %al,%rax             # bound value</span>
<span class="runtime"> sar    $1,%r8               # untag digit</span>
<span class="useful"> imul   $0xa,%rdi,%rdi</span>
<span class="useful"> add    %r8,%rdi             # value = 10v+d</span>
<span class="poll"> cmp    (%r14),%r15          # runtime poll</span>
<span class="poll"> jbe    .poll</span>
<span class="runtime"> test   %rax,%rax            # test bound</span>
<span class="useful"> jne    .loop</span></code></pre></section>
</div>
<div class="assembly-legend"><span class="useful">parsing work</span><span class="runtime">representation / explicit control</span><span class="poll">runtime poll</span></div>
<figcaption>Exact decimal-integer loop bodies from the linked x86 binaries. The colours classify work; 19 versus 8 is an instruction count, not a time ratio.</figcaption>
</figure>

The OxCaml loop clearly does more work, but it does not necessarily take 19/8 as long. Modern out-of-order CPUs can execute independent instructions in parallel. For example, some representation and bounds-checking work can overlap the integer multiplication. Instruction count exposes the extra work, while benchmarks determine its actual cost. Unrolling the first scalar digit shows the difference: it increased the number of retired instructions but removed a common back edge and runtime poll, producing the largest gain in the corresponding table.

<div class="report-table">

| Checkpoint                           | Tape MB/s | Instructions | Branches |
|:-------------------------------------|----------:|-------------:|---------:|
| Investigation baseline               |       840 |        45.1B |     8.0B |
| Remove redundant finite checks       |       860 |        43.9B |     7.5B |
| Unroll first scalar tail digit       |       890 |        44.3B |     7.4B |
| Outline cold paths, trim scan result |       900 |        42.1B |     7.2B |

<div class="table-caption" id="tab:number-checkpoints">Selected number-parser checkpoints on `numbers.json`. The largest throughput gain increased the retired-instruction count.</div>

</div>

Outlining rare integer and float paths also reduced register pressure and code size. Routing every float through substring allocation and `float_of_string_opt`, by contrast, reduced `numbers.json` to 170 MB/s. Native decimal conversion is essential.

## Memory behaviour did not transfer

OCaml allocates into a minor heap and promotes survivors. Old-to-young pointers require a write barrier, and references left in reusable buffers keep objects reachable for longer. The structural index, the parser's internal buffers for accumulating array and object elements, and the result tree therefore need different allocation strategies.

### Removing allocation from the scanner loop

The first scanner expanded each mask into positions and appended them to `Dynarray`. Each live slot is an `Elem` block. Clearing replaces it with `Empty`, so refilling allocates a block per index even when capacity is kept.

Packed masks remove per-index appends: input length determines an exact `int64# array`, with one word per block (the figure below).

<figure id="fig-dynarray-packed" class="report-figure">
<div class="packing-compare" role="img" aria-label="Dynarray slots point to separately allocated Elem blocks, while the packed representation stores one unboxed 64-bit mask for each 64-byte input block."><section><strong>Dynarray</strong><div class="pointer-slots"><span>ptr</span><span>ptr</span><span>…</span></div><div class="pointer-arrows"><i>↓</i><i>↓</i></div><div class="elem-slots"><span><code>Elem 0</code></span><span><code>Elem 5</code></span></div><small>one new <code>Elem</code> block per structural position</small></section><section><strong>packed</strong><div class="mask-slots"><span>64-bit mask</span><span>64-bit mask</span></div><small>one unboxed word per 64-byte input block</small></section></div>
<figcaption>Replacing expanded structural positions with packed masks. Keeping Dynarray capacity does not keep its <code>Elem</code> blocks: refilling it allocates again. The packed array is allocated once at its final size.</figcaption>
</figure>

The fold still allocated two boxed records per 64 bytes:

```ocaml
Simd.Int8x64.String.fold_blocks input
  ~init:{ state = initial_state; block_idx = 0 }
  ~f:(fun { state; block_idx } block ->
    let result = scan_block state block in
    Structure.set_mask structure block_idx
      result.structural_mask;
    { state = result.state; block_idx = block_idx + 1 })
```

Unboxed products kept both records’ fields in loop state. Together with packed output, this moved the scanner to 2 GB/s.

### Representing the structural index

The scanner naturally produces masks. The parser consumes positions. I compared packed masks with C++ simdjson’s flat 32-bit indices (the figure below). Dynarray remains in the benchmark only as the clearly slower baseline. Packed won overall, although flat was faster when its worst-case allocation was already warm.

<figure id="fig-index-representations" class="report-figure ">
<div class="index-compare"><div><strong>packed masks</strong><span><code>64-bit mask</code><code>64-bit mask</code><code>…</code></span><small>exact size · one bit per input byte</small></div><div><strong>flat indices</strong><span><code>0</code><code>1</code><code>4</code><code>5</code><code>…</code></span><small>direct positions · four bytes per input byte reserved</small></div></div>
<figcaption>The two viable scanner-to-parser representations. Packed keeps the scanner's native masks. Flat expands them to direct positions.</figcaption>
</figure>

<div class="report-table">

| Allocation | Workload          | Packed | Flat | Dynarray |
|:-----------|:------------------|-------:|-----:|---------:|
| Reusable   | produce           |    6.0 |  4.1 |      1.0 |
| Reusable   | produce + consume |    3.2 |  3.5 |      0.9 |
| One-shot   | produce           |    4.9 |  1.1 |      0.4 |
| One-shot   | produce + consume |    2.9 |  1.0 |      0.3 |

<div class="table-caption" id="tab:index-repr">Structural-index throughput in GB/s, geometric mean.</div>

</div>

After warmup, packed and flat allocate no reported words. Flat is about 10% faster when consuming every position, but reserves four bytes per input byte against one bit for packed. On `canada.json`, that is 9 MB versus 280 KB.

End to end, flat gains 4% with a reusable parser but loses 25% one-shot because each call initializes its worst-case reservation (the figure below). Packed uses 32 times less capacity and works well in both modes.

<figure id="fig-index-dom" class="report-figure ">
<div class="mode-compare"><div><strong>reusable parser</strong><span>packed <b>717 MB/s</b></span><span>flat <b>742 MB/s</b></span><em>flat +3.5%</em></div><div><strong>one-shot parser</strong><span>packed <b>646 MB/s</b></span><span>flat <b>486 MB/s</b></span><em>flat −24.7%</em></div></div><div class="capacity"><span>packed <i style="--value:3.125%"></i><b>input / 8</b></span><span>flat <i style="--value:100%"></i><b>input × 4</b></span></div>
<figcaption>Flat helps only when its four-bytes-per-input-byte reservation is already warm. Values come from a controlled packed-versus-flat A/B run.</figcaption>
</figure>

C++ simdjson benchmarks a warmed parser, making flat preallocation look almost free. The one-shot result shows why usage mode matters.

### The cost of owning `Json.t`

After warmup, the 1,250 MB/s tape reports no minor or major allocation: its words and string buffer contain no OCaml pointers. Materializing `Json.t` drops throughput to 730 MB/s. On `canada.json`, an earlier profile attributed 40% of cycles to allocation and GC runtime.

#### Numeric leaves

Numbers expose the largest obvious opportunity. On `numbers.json`, the owned parser reaches 560 MB/s against 890 MB/s for the tape. `float#` avoids a separate float box, but `Json.Float` and `Json.Int` still allocate one variant block per leaf. Those blocks remain reachable through the result tree and may be promoted by the GC.

User-defined unboxed variants could flatten numeric alternatives while preserving the pattern-matchable API. OxCaml currently provides only built-ins such as `or_null`.[^fn-unboxed-variants] This remains an unmeasured but likely large opportunity.

#### Accumulating arrays and objects

Containers add a different source of allocation and GC work. Container length is unknown until the closing bracket. The parser therefore marks a shared scratch buffer, appends elements, copies the completed slice into the newly allocated array or object in the resulting `Json.t`, and restores the mark (the figure below).

<figure id="fig-value-buffer" class="report-figure ">
<div class="value-buffer-diagram"><div class="buffer-json"><code>parse_array</code> on {&quot;x&quot;:<b>[12,true]</b>}</div><div class="buffer-sequence"><section><strong>1 · enter array</strong><small>parser-owned <code>Value_buffer</code></small><div class="buffer-cells"><i class="mark">mark ↓</i><span class="empty">·</span><span class="empty">·</span></div><p>save current scratch end</p></section><b>→</b><section><strong>2 · parse values</strong><small>parser-owned <code>Value_buffer</code></small><div class="buffer-cells"><i class="mark">mark ↓</i><i class="end">end ↓</i><span>Int 12</span><span>Bool true</span></div><p>append each owned <code>Json.t</code></p></section><b>→</b><section><strong>3 · see <code>]</code></strong><small>exact <code>Json.Array</code></small><div class="buffer-cells exact"><span>Int 12</span><span>Bool true</span></div><p>copy slice · reset scratch end</p></section></div></div>
<figcaption>Building an array whose length is unknown at the opening bracket. A mark lets nested containers share one parser-owned scratch buffer.</figcaption>
</figure>

A reusable parser amortizes growth, but its scratch arrays eventually become old. Appending a young `Json.t` then triggers the write barrier and keeps the value reachable until its slot is cleared. Each element is also written to scratch and to the final result. This works well for long containers but poorly for small ones.

The parser therefore keeps up to four elements in local variables and constructs the result array or object directly. Larger containers spill to scratch. Despite the extra branching in the code, this was still a win because it avoided scratch writes, write barriers, and stale buffer references that could keep completed values alive. This improved `canada` and `citm_catalog` by 14%. For spills, `Array.sub` allocates and copies the result in one operation. A high-water mark also lets the parser clear stale references once per parse rather than once per container, improving representative reusable DOM rows by 8%.

We rejected several alternatives after benchmarking them. Pre-sizing 4,096 pointer slots made `twitter.json` one third slower by placing the buffer directly in the major heap. Fresh or growing per-container buffers created more minor-heap traffic. A 32M-word minor heap changed optimized `canada.json` throughput by only 1%. Pointer placement and reachability mattered more than collector tuning.

### Stack allocation removed heap traffic, not the work

The number parser returns short-lived records for digit runs and number parts. Adding `stack_` removed their accidental heap allocation, but not record construction or calls.

With inline annotations, scalar replacement removes the records. Without them, the records allocate and throughput falls. Restoring only `stack_` gives zero heap words but remains slower because the aggregate work survives (the corresponding table).

<div class="report-table">

| Build | DOM numbers | DOM twitter | Tape numbers | Tape twitter |
|:---|---:|---:|---:|---:|
| Manual inlining (default build) | 510 | 1,070 | 900 | 1,480 |
| Automatic inlining (`-O3`) | 370 | 960 | 660 | 1,210 |
| plus explicit `stack_` | — | — | 630 | 1,210 |

<div class="table-caption" id="tab:inline-presets">Inlining and allocation experiment, MB/s. Values are medians of three pinned-CPU runs; checksums were identical.</div>

</div>

The fast case is not “the same allocation, on the stack.” It is no allocation and no aggregate work at all. Heap allocation puts a ceiling on throughput. Inlining and scalar replacement are what remove the execution cost.

OxCaml does not provide a simple escape hatch from the GC. Stack allocation and unboxed types provide more control over allocation and representation, but the collector still shapes the design. Fast code must account for generations, write barriers, promotion, reachability, inlining, and scalar replacement. OxCaml makes that code easier to express, but using its low-level features still requires understanding and sometimes working around the runtime.

## Some feature combinations were still rough

The core features worked, but combinations of new layouts, SIMD values, and templates exposed optimizer and library gaps.

### Inlining needed extensive manual guidance

The scanner ultimately needed twenty-six forced-inline annotations. Together they roughly doubled its throughput. One surprising miss was a tiny local helper inside `Chars.process`. The listing highlights `make_group_mask`. Both call sites are immediately below it.

<div class="highlighted-listing"><pre><code class="sourceCode ocaml"><span><b class="kw">let</b> process block =</span>
<span>  <b class="kw">let</b> low_class = lookup ~table:low_lut block <b class="kw">in</b></span>
<span>  <b class="kw">let</b> high_class =</span>
<span>    block |&gt; high_nibbles |&gt; lookup ~table:high_lut</span>
<span>  <b class="kw">in</b></span>
<span>  <b class="kw">let</b> klass = low_class <b class="kw">land</b> high_class <b class="kw">in</b></span>
<mark><b class="kw">  let</b> make_group_mask group =</mark>
<mark>    equal (klass <b class="kw">land</b> group) (zero ())</mark>
<mark>    |&gt; Int64_u.lnot</mark>
<span>  <b class="kw">in</b></span>
<span>  <b class="kw">let</b> operator_mask =</span>
<span>    make_group_mask vector_group_operators</span>
<span>  <b class="kw">in</b></span>
<span>  ...</span></code></pre></div>

It survived as a call in the hot loop, as did similar boundaries across the scanner. `-O3` did not substitute for the annotations: removing them lost 10–30% depending on path and fixture (the corresponding table).

A parser example showed the same problem as an allocation. The original literal matcher contained a recursive local helper:

<div class="highlighted-listing"><pre><code class="sourceCode ocaml"><span><b class="kw">let</b> expect_literal state ~start literal value =</span>
<span>  <b class="kw">let</b> literal_length = String.length literal <b class="kw">in</b></span>
<mark>  <b class="kw">let rec</b> loop offset =</mark>
<mark>    <b class="kw">if</b> offset = literal_length <b class="kw">then</b></mark>
<mark>      require_boundary state (start + offset)</mark>
<mark>    <b class="kw">else if</b> state.input.[start + offset] = literal.[offset]</mark>
<mark>    <b class="kw">then</b> loop (offset + <b class="dv">1</b>)</mark>
<mark>    <b class="kw">else</b> invalid_syntax ()</mark>
<span>  <b class="kw">in</b> loop <b class="dv">0</b>; value</span></code></pre></div>

Flambda2 did not lift `loop`, so each literal allocated a closure. Moving it to the top level removed the allocation.

### Intrinsics did not always lower to the instructions they name

In the installed package snapshot,[^fn-intrinsics-snapshot] the popcount and count-trailing-zeros externals lacked `[@@builtin]`. The backend supported both, but popcount became an OCaml runtime call and then libgcc’s `__popcountdi2`.

A local `[@@builtin]` declaration produced one `popcnt` and improved the scanner by 10%; count-trailing-zeros needed the same fix. Later package updates restored the metadata. Only machine-code inspection exposed the issue.

### Non-value layouts need library support

The first object representation used `assoc# array`. Here `assoc#` is an unboxed pair-like record that stores the key and JSON value directly with layout `value & value`, removing one record per object field. While parsing an object of unknown size, however, the association scratch buffer must append fields, grow when full, copy the completed slice into the owned object, clear references to parsed values, and reuse its capacity for the next parse.

Neither Stdlib nor Base exposed bulk copy and fill for this layout. A custom binding to internal `%arrayblit` expanded to an OCaml loop with two `caml_modify` calls per product store. Clearing required another such loop because no matching fill was available.

Boxed associations restore ordinary value arrays: slicing becomes one `caml_array_blit`, clearing one `caml_array_fill`, and the runtime batches barrier work.

Depending on the fixture, unboxed associations ranged from a 10% loss to a 10% win. A better element layout helps only when libraries support its full lifecycle.

Templates can define operations for several layouts, but case lists must repeat product kinds: the grammar contains kind-abbreviation syntax that is not exposed to users. Support for non-value layouts remains spotty, and complete template families are cumbersome to write and maintain.

### Practical guidance is sparse

The new features are documented individually, but there is little practical material on using them together with Flambda2 for performance work. Much of this investigation started with `ocamlopt -help`, compiler source, profiles, and disassembly. End-to-end examples of performance engineering with these features would make the toolchain easier to learn.

# Where the remaining gap comes from

The tape runs 2.7 times slower than C++ simdjson. Some of that gap is elbow grease: simdjson has years of target-specific tuning, while the number-parser checkpoint table shows that my newer implementation was still finding gains. Tuning is not the whole explanation, however. The OxCaml parser also executes more work.

On a complete `canada.json` parse, the OxCaml tape path executes about 33 instructions per input byte against about 16 for C++ simdjson (the figure below). The instruction-count ratio does not identify where the cycles are spent, but it shows that the OxCaml parser performs about twice as much work per byte. The decimal loop above shows one source: representation work and a runtime poll around the same digit recurrence.

<figure id="fig-whole-parser-instructions" class="report-figure ">
<div class="instruction-compare" role="img" aria-label="C++ simdjson 16 instructions per byte; OxCaml tape 33 instructions per byte."><div><span>C++ simdjson</span><i style="--value:48.5%"></i><b>16 instructions / byte</b></div><div><span>OxCaml tape</span><i class="ox" style="--value:100%"></i><b>33 instructions / byte</b></div></div>
<figcaption>Whole-document instruction count on <code>canada.json</code>. The OxCaml tape path retires about twice as many instructions per input byte.</figcaption>
</figure>

Instruction count does not predict time, but the ratio confirms extra work across scalar parsing, representation, and runtime bookkeeping. Tuning can shrink some of it. The current OCaml representation and runtime require some of it.

# Conclusions

SIMD, unboxed types, and integer intrinsics let me implement simdjson directly in OxCaml. The owned parser is five times faster than existing OCaml parsers, which suggests that regular OCaml can leave meaningful performance untapped when code cannot express the representations or operations it needs. Numeric variant blocks remain costly. I believe user-defined unboxed variants can bring the parser closer to ten times the OCaml baseline, but that estimate is unmeasured.

The algorithm mapped naturally, but the first version was still 3.5 times slower than the current one. CPU-level techniques transferred from C++. Finding where to apply them required benchmarks, profiles, and generated code.

Memory-related choices were runtime-specific. Stack allocation removed GC traffic, but inlining and scalar replacement made code fast by removing aggregates altogether. Reusable storage suited the tape. The owned tree needed small-container specialization and buffer strategies designed around the GC. Writing fast OxCaml therefore requires understanding both how the generated machine code runs on the CPU and how the OCaml runtime manages allocation and pointers. More predictable inlining and broader library support for new layouts would make that work easier.

# Benchmarking and testing setup

## Machine and harness

Unless noted, figures use the 15 August snapshot on Fedora 43 and OxCaml 5.2.0+ox. Benchmarks were pinned to one Golden Cove performance core, with the performance governor and ASLR disabled. Boost remained enabled, so small differences are noise. OCaml rows use Bechamel and C++ rows use Google Benchmark. Hardware counters use the same core. Throughput is rounded to 10 MB/s, except the structural-index table in GB/s.

## Usage modes

*Reusable* creates one parser and keeps its grown buffers. C++ simdjson benchmarks this mode. *One-shot* creates parser state per document, including buffer allocation and initialization. Both return the same `Json.t`.

## What the rows measure

Yojson, Jsont, simdjson-ox DOM, and RapidJSON return owned trees. The OxCaml, simdjson, and sajson tape rows return handles into parser-owned storage. Every row completes parsing, but ownership differs. Driver checksums catch missing work. They are not cross-language equivalence tests for integer or UTF-8 policy.

The seven-fixture geometric mean is a useful summary but hides real variation, as the per-fixture table shows. The borrowed tape is ahead of RapidJSON on object- and string-heavy fixtures and behind it on `numbers`, `canada`, and `marine_ik`. Those are also the cases where C++ simdjson loses much of its lead, because number parsing is scalar work.

<div class="report-table">

| Fixture | Ox DOM | Ox tape | RapidJSON | C++ simdjson |
|:---|---:|---:|---:|---:|
| `apache_builds` | 680 | 1,560 | 870 | 5,840 |
| `citm_catalog` | 1,600 | 2,070 | 1,650 | 5,560 |
| `gsoc-2018` | 1,090 | 2,260 | 1,090 | 6,520 |
| `twitter` | 1,110 | 1,430 | 1,050 | 5,520 |
| `numbers` | 560 | 890 | 1,050 | 1,640 |
| `canada` | 430 | 760 | 990 | 1,530 |
| `marine_ik` | 350 | 670 | 890 | 1,610 |
| Geometric mean | 730 | 1,250 | 1,060 | 3,350 |

<div class="table-caption" id="tab:per-fixture">Per-fixture throughput from the August snapshot, rounded to the nearest 10 MB/s. OCaml rows use reusable parsers.</div>

</div>

## Correctness

The parser passes all 105 vendored JSONChecker cases. Tests compare tape with tree, reusable with one-shot, and SIMD with an independent scalar scanner. QuickCheck covers complete values, strings, numeric edge cases, and invalid syntax. Errors lack stable positions and categories.

## Use of AI tools

AI coding agents were part of the implementation workflow, primarily Codex with the [ocaml-codex](https://github.com/artempyanykh/ocaml-codex) plugin. They worked best on tasks with an automatic check: tests, benchmark and corpus wiring, and well-specified experiments. They were also useful as fuzzy search across compiler and package sources. Tasks that depended on judgement, such as API design, representation choices, and deciding what to measure, were a poor fit. Even checkable work required steering. Tests, benchmarks, profiles, and disassembly decided which changes stayed.

## Scope and limitations

The public library needs API polish, better errors, broader platform support, and production use. The scanner uses four 128-bit x86 SSE vectors per block. Wider AVX types need runtime dispatch, while ARM support waits on upstream SIMD interfaces.


## References and notes

[^fn_rec_impact]: This is a nice example of cross-pollination: OCaml originally inspired Rust, and Rust is now influencing OxCaml.

[^fn-unboxed-variants]: This refers to the OxCaml feature set available for the August 2026 parser snapshot.

[^fn-intrinsics-snapshot]: The affected declarations were in the installed `ocaml_intrinsics_kernel` package snapshot; later package updates restored the metadata.

[^fn-oxcaml]: Jane Street, “[OxCaml][oxcaml].”

[^fn-simdjson-paper]: Geoff Langdale and Daniel Lemire, “[Parsing gigabytes of JSON per second][simdjson-paper],” *The VLDB Journal* 28(6), 2019.

[^fn-lemire-number]: Daniel Lemire, “[Number parsing at a gigabyte per second][lemire-number],” *Software: Practice and Experience* 51(8), 2021.

[^fn-simdjson-dom]: Daniel Lemire et al., “[The simdjson DOM API][simdjson-dom].”

[^fn-simdjson-copy-and-find]: simdjson, “[The `copy_and_find` implementation][simdjson-copy-and-find].”

[^fn-ocaml-representation]: OCaml.org, “[Memory representation of OCaml values][ocaml-representation].”

[^fn-simdjson-oxcaml]: Artem Pianykh, “[simdjson-oxcaml][simdjson-oxcaml].”

[oxcaml]: https://oxcaml.org/
[simdjson-paper]: https://doi.org/10.1007/s00778-019-00578-5
[lemire-number]: https://doi.org/10.1002/spe.2984
[simdjson-dom]: https://simdjson.org/api/1.0.0/md_doc_dom.html
[simdjson-copy-and-find]: https://github.com/simdjson/simdjson/blob/master/include/simdjson/haswell/stringparsing_defs.h#L31-L41
[ocaml-representation]: https://ocaml.org/docs/memory-representation
[httpz]: https://anil.recoil.org/notes/oxcaml-httpz
[simdjson]: https://github.com/simdjson/simdjson
[simdjson-oxcaml]: https://github.com/artempyanykh/simdjson-oxcaml

</div>
