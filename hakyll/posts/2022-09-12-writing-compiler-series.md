---
title: Writing a compiler in F# (video series)
category: compilers, fsharp
---

In this 5 part series I build a compiler for a small **f**unctional l**ang**uage called Fang. The
series is focused on implementing a compiler backend for a functional language which is a variation
of the untyped λ-calculus. Each video demostrates a complete implementation of a particular part of
the compiler: from the *abstract syntax tree* to different *evaluation* strategies, to the
*bytecode* and the *virtual machine*.

<!--more-->

Each video has very detailed timestamps, so feel free to skip to the part that you may be interested
in the most. The source code of the compiler is available [on the GitHub][fang-repo].

## Part 1. AST, term reduction via substituion

In this video we define the language's AST and implement a tree-walking interpreter that does term
reduction by substitution. We also explore ways to encode recursion via a fixpoint combinator as
well as lazy and eager evaluation strategies.

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/qRHJ4qcFbNE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Part 2. Environment, closures, let-bindings

In this video we improve the tree-walking interpreter's performance 10x using environments and
closures instead of explicit term substitution. Furthermore, we add support for recursive
let-bindings which allow direct encoding of recursive functions without the use of the fix-point
operator.

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/STzgckXYpjw" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Part 3. Bytecode basics

Now the time has come to work on the bytecode. Compared to a tree-walking interpreter, the setup for
the bytecode compilation is more involved, so the actual implementation is split into several
videos.

In this video we'll lay the groundwork: define types and basic components required to generate and
execute Fang bytecode.

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/ZID0IJiOJdE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Part 4. Bytecode and let-bindings

In this video we'll extend the Fang compiler by adding support for let-bound variables to the codegen and the VM.

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/cjv4Qrf2rsA" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

## Part 5. Bytecode finale

In this video we complete the bytecode compiler for Fang by adding support for lambdas, closures,
function application, and conditionals. This is the most interesting and the most complicated part
of the bytecode compiler.

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/5mvQOScXDl0" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

[fang-repo]: https://github.com/artempyanykh/Fang
