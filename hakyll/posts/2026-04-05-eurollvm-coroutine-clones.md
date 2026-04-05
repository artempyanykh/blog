---
title: "EuroLLVM '26: Attack of the Clones: Speeding Up Coroutine Compilation"
redirect: /talks/eurollvm26.html
---

Compiling coroutines with full debug information shouldn't be dramatically slower than with line tables — but we found CoroSplitPass running over 100x slower, adding minutes to compilation time. The cause traced back to LLVM's function cloning, where processing debug info metadata was O(Module) rather than O(Function). This talk covers the investigation, the upstream patches, and how the fix ended up benefiting all users of the function cloning API.

<!--more-->

[View the slides](/talks/eurollvm26.html)
