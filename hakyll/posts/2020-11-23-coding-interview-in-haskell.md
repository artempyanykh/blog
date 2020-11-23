---
title: Coding interview in Haskell (FAANG-style). Yay or nay?
category: haskell, career
---

Can Haskell be a practical language choice for passing a FAANG-style coding interview? 
To find this out we're going to solve a typical interview problem in Haskell in 2 different ways: 

- simple with recursion and persistent maps, and
- more advanced with mutable arrays and ST monad.

In both cases we're going to look at how straightforward the translation from the original idea to the code is.

<!--more-->

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/C3_HzCUMaGs" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

--- 

- Coding problem: [link to Hackerrank](https://www.hackerrank.com/challenges/minimum-swaps-2/problem).
- Source code of the solutions: [link to Github](https://github.com/artempyanykh/artempyanykh.github.io/tree/master/lab/coding-in-haskell).