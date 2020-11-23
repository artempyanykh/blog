---
title: System calls at the assembly level
category: linux, asm
---

We're going to take a quick look at the system calls (in a Linux-centric way) and answer the following questions: 

- what syscalls are, 
- motivation behind syscalls, 
- slow and fast syscall mechanisms. 

We'll be using assembly to see what's happening at the lowest level and will browse Linux kernel source code to find answers to some important questions we'll have along the way.

<!--more-->

<div class="video-container">
<iframe class="video" src="https://www.youtube.com/embed/UW7a1n3VEFg" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</div>

---

Source code for the lab: [link to Github](https://github.com/artempyanykh/blog/tree/master/lab/syscall).