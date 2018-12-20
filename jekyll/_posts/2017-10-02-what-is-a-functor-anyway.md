---
title: What is a Functor anyway?
category: category-theory
layout: page
use_math: true
---

**A functor is a homomorphism of categories.**
Nice and short definition, but it made absolutely no sense to me.
At the time I decided to step up my FP game, I was pretty well versed in Scalaz, confidently used Functor, Monad, Applicative and the like.
So, I jumped into a textbook on Category Theory... and got totally stuck.
Something just hadn't clicked in my mind, I couldn't map the math to Scalaz code, and instead of being enlightened I only got confused.
Then other things came up, and Cats were put on a back-burner.
The issue as I see it now was in the wrong mindset I developed while being exposed only to the code and internet articles.
Recently, I made another attempt in learning Category Theory which is on-going now and to some extent more successful.

## What is a category?
It always helps to go back to basics when feeling confused.
So, let's start with the most basic thing in Cats, namely the definition of a category.

Formally, a category $\mathscr{C}$ consists of:
\\(
    \def\dom{\mathsf{dom}\ }
    \def\cod{\mathsf{cod}\ }
    \def\id{\mathbb{1}}
\\)

1. A collection of objects $C_0 = \\{A, B, C, \ldots\\}$,
2. A collections of arrows[^morphisms] $C_1 = \\{f, g, h, \ldots\\}$.

Each arrow has a domain $\dom f = A \in C_0$ and a codomain $\cod f = B \in C_0$.
This is best visualized with an actual arrow going from $A$ to $B$, that is $A \xrightarrow{f} B$.

For each pair of arrows $f$ and $g$ such that $\cod f = \dom g$ there should exist another arrow $g \circ f$ called *a composition*.
This operation should be associative, that is
\\[
(h \circ g) \circ f = h \circ (g \circ f).
\\]

Another important piece in the definition of a category is the existence of an identity arrow.
Formally, for each object $A \in C_0$ there should be an arrow $\id_A: A \rightarrow A$ such that for every arrow $f: A \rightarrow B$ the following holds
\\[
f \circ \id_A = \id_B \circ f = f.
\\]

From a categorical point of view, objects are not that important, arrow and relationships between them are important.

So far, so good.
Let's make the last step and figure out what category we're dealing with as functional programmers. Apparently it has something to do with functions, and actually, that's pretty much it. Category $\mathscr{L}$[^lang] defined as follows:

* Objects $C_0$ are types of our programming language, like `Int`, `Float` or `Vector[String]`.
* Arrows $C_1$ are functions, e.g.

  ```
  def f(x: String): Int = x.length
  ```

* Composition $\circ$ is a regular function composition.

Not really a rocket science, but it takes time to sink in.

It helps to consider another example of a category.
I like to call it $\mathscr{F}lower$.
It consists of one artificial object $O$, and four arrows denoted as $0, 1, 2, 3$, with the composition being addition modulo 4.

<img src="{{ "/assets/posts/what-is-a-functor/graphs.svg" | prepend: site.baseurl }}" width="200" alt="Flower category">

A simple exercise for a reader would be to prove that:

1. the composition of arrows is associative,
2. arrow $0$ is an identity arrow.

Just by looking at $\mathscr{L}$ and $\mathscr{F}lower$ it is easy to see that the notion of a _category_ captures a very broad range of subjects.
We as functional programmers are interested in just one category, however, and that is $\mathscr{L}$.

## What is a functor?

In short, functor $F$ is a _structure preserving_ map from category $\mathscr{C} = \\{C_0, C_1, \circ_\mathscr{C}\\}$ to category $\mathscr{D} = \\{D_0, D_1, \circ_\mathscr{D}\\}$.
Since categories have objects **and** arrows we actually need two maps here:

* $F_0: C_0 \rightarrow D_0$ which maps objects to objects, and
* $F_1: C_1 \rightarrow D_1$ which maps arrows to arrows,

We also need laws that capture the essence of being a _structure preserving_ map:

1. Functor should preserve domain and codomain:
   \\[
   \forall f: A \rightarrow B \in C_1 \implies F_1(f): F_0(A) \rightarrow F_0(B).
   \\]
2. Functor should preserve composition:
   \\[
   \forall f, g \in C_1 \text{where } \cod f = \dom g: F_1 (g \circ_\mathscr{C} f) = F_1(f) \circ_\mathscr{D} F_1(g).
   \\]
3. Functor should preserve identity arrow:
   \\[
   \forall A \in C_0: F_1(\id_A) = \id_{F_0(A)}.
   \\]

This may seem abstract, but not crazy complicated, and it actually starts making sense if you give it enough thinking.

## How do we translate this to Scala?

First, we need to understand that the only category we're dealing with is $\mathscr{L}$.
So, every functor $F$ will be from $\mathscr{L}$ to $\mathscr{L}$.
Such functors are called _endofunctors_ [^endo].

Let's translate the definition from the previous section into Scala code.
There are two parts:

1. Mapping from any type to some other type. This is $F_0$ or "objects to objects".
2. Mapping from any function to some other function. This is $F_1$ or "arrows to arrows".

In the code that looks like this:

```scala
trait Functor {
  type F0[A]
  def F1[A, B](arrow: A => B): F0[A] => F0[B]
}
```

One specific example of a functor would be the following:

```scala
class ListFunctor extends Functor {
  type F0[A] = List[A]
  def F1[A, B](arrow: A => B): F0[A] => F0[B] = fa => fa.map(arrow)
}
```

It's relatively easy to check that functor laws hold for this object.

Consider another example:

```scala
class NotListFunctor extends Functor {
  type F0[A] = List[A]
  def F1[A, B](arrow: A => B): F0[A] => F0[B] = fa => List.empty[B]
}
```

We have all necessary pieces defined, but the resulting object is not a functor because some of the laws don't hold.
Give it a minute and think which law doesn't hold here.

Sometimes when there is a functor defined with `type F0[A] = F[A]`, people say that `F` is a functor.
This is confusing and not true.
`F[_]` is just a type constructor or, in other words, a mapping from any type `A` to some other type.
A functor is a thing on its own, and specific `F` is just a part of its definition.

An interesting question is whether there is a single functor with `type F0[A] = List[A]`.
The answer is _yes_, and it actually holds true for pretty much every parametric data type.
Fascinating!

One very different example of a functor would be the following:

```scala
class IntFunctor extends Functor {
  type F0[A] = Int
  def F1[A, B](f: A => B): F0[A] => F0[B] = x => x
}
```

We don't have a parametric data type here.
Instead, we map every type to `Int`.
It's a good exercise to show that this actually is a functor.

Enough with examples.
Let's compare the definition of a functor above with a simplified version in Scalaz:

```scala
trait ScalazFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

It's close, but not exactly the same.
I think that we end up with `ScalazFunctor` instead of `Functor` for a couple reasons.
First, if you want to use a functor as an implicit parameter in a function, it's more convenient to have `F0` as a type parameter of a `Functor`.
After fixing that we have:

```scala
trait Functor[F0[_]] {
  def F1[A, B](arrow: A => B): F0[A] => F0[B]
}
```

Next issue is with type inference --- we need to specify type arguments on every `F1` invocation.
If we make `F0[A]` the first argument to `F1[A, B]` then the compiler will usually figure out `A` and `B` on its own.
After this change we arrive at:

```scala
trait Functor[F0[_]] {
  def F1[A, B](f0a: F0[A])(arrow: A => B): F0[B]
}
```

This perfectly matches `ScalazFunctor`, the definitions are completely equivalent and `ScalazFunctor` is actually more convenient to use.
The downside of the latter definition, in my opinion, is that it hides _objects to objects_ part of a functor definition, and makes relating math and code harder.

## Conclusion
At this point, I'm pretty happy with my understanding of basic categorical notions and how to map them to the code.
There are a couple more things that require a deeper dive in, for instance, the notion of _structure preserving_ actually has a precise definition, although it was used rather loosely in this post.
Also, the proof of uniqueness of `ListFunctor` and other similar functors is still an open question to me.

---
---

[^morphisms]: Arrows are also called morphisms.
[^lang]: $\mathscr{L}$ from "**L**anguage". I'm deliberately avoiding possible complications related to the presence of side-effects and such.
[^endo]: _Endo_ stands for "internal/within".
