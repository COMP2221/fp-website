---
title: Slides & Recordings
draft: false
weight: 2
---

# Lecture slides and video links

As the course progresses, I'll add the annotated slides and live code
examples, along with links to the videos (accessible with a Durham
account) to this page.

- 2023-01-09: [Slides]({{< static-ref
  "slides/2022-23/Lec01.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=cdb1e9c6-f338-4f60-acdb-af820094eda4), [code]({{< code-ref "lectures/2022-23/Lec01.hs" >}})
  
  We got most of the way through the slides, and then did some live
  examples. We defined and composed some very simple functions and saw a
  bit of Haskell syntax that we will consider in more detail as the course
  progresses.
  
  Please setup your Haskell programming environment (GHC + an editor/IDE of your choice), try some of the examples from the lecture and try to work your way through the first exercise before the practical sessions start next week.
  
- 2023-01-13: [Slides]({{< static-ref
  "slides/2022-23/Lec02.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ec73c077-ce8f-4ea2-ac90-af8800948d14), [code]({{< code-ref "lectures/2022-23/Lec02.hs" >}})
  
  We got most of the way through the slides, considered basic types, tuples and lists in Haskell. Further, we saw hout to define functions with multiple arguments by packaging all arguments into a tuple or list.
  
- 2023-01-16: [Slides]({{< static-ref
  "slides/2022-23/Lec03.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=55994d27-a6ed-4ca9-bd55-af890094d961), [code]({{< code-ref "lectures/2022-23/Lec03.hs" >}})
  
  We considered currying as an alternative to defining n-ary functions via tuples or lists, we compared curried and uncurried versions of the same functions during live coding. Further, we introduced lambda expressions and the infix notation.
  
- 2023-01-20: [Slides]({{< static-ref
  "slides/2022-23/Lec04.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=1e4a90d5-39cb-4b4c-b8c7-af8f009469af), [code]({{< code-ref "lectures/2022-23/Lec04.hs" >}})
  
  We discussed list comprehensions in conjunction with generators and guard expressions. Further, we considered pattern matching and its limitations. We introduced the types of polymorphism and will continue there next time.
  
- 2023-01-23: [Slides]({{< static-ref
  "slides/2022-23/Lec05.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c9625930-9354-438c-ad5f-af9000966d6b)
  
  We continued with polymorphism and compared Haskell's parameteric polymorphism to the types of polymorphism in other languages. Further, we considered the declaration and definition of custom data types, and how they relate to pattern matching and recursion. We did not do any live coding this time around since the slides are quite code heavy already.
  
- 2023-01-27: [Slides]({{< static-ref
  "slides/2022-23/Lec06.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=bc1539da-4907-4331-ac48-af960094809d)
  
  We considered algebraic data types and their pros and cons in comparison to classes in object-oriented languages. We learned a strcutural approach to write recursive functions and discussed different types of recursion (linear vs. multi, direct vs. indirect, tail vs. non-tail).
  
- 2023-01-30: [Slides]({{< static-ref
  "slides/2022-23/Lec07.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=eaa9852d-6e86-43df-8ca1-af90009579dc)
  We started out by discussing the complexity of two recursive versions of the reverse function, and saw how a tail-recursive approach can lead to a more efficient implementation. Afterwards, we introduced the idea of higher order functions and saw some examples (map, filter, any, all, dropWhile, takeWhile, ...). We learned how to model function compostion in Haskell with the (.) operator. Having recapped the concept and usage of type classes (e.g. Eq, Num, Ord), we discussed how they model generic patterns of computation. We finished by writing Functor instances for custom data types and by considering the functor laws.
  
- 2023-02-03: [Slides]({{< static-ref
  "slides/2022-23/Lec08.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=449a094b-3bc5-4f1a-adfb-af9d00a4d305)
  
  We considered how Haskell evaluates expressions by means of lazy evaluation and expression graphs. We learned that lazy evaluation enables working with infinte data structures and made a start to yet another family of higher order functions: folds. 
  
- 2023-02-06: [Slides]({{< static-ref
  "slides/2022-23/Lec09v2.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=34e22c72-8b21-4bd1-ad9b-af9e00a4d25d),[notes]({{< static-ref "slides/2022-23/beta_reduction_examples.jpg" >}})
  
  We continued with the idea of folds, and saw how `foldr` and `foldl` are implemented recursively. Thanks to you spotting a mistake in the evaluation order of the folding example (Slide 4), I just updated the slides based on the definitions of `foldr` and `foldl` used by GHC. We learned that folds model the reduction principle and that the `Foldable` type class provides a generalization over this principle. Further, we introduced the syntax of the lambda calculus and saw how lambda expression can be evaluated by means of beta reductions.
  
  - 2023-02-10: [Slides]({{< static-ref
  "slides/2022-23/Lec10.pdf" >}}),
  [video](https://durham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a1241441-a2c2-4554-b4a7-afa4009594d2)
  
  We did a recap of the topics we covered in the course, mainly, types, functions, lists, recursion, evaluation strategies and the lambda calculus. Further, I briefly outlined the nature of the summer exam and provided an overview of topics from past exams that moght be relevant for you. In general, you can find past CS exams [here](https://durhamuniversity.sharepoint.com/teams/exampapers/Computer%20Science/Forms/AllItems.aspx).



