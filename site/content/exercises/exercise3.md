---
title: List manipulation and sorting
weight: 3
katex: true
---

# More list manipulation

We'll start off doing a bit more list manipulation, looking at some
list comprehensions and pattern matching. Then we'll implement [merge
sort](https://en.wikipedia.org/wiki/Merge_sort).

## List comprehensions & pattern matching

For this section, the template code is [`code/lists-exercise3.hs`]({{<
code-ref "lists-exercise3.hs" >}}).

Let's first look at some pattern matching, and combination with guard
expressions.

{{< exercise >}}

Write a function `compress :: Eq a => [a] -> [a]` that eliminates
consecutive duplicate elements of a list, but otherwise leaves the
order unchanged.

{{< /exercise >}}

Now some simple list comprehensions. A [pythagorean
triple](https://en.wikipedia.org/wiki/Pythagorean_triple) is a tuple
$(x, y, z)$ of positive integers where $x^2 + y^2 = z^2$. For our
first go, we won't care about ordering, so we'll allow generation of
both $(3, 4, 5)$ and $(4, 3, 5)$ (for example).

{{< exercise >}}
Using a list comprehension, define `pyths :: Int -> [(Int, Int, Int)]`
that generates all pythogorean truples with components less than or
equal to the specified integer.

For example
```
Prelude> pyths 5
[(3, 4, 5), (4, 3, 5)]
```

{{< hint info >}}
Note that integer exponentiation in Haskell is written `x^y`.
{{< /hint >}}
{{< /exercise >}}

{{< exercise >}}
Now modify your answer to only produce unique triples (don't worry
about ordering), so for example

```
Prelude> pyths' 5
[(3, 4, 5)]
```

{{< hint info >}}
Recall that when introducing variables in a list comprehension, later
generators can refer to the variables introduced by earlier ones.
{{< /hint >}}
{{< /exercise >}}

{{< question >}}
The power two is special here, indeed [Fermat's Last
Theorem](https://en.wikipedia.org/wiki/Fermat%27s_Last_Theorem),
proved by Andrew Wiles in 1994, states that the equation $x^n + y^n =
z^n$ has no solutions for positive integers $x, y, z, n$ when $n > 2$.

You could try and confirm this for $n = 3$ by checking that the list
generated by a comprehension with $x^3 + y^3 = z^3$ is empty.

Would you be able to use this to prove the theorem? If not, why not?

{{< /question >}}

### More than one way to do it

Unlike Python for which the [zen of
python](https://www.python.org/dev/peps/pep-0020/) says that

> There should be one -- and preferably only one -- obvious way to do
> it.

In Haskell there are often multiple different ways to do the same
thing, each of which may be more or less obvious depending on what
you're used to.

Let's look at this by definining a function to compute the scalar
product of two vectors (represented as lists) of numbers of length
$n$. Recall that the scalar (or dot)
[product](https://en.wikipedia.org/wiki/Dot_product) is defined as

$$
a \cdot b = \sum_{i=1}^{N} a_i b_i
$$

{{< exercise >}}

Define a function to compute the scalar product in three different
ways

1. Using `sum` and a list comprehension
2. Using `sum`, `map`, and `zip`.
3. Using `sum` and `zipWith`

The template code has a few more explanatory comments.

You may assume that both vectors have the same length.

{{< /exercise >}}

{{< question >}}
Which do you prefer, and why?
{{< /question >}}

### Solutions

I've added some [commented solutions]({{< code-ref
"exercises/exercise3-solutions.hs" >}}) to these exercises. If you
have queries about them please ask in the practical sessions or else
[get in touch]({{< ref "/#discussion-forum" >}}).

## Merge sort

Now we're going to implement sorting of lists using the [merge
sort](https://en.wikipedia.org/wiki/Merge_sort) algorithm. Merge sort
is a divide-and-conquer algorithm that is built out of three parts.

1. Dividing a list into two sublists that are to be sorted.
2. Sorting the sublists
3. Merging two sublists that are already sorted

This has a very natural recursive definition and a succint Haskell
implementation.

We'll break this into parts. The template code for this exercise is in
[`code/lists-mergesort.hs`]({{< code-ref "lists-mergesort.hs" >}})

First, define a function

```hs
halve :: [a] -> ([a], [a])
```

which splits a list into two halves at its midpoint (for odd-length
lists the two sublists should have lengths that differ by at most
one). You might find [`splitAt :: Int -> [a] -> ([a],
[a])`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:splitAt)
helpful.

Next define a function

```hs
merge :: Ord a => [a] -> [a] -> [a]
```
which takes two (sorted) lists and merges them into one sorted list.
This is probably easiest to write recursively, think about the
possible cases.

Finally use your two helper functions to write

```hs
mergeSort :: Ord a => [a] -> [a]
```
Again, the recursive definition is a natural one. There are base cases
for empty and singleton lists, while the recursive case should
implement the divide and conquer, splitting and merging sorted sublists.

### A higher-order version

Notice how this implementation of `mergeSort` requires that the list
entries are
[_orderable_](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ord.html#t:Ord).
It is often useful to instead allow the caller to provide a comparison
function that orders elements.

Rework your code to implement

```hs
mergeSortWith :: (a -> a -> Ordering) -> [a] -> [a]
```

Where the
[`Ordering`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ord.html#t:Ordering)
type has three values `LT`, `EQ`, or `GT`.

You'll need to implement new `mergeWith` and `mergeSortWith` functions.
With `mergeSortWith` implemented, the implementation of `mergeSort` is
then just

```hs
mergeSort' :: Ord a => [a] -> [a]
mergeSort' = mergeSortWith compare
```

Where we used the generic function
[compare](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ord.html#v:compare).

{{< question >}}
Do you understand how the implementation of `mergeSort'` works using
currying and partial application?
{{< /question >}}

### Composition

We can use this idea of higher-order functions and composition as
building blocks for a number of different ways of sorting objects.

To simplify spelling things, let's introduce a type-synonym for the
type of the comparison function

```hs
type Comparator a = a -> a -> Ordering
```

We can now do various things, suppose that we want to sort lists in a
reverse order from the one given by the default ordering. We could do
```hs
sortReverse :: Ord a => [a] -> [a]
sortReverse = reverse . mergeSortWith compare
```
But this unnecessarily traverses the list to reverse it at the end.

Better is to invert the comparison function. I provide an
`invertOrdering` function that reverses an ordering.

{{< exercise >}}

Implement

```hs
invert :: Comparator a -> Comparator a
```

Which takes a comparator and produces a new comparator that delivers
the reverse. We can then implement `sortReverse` without the extra
traversal

```hs
sortReverse :: Ord a => [a] -> [a]
sortReverse xs = mergeSortWith (invert compare) xs
```

{{< hint info >}}

The `invert` function _returns_ a function, so we need
a way to introduce names for its arguments (they don't appear on the
left hand side of the definition).

The easiest way to do this is by using a lambda expression on the
right hand side. If you want to then see how write it in
[pointfree](https://wiki.haskell.org/Pointfree) style, go to
[pointfree.io](http://pointfree.io).

{{< /hint >}}

{{< /exercise >}}

Now let's look at slightly more involved comparator transformations.

Suppose we have a `Comparator a` and a way of turning `b`s into `a`s,
we can use this to deliver a `Comparator b`. We'll call this function
`on` for reasons which will become (hopefully) obvious.

```hs
on :: Comparator a -> (b -> a) -> Comparator b
on = undefined
```

For example, suppose we want to compare tuples by their first element
using the builtin `compare` on that element.

Our comparison operation would be

```hs
on compare fst
```

Now we can see why we called this function `on`, since when we write
it with infix notation

```hs
compare `on` fst
```

We read this as "Compare the elements on their first part". As an
example, if we run

```
Prelude> mergeSortWith (compare `on` fst`) [(7, "a"), (-1, "b"), (5, "d")]
[(-1, "b"), (5, "d"), (7, "a")]
```

{{< exercise >}}
Implement `on`. Check that it produces the right answer.

{{< hint info >}}

Like with `invert`, the `on` function _returns_ a function, so we need
a way to introduce names for its arguments (they don't appear on the
left hand side of the definition).

The easiest way to do this is by using a lambda expression on the
right hand side. If you want to then see how write it in
[pointfree](https://wiki.haskell.org/Pointfree) style, go to
[pointfree.io](http://pointfree.io).

{{< /hint >}}
{{< hint info >}}
If the transformation function you provide is the identity function
`id`, then you should get back the same thing as if you had not used
`on`. This is a useful sanity check.

That is

```hs
compare `on` id == compare
```
{{< /hint >}}
{{< /exercise >}}

{{< exercise >}}
Finally, we'll compose these two extra functions. Write a function 

```hs
sortReversedByLengthSnd :: [(a, String)] -> [(a, String)]
```

That sorts a list of pairs of something and strings in reverse order
by the length of the string.

For example

```
Prelude> sortReversedByLengthSnd [(5, "a"), (2, "foo"), (3, "four"), (7, "ab")]
[(3, "four"), (2, "foo"), (7, "ab"), (5, "a")]
```

You should not write this comparison function by hand, but just
compose together functions which you already have.
{{< /exercise >}}

{{< solution release=true >}}
## Solutions

I've added some [commented solutions]({{< code-ref
"exercises/mergesort-solutions.hs" >}}) to these exercises. If you
have queries about them please ask in the practical sessions or else
[get in touch]({{< ref "/#discussion-forum" >}}).

{{< /solution >}}
