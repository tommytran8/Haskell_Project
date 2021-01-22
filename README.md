# Haskell_Project

This is a class project were I used Haskell to create a basic language
that does lambda calculus computation and operator operations.

This project also highlights fundamentals like polymorphism and inheritance.

### Overview

The overall objective of this project is to get some experience 
with different *type-classes*, in particular to learn about:

* Property-based testing, and
* Monads.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

# Features

## Sets via Binary Search Trees

Use Haskell's data types to
implement an _Abstract Set_ datatype that implements 
the following API:

```haskell
-- | The Set data type
data Set a

-- | `contains x ys` returns `True` if and only if `x` is a member of the set `ys`
contains :: (Ord a) => a -> Set a -> Bool

-- | `add x xs` returns the (new) set obtained by inserting `x` into the set `xs`
add :: (Ord a) => a -> Set a -> Set a

-- | `remove x xs` returns the (new) set obtained by deleting `x` from the set `xs`
remove :: (Ord a) => a -> Set a -> Set a
```

### Data Type

The sets will be represented as *Binary Search Trees* that support
efficient addition and removal of elements. We represent the 
datatype as:

```haskell
data BST a
  = Leaf                      -- ^ empty tree
  | Node a (BST a) (BST a)    -- ^ node with left and right subtrees
  deriving (Show)
```

Thus, values of type `BST a` are of the form

- `Leaf` or
- `Node e l r` where e is an element of type `a` and `l` and `r` are left and right subtrees of type `BST a`.

### The `isOrdered` Invariant

We will only use trees that are **Binary-Search Ordered**,
which is to say, where the element in each node is greater than any element in the left subtree
and smaller than any element in the right subtree.
We have provided a Boolean function `isOrdered t`
that evaluates to `True` iff `t` is binary-search ordered. 
Make sure you understand what the `isOrdered` function is doing!

### Build

The definition of `build` that converts the input `[a]`
into a `BST a` by recursively splitting the list into sub-lists,
similarly to [QuickSort](https://nadia-polikarpova.github.io/cse130-web/lectures/02-haskell.html)
converting the sub-lists to trees. Make sure that the resulting
`BST a` is binary-search-ordered. 

```haskell
λ> build [5,10,20,30]
Node 5 Leaf (Node 10 Leaf (Node 20 Leaf (Node 30 Leaf Leaf)))

λ> build [20,10,30,5]
Node 20 (Node 10 (Node 5 Leaf Leaf) Leaf) (Node 30 Leaf Leaf)
```

### Property-based testing

Writing tests by hand is tedious! 
Instead we will write a *property* called `prop_build` 
that checks that a tree generated with `build` is ordered.
A property is just a Boolean function:
```haskell
prop_build :: [Int] -> Bool
prop_build xs = isOrdered (build xs)
```

Now we will use a *property-based testing* tool called 
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
to test our property on a bunch of random inputs:

```haskell
λ> quickCheck prop_build
+++ OK, passed 100 tests.
```

Make sure that you get this result.
Otherwise, debug your `build` function.

### Contains


```haskell
contains :: (Ord a) => a -> BST a -> Bool
```

`contains x t` evaluates to `True` iff the element `x`
is in the tree `t`. When you are done, you should see the
following behavior:

```haskell
λ> t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> [ contains x t2 | x <- [5,6,10,11,20,21,30,31] ]
[True,False,True,False,True,False,True,False]
```

The property `prop_contains_elt` 
states that a tree built from a list `xs`
`contain`s any  `x` that is in `xs`:

```haskell
λ> quickCheck prop_contains_elt
+++ OK, passed 100 tests.
```

###  Fold

A `fold` function that performs an *in-order traversal* of the tree.

```haskell
fold :: (b -> a -> b) -> b -> BST a -> b
```
Why is there no `Ord` in the type for `fold`?

By *in-order* we mean that `fold (+) 0 t` would execute in the folloiwng order:

```haskell
-- Tree `t` looks like this:
--   5
--  / \
-- 1   20
--    /  \
--   10  30

-- `fold (+) 0 t` executes like this:
((((0 + 1) + 5) + 10) + 20) + 30
```

Bonus properties get unlocked thanks
to the `toList` function that we have supplied that uses `fold`.


```haskell
λ> toList t2
[5,10,20,30]

λ> toString t2
"build [5,10,20,30]"
```

Tests.

```haskell
λ> quickCheck prop_contains_elts
+++ OK, passed 100 tests.
```


###  Add

```haskell
add :: (Ord a) => a -> BST a -> BST a
``` 

such that `add x t` returns a (new) BST which
has the same elements as `t` *plus* the new
element `x`. Of course, the new tree should
satisfy the binary-search-ordering invariant.


```haskell
λ> t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> add 12 t2
Node 5 Leaf (Node 20 (Node 10 Leaf (Node 12 Leaf Leaf)) (Node 30 Leaf Leaf))

λ> let tnew = add 12 t2

λ> isOrdered tnew
True

λ> contains 12 tnew
True
```


```haskell
λ> quickCheck prop_add_elt
+++ OK, passed 100 tests.

λ> quickCheck prop_add_elts_old
+++ OK, passed 100 tests.

λ> quickCheck prop_add_isOrd
+++ OK, passed 100 tests.
```


### Remove Minimum


```haskell
removeMin :: (Ord a) => BST a -> (a, BST a)
```

that returns a tuple of the _minimum element_ in the tree,
and the tree containing all elements _except_ the minimum.
When you are done you should see this behavior

```haskell
λ> removeMin t2
(5,Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> quickCheck prop_remove_min
+++ OK, passed 100 tests.
```

`removeMin` should throw an error if given an empty tree.

### Remove

Use `removeMin` to fill in the definition of

```haskell
remove :: (Ord a) => a -> BST a -> BST a
```

such that `remove x t` returns the tree containing all
elements _except_ x. Of course, the new set should
satisfy the binary-search-ordering property.

`remove x t` should return `t` unchanged if `x` is not
in `t`.


```haskell
λ> remove 12 t2
Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))

λ> remove 20 t2
Node 5 Leaf (Node 30 (Node 10 Leaf Leaf) Leaf)

λ> quickCheck prop_remove
+++ OK, passed 100 tests.

λ> quickCheck prop_remove_old
+++ OK, passed 100 tests.

λ> quickCheck prop_remove_isOrd
+++ OK, passed 100 tests.
```

## Exceptions

### Exceptional Expressions

We have extended the representation of _Expressions_ by adding two new constructs:

```haskell
data Expr
  = ...
  | EThr Expr           -- ^ throw e
  | ETry Expr Id Expr   -- ^ try e1 catch z => e2
```

### Exceptional Values

We will represent the _result_ of evaluation via the type:

```haskell
data Either a b = Left a | Right b
```

The key idea is that when we evaluate an expression `e` we get:

1. `Left exn` when `e` "throws" an (uncaught) exception;
2. `Right val` when `e` "finishes" normally, without an exception.


To do so, note that the _top-level_ `eval` function is defined as:

```haskell
eval :: Env -> Expr -> Value
eval env e = case evalE env e of
  Left exn  -> exn
  Right val -> val
```

The helper function `evalE` has the type:

```haskell
evalE :: Env -> Expr -> Either Value Value
```


### Throw


```haskell
evalE env (EThr e)       = error "TBD"
```


```haskell
λ> eval [] (EBin Plus (EInt 1) (EInt 2))
3
λ> eval [] (EBin Plus (EThr (EInt 1)) (EInt 2))
1
λ> eval [] (EBin Plus (EInt 1) (EThr (EInt 2)))
2
λ> eval [] (EBin Plus (EThr (EInt 1)) (EThr (EInt 2)))
1
λ> eval [] (EThr (EBin Plus (EInt 1) (EInt 2)))
3
λ> eval [] (EThr (EBin Plus (EInt 1) (EThr (EInt 2))))
2
```

Remember that `Either` is a monad, and use monad operations to implement this!


###  Catch


```haskell
evalE env (ETry e1 x e2) = error "TBD"
```


```haskell
λ> let tryZ e = ETry e "z" (EBin Plus (EVar "z") (EInt 10))

λ> eval [] (tryZ (EBin Plus (EInt 1) (EInt 2)))
3
λ> eval [] (tryZ (EBin Plus (EThr (EInt 1)) (EInt 2)))
11
λ> eval [] (tryZ (EBin Plus (EInt 1) (EThr (EInt 2))))
12
λ> eval [] (tryZ (EBin Plus (EThr (EInt 1)) (EThr (EInt 2))))
11
λ> eval [] (tryZ (EThr (EBin Plus (EInt 1) (EInt 2))))
13
λ> eval [] (tryZ (EThr (EBin Plus (EInt 1) (EThr (EInt 2)))))
12
```

## Nano REPL

For this problem, you will get some experience building
a _standalone "app"_ in Haskell, by implementing a "shell" 
for `nano`, that will let you:

- `quit` :-)
- `run` a file,
- `eval` an expression passed in as a `String`
- `load` a file and then evaluate expressions 
   that can refer to the top-level definitions of the file.

**This time, we're giving you almost _no_ scaffolding.**
Your task is simply to implement the code in `src/Main.hs`,
specifically, to fill in the implementation of the function

```haskell
main :: IO ()
main = error "TBD:main"
```

which is the top-level `IO` _recipe_ that Haskell runs as an "app".

However, there are lots of _very useful_ functions in

- [Repl.hs](/src/Language/Nano/Repl.hs)

that we suggest you understand (and complete the implementation of where necessary.)

### Quit

First, hook up your shell so that it starts up thus:

```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0]
```

The `[0]` is a "prompt" at which the user can type a command.

Specifically, if the user types `:quit` then the shell should exit!

```sh
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :quit
Goodbye.
```

### Evaluating Expressions

Extends REPL so that the user can (repeatedly)
enter expressions that then get parsed and evaluated, with
the results printed back.

**Don't worry about parsing `throw` and `try-catch` -- just the older constructs.**


```sh
polikarn@lena ~/t/1/a/05-classes (master)> make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] 2 + 3
5
λ [1] let x = 2 in x + 3
5
λ [2] let add x y = x + y in ((add 10) 20)
30
λ [3] quit
unbound variable: quit
λ [4] :quit
Goodbye.
```


### Running Files

Extends REPL so that you can `run` a particular file, that is,

- read the file,
- parse its contents into an `Expr`
- evaluate the expr to get a `Value`.


```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :run tests/input/t1.hs
45
λ [1] :run tests/input/t2.hs
0
λ [2] :run tests/input/t3.hs
2
λ [3] :quit
Goodbye.
```


### Loading Files

Extends REPL so that you can `load` a file's definitions 
into the environment, and then write expressions that refer to those
expressions. For example, consider the file: `tests/input/tAdd.hs`
which has two definitions:

```haskell
add =
  let add1 x y = x + y in
      add1
,

sub = 
  let sub1 x y = add x (0 - y) in
      sub1
```


```sh
$ make repl
...
------------------------------------------------------------
-------- The NANO Interpreter v.0.0.0.0 --------------------
------------------------------------------------------------

λ [0] :load tests/input/tAdd.hs
definitions: tail head add sub
λ [1] ((sub ((add 10) 20)) 5)
25
λ [2] :quit
Goodbye.
```

That is, typing `:load tests/input/tAdd.hs` adds the definitions
for `add` and `sub` into the top-level environment (which already
had `tail` and `head`). 

As with "evaluating expressions" you can then enter the expression

```haskell
((sub ((add 10) 20)) 5)
```

which should get evaluated and the result `25` is printed out.

You can assume that each `load` wipes out all previous definitions,
except those in `prelude`.
