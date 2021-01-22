# Nano-language

This is a class project were I used Haskell to create a basic language
that does lambda calculus computation and operator operations.

This project also highlights fundamentals like polymorphism and inheritance.

### Overview

The overall objective of this project is to fully understand the notions of:

* lexing,
* parsing,
* scoping,
* binding,
* environments and closures,

and get some experience 
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

## Data Structures

Build an interpreter
for a subset of Haskell called *Nano*. The following
data types (in `Types.hs`) are used to represent the
different elements of the language.

### Binary Operators

Nano uses the following **binary** operators encoded
within the interpreter as values of type `Binop`.

```haskell
data Binop
  = Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | And
  | Or
  | Cons
```

### Expressions

All Nano programs correspond to **expressions**
each of which will be represented within your
interpreter by Haskell values of type `Expr`.

```haskell
data Expr
  = EInt  Int
  | EBool Bool
  | ENil
  | EVar Id
  | EBin Binop Expr Expr
  | EIf  Expr Expr  Expr
  | ELet Id   Expr  Expr
  | EApp Expr Expr
  | ELam Id   Expr
  deriving (Eq)
```

where `Id` is just a type alias for `String` used to represent
variable names:

```haskell
type Id = String
```

The following lists some Nano expressions,
and the value of type `Expr` used to represent
the expression inside your interpreter.

1. Let-bindings

```haskell
let x = 3 in x + x
```

is represented by

```haskell
ELet "x" (EInt 3)
  (EBin Plus (EVar "x") (EVar "x"))
```

2. Anonymous Functions definitions

```haskell
\x -> x + 1
```

is represented by

```haskell
ELam "x" (EBin Plus (EVar "x") (EInt 1))
```

3. Function applications ("calls")

```haskell
f x
```

is represented by

```haskell
EApp (EVar "f") (EVar "x")
```

4. (Recursive) Named Functions

```haskell
let f = \ x -> f x in
  f 5
```

is represented by

```haskell
ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x")))
  (EApp (EVar "f") (EInt 5))
```


### Values

```haskell
data Value
  = VInt  Int
  | VBool Bool
  | VClos Env Id Expr
  | VNil
  | VCons Value Value
  | VPrim (Value -> Value)
```

where an `Env` is simply a dictionary: a list of pairs
of variable names and the values they are bound to:

```haskell
type Env = [(Id, Value)]
```

- `VClos env "x" e` represents a function with argument `"x"`
   and body-expression `e` that was defined in an environment
   `env`.
   
## Nano Interpreter

```haskell
data Binop = Plus | Minus | Mul

data Expr  = EInt Int
           | EVar Id
           | EBin Binop Expr Expr

data Value = VInt Int
```

That is,

- An *expression* is either an `Int` constant,
  a variable, or a binary operator applied
  to two sub-expressions.

- A *value* is an integer, and an *environment*
  is a list of pairs of variable names and values.

```haskell
lookupId :: Id -> Env -> Value
```

where `lookupId x env` returns the most recent
binding for the variable `x` (i.e. the first from the left)
in the list representing the environment.
If no such value is found, you should throw an error:

```haskell
throw (Error ("unbound variable: " ++ x))

Should get the following behavior:

```haskell
>>> lookupId "z1" env0
0

>>> lookupId "x" env0
1

>>> lookupId "y" env0
2

>>> lookupId "mickey" env0
*** Exception: Error {errMsg = "unbound variable: mickey"}
```

```haskell
eval :: Env -> Expr -> Value
```

such that `eval env e` evaluates the Nano
expression `e` in the environment `env`
(i.e. uses `env` for the values of the
**free variables** in `e`), and throws
an `Error "unbound variable"` if the
expression contains a free variable
that is **not bound** in `env`.

Should get the following behavior:

```haskell
>>> eval env0 (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1")))
0

>>> eval env0 (EVar "p")
*** Exception: Error {errMsg = "unbound variable: p"}
```

binary operators

```haskell
data Binop = ...
           | Eq | Ne | Lt | Le | And | Or
```

This will require using the new value type `Bool`

```haskell
data Value = ...
           | VBool Bool
```

* The operators `Eq` and `Ne` should work if both operands
  are `VInt` values, or if both operands are `VBool` values.

* The operators `Lt` and `Le` are only defined for `VInt`
  values, and `&&` and `||` are only defined for `VBool`
  values.

* Other pairs of arguments are **invalid**

Should see the following behavior

```haskell
>>> eval []  (EBin Le (EInt 2) (EInt 3))
True

>>> eval []  (EBin Eq (EInt 2) (EInt 3))
False

>>> eval []  (EBin Lt (EInt 2) (EBool True))
*** Exception: Error {errMsg = "type error: binop"}
```

`EIf p t f` expressions.

1. First, evaluate the `p`; if `p` does not evaluate to a
   `VBool` value, then your evaluator should
   `throw (Error "type error")`,

2. If `p` evaluates to the true value then the expression
   `t` should be evaluated and returned as the value of
   the entire `If` expression,

3. Instead, if `p` evaluates to the false value, then `f`
   should be evaluated and that result should be returned.

Should get the following behavior:

```haskell
>>> let e1 = EIf (EBin Lt (EVar "z1") (EVar "x")) (EBin Ne (EVar "y") (EVar "z")) (EBool False)
>>> eval env0 e1
True

>>> let e2 = EIf (EBin Eq (EVar "z1") (EVar "x")) (EBin Le (EVar "y") (EVar "z")) (EBin Le (EVar "z") (EVar "y"))
>>> eval env0 e2
False
```


The *let-in* expressions which introduce local bindings.

```haskell
data Expr
  = ...
  | ELet Id   Expr  Expr
```

The expression `ELet x e1 e2` should be evaluated
as the Haskell expression `let x = e1 in e2`.


Should get the following behavior:

```haskell
>>> let e1 = EBin Plus (EVar "x")  (EVar "y")
>>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
>>> eval [] e2
3
```

Evaluator includes the expressions
corresponding to function definitions and applications.

```haskell
data Expr
  = ...
	| ELam Id   Expr
	| EApp Expr Expr
```

In the above,

* `ELam x e` corresponds to the function defined `\x -> e`, and

* `EApp e1 e2` corresponds to the Haskell expression `e1 e2`
   (i.e. applying the argument `e2` to the function `e1`).
   
```haskell
data Value
  = ...
	| VClos Env Id Expr
```

Functions do have values represented by
the `VClos env x e` where

* `env` is the environment at the point where
   that function was declared,
* `x` is the formal parameter, and
* `e` the body expression of the function.

Eval has the function:

```haskell
>>> eval [] (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EInt 3))
6

>>> let e3 = ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) (EApp (EVar "f") (EVar "h"))
>>> let e2 = ELet "x" (EInt 100) e3
>>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp (EVar "g") (EInt 2)))) e2
>>> eval [] e1
102
```


```haskell
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul (EVar "n") (EApp (EVar "fac") (EBin Minus (EVar "n") (EInt 1))))))
--             (EApp (EVar "fac") (EInt 10)))
-- :}
-- 3628800
```

Program support operations on lists.

```haskell
data Binop = ...
           | Cons

data Expr = ...
          | ENil

data Value = ...
           | VNil
           | VCons Value Value
```

```haskell
>>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)

>>> execExpr el
(1 : (2 : []))

>>> execExpr (EApp (EVar "head") el)
1

>>> execExpr (EApp (EVar "tail") el)
(2 : [])
```


## Nano Lexer (Lexer.x) and Parser (Parser.y)

```haskell
>>> parseTokens "True"
Right [TRUE (AlexPn 0 1 1)]

>>> parseTokens "True False 12345 foo bar baz"
Right [TRUE (AlexPn 0 1 1),FALSE (AlexPn 5 1 6),NUM (AlexPn 11 1 12) 12345,ID (AlexPn 17 1 18) "foo",ID (AlexPn 21 1 22) "bar",ID (AlexPn 25 1 26) "baz"]
```

The `AlexPn n l c` denote the **position**
in the string where the token was parsed.
For example, the `FALSE` is at character
`5`, line `1` and column `6`.

```haskell
>>> parse "True"
EBool True

>>> parse "False"
EBool False

>>> parse "123"
EInt 123

>>> parse "foo"
EVar "foo"
```

Tokens to the lexer and parser.

| String  | Token   |
|:--------|:--------|
| `let`   | `LET`   |
| `=`     | `EQB`   |
| `\`     | `LAM`   |
| `->`    | `ARROW` |
| `if`    | `IF`    |
| `then`  | `THEN`  |
| `else`  | `ELSE`  |


```haskell
>>> parseTokens "let foo = \\x -> if y then z else w in foo"

Right [LET (AlexPn 0 1 1),ID (AlexPn 4 1 5) "foo",EQB (AlexPn 8 1 9),
       LAM (AlexPn 10 1 11),ID (AlexPn 11 1 12) "x",ARROW (AlexPn 13 1 14),
       IF (AlexPn 16 1 17),ID (AlexPn 19 1 20) "y",THEN (AlexPn 21 1 22),
       ID (AlexPn 26 1 27) "z",ELSE (AlexPn 28 1 29),ID (AlexPn 33 1 34) "w",
       IN (AlexPn 35 1 36),ID (AlexPn 38 1 39) "foo"]

>>> parse "let foo = \\x -> if y then z else w in foo"
ELet "foo" (ELam "x" (EIf (EVar "y") (EVar "z") (EVar "w"))) (EVar "foo")

>>> parse "let foo x = if y then z else w in foo"
ELet "foo" (ELam "x" (EIf (EVar "y") (EVar "z") (EVar "w"))) (EVar "foo")
```

Tokens to the lexer and parser.

| String  | Token   |
|:--------|:--------|
| `+`     | `PLUS`  |
| `-`     | `MINUS` |
| `*`     | `MUL`   |
| `<`     | `LESS`  |
| `<=`    | `LEQ`   |
| `==`    | `EQL`   |
| `/=`    | `NEQ`   |
| `\|\|`  | `OR`    |

```haskell
>>> parseTokens "+ - * || < <= = && /="
Right [PLUS (AlexPn 0 1 1),MINUS (AlexPn 2 1 3),
       MUL (AlexPn 4 1 5),OR (AlexPn 6 1 7),
       LESS (AlexPn 9 1 10),LEQ (AlexPn 11 1 12),
       EQB (AlexPn 14 1 15),AND (AlexPn 16 1 17),
       NEQ (AlexPn 19 1 20)]

>>> parse "x + y"
EBin Plus (EVar "x") (EVar "y")

>>> parse "if x <= 4 then a || b else a && b"
EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))

>>> parse "if 4 <= z then 1 - z else 4 * z"
EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))

>>> parse "let a = 6 * 2 in a /= 11"
ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
```

```haskell
>>> parseTokens "() (  )"
Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]

>>> parse "f x"
EApp (EVar "f") (EVar "x")

>>> parse "(\\ x -> x + x) (3 * 3)"
EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))

>>> parse "(((add3 (x)) y) z)"
EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")

>>> parse <$> readFile "tests/input/t1.hs"
EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))

>>> parse <$> readFile "tests/input/t2.hs"
ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))

```


**Operators Precedence Order**

+ (Highest) Fun Application
+ `*`
+ `+`, `-`
+ `==`, `/=`, `<`, `<=`
+ `&&`
+ `||`
+ `->`
+ (Lowest) `=`, `if`, `then`, `else`, `in`

**Precedence** Function application having higher precedence than
multiplications, and multiplication higher than addition,
so `"1+f x*3"` should be parsed as if it were
`"1+((f x)*3)"`.

**Associativity** All arithmetic and logical operators, as well as function application are *left associative*,
so `"1-2-3-4"` should be parsed as `"((1-2)-3)-4"`, 
and `"f x y z"` should be parsed as `"((f x) y) z"`.

The following behavior:

```haskell
>>> parse "1-2-3"
EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
>>> parse "1+a&&b||c+d*e-f-g x"
EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
```

Tokens to the lexer


| String | Token   |
|:-------|:--------|
| `[`    | `LBRAC` |
| `]`    | `RBRAC` |

Rules to your parser to support parsing lists.
`"[a,b,c]"` should be parsed as if it were
`"((a):(b):(c):[])"`. The `:` operator should
have higher priority than the comparison
functions (`==`, `<=` etc.), and lower priority
than `+` and `-`.
In addition, `:` should be right associative.  `"[]"`
should be parsed as `ENil`, and `:` should be treated
as any other binary operator.

The following behavior

```haskell
>>> parse "1:3:5:[]"
EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

>>> parse "[1,3,5]"
EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
```

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
