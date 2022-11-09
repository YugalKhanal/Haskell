# Types in Programming, types in Haskell


## Introduction

The topic is introduced in the [video "Typing - Introduction"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=3d391440-24b8-4772-a8f0-adab008c58f5) on Canvas.


## Why are types useful?

The purpose of types in programming languages is to **disallow** certain programmes.
Specifically, types ensure that we only feed those arguments to a function that the function was designed to handle.

For instance, consider a function designed to return the double of a natural number. If we were to feed it with a string instead of a natural number by mistake, the programme would not know what to do.
Types allow us to discover such mistakes at compile time - and fix the mistake before shipping the software - instead of at runtime, when it is too late!
In the specific instance above, the compiler could understand, and warn about, the mistake of applying a function designed to act on natural numbers to an input of type `string`.

Most programming languages have a concept of typing, where every valid expression of the language is assigned a type. However, not all programming languages pay attention to these types to the same extent.

### Type inference

Every valid expression in Haskell has a type, and we can ask `ghci` to tell us that type. This is called **type inference**:
```hs
Prelude> :type False
False :: Bool

Prelude> :type True
True :: Bool
```
Here, the double colon `::` should be read as "has type", e.g., "False has type Bool".

We also have **function types** in Haskell:
```hs
Prelude> :type not
not :: Bool -> Bool
```

As in Lean, function application is simply written by juxtaposition in Haskell. When applying an argument to a function of suitable type, the output type is the obvious one:
```hs
Prelude> :type not False
not False :: Bool
```

### Type checking

We can ask `ghci` to **confirm** that a given expression has a given type:
```hs
Prelude> :type False :: Bool
False :: Bool :: Bool

Prelude> :type not False :: Bool
not False :: Bool :: Bool
```
This is called **type checking**. (Compare this to Lean, where `#check ff : bool` checks whether `ff` has the type `bool`.)

----

**Remark on type checking vs type inference**

When writing programmes in Haskell, it is much safer to indicate to Haskell which type we expect an expression to have (and have Haskell confirm that type), than to have Haskell figure out the type of the functions we are writing.
A good programmer first writes down the type of the function they are constructing, before programming the function itself.

----

**Exercise**

Use `ghci` to find out the type of the following expressions:
1. `not (not (not False))`
2. `(True,False)` (see Section 3.4 of Programming in Haskell)
3. `['a', 'b', 'x']` (see Section 3.3 of Programming in Haskell)
4. `(++)` (What is strange about this type? See "polymorphic functions" discussed later.)

Tip: You can use `:t` instead of `:type` for brevity.

**Explanation**: Watch this [video "Type-inference in Haskell using ghci"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=5a13e413-dc73-44ab-976d-adab008c569e) on Canvas.

----

## Type inference before evaluation

Importantly, the type of a Haskell expression is inferred **before** the Haskell expression is evaluated.
If an expression does not have a type, then it is not a valid Haskell expression, and Haskell will not evaluate the expression.
For instance, the expression `if True then 1 else "foo"` does not have a valid type, since `1` is not of the same type as `"foo"`, and hence the expression is rejected.
(Reason for the rejection: in a more complicated situation where `True` is replaced by a more complicated expression of type `Bool`, Haskell would not be able to ensure what the return type is: it could be either an `Integer` or a `[Char]`. This would mean that Haskell could not ensure **type safety**: the property that type errors occur only during compilation, not during evaluation (runtime).)

----
**Exercise**

- Write five ill-typed expressions in Haskell.
- Check their types in `ghci` - what does `ghci` say?
- What happens when you try to evaluate that expression?

**Explanation**: Watch this [video "Ill-typed expressions in Haskell"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=664d2900-c2d3-4ed6-b8da-adab008c574c) on Canvas.

----

## Well-typed programmes can fail

Not every valid (well-typed) Haskell expression evaluates without errors:
```hs
Prelude> :type (!!)
(!!) :: [a] -> Int -> a
Prelude> :type ["foo", "bar"] !! 5
["foo", "bar"] !! 5 :: [Char]
Prelude> ["foo", "bar"] !! 5
"*** Exception: Prelude.!!: index too large
```
Another example is division by zero:
```hs
Prelude> :type 1 `div` 0
1 `div` 0 :: Integral a => a
Prelude> 1 `div` 0
*** Exception: divide by zero
```

----

**Exercise**

Use `ghci` and/or the [online documentation of Hackage: The Haskell Package Repository](https://hackage.haskell.org/) to find out what the function `(!!)` does.

Hint: you can find the documentation for the library for lists [here](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html).

----

## Tour of Haskell types

In this section, we look at some Haskell types, both base types and composite types.
A video accompanying this section is available [here](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=27d77c3a-fca7-4142-bda9-adab008c5cf4).

### Base types

Haskell has various base types:
1. `Bool` - the type of Booleans
2. `Char` - the type of characters
3. `Int` - fixed-precision integers (between -(2^63) and (2^63)-1); be aware of buffer overflows!
4. `Integer` - arbitrary-precision integers
5. `Float` - numbers with decimal point
6. `Double` - similar to floats, but more precision

```hs
Prelude> :type False
False :: Bool
Prelude> :type 'c'
'c' :: Char
Prelude> :type "foo"
"foo" :: [Char]
Prelude> :type 2 :: Int
2 :: Int :: Int
Prelude> :type 2 :: Integer
2 :: Integer :: Integer
Prelude> :type (sqrt 2)
(sqrt 2) :: Floating a => a
Prelude> :type (sqrt 2) :: Float
(sqrt 2) :: Float :: Float
Prelude> :type (sqrt 2) :: Double
(sqrt 2) :: Double :: Double
Prelude> sqrt 2 :: Float
1.4142135
Prelude> sqrt 2 :: Double
1.4142135623730951
```

### Composite types

We can form complicated types from simpler ones:
1. If `a` is a type, then `[a]` is the type of lists of values in `a`.
2. If `a` and `b` are types, then `(a,b)` is the type of pairs of a value in `a` and a value in `b`
3. Generalizing the above point, we also can build triples `(a, b, c)`, quadruples `(a, b, c, d)`, etc.
4. If `a` and `b` are types, then `a -> b` is the type of functions from `a` to `b`.

```hs
Prelude> :type [True, False]
[True, False] :: [Bool]
Prelude> :type []     -- Polymorphic function
[] :: [a]
Prelude> :type [['a', 'b'], ['c']]
[['a', 'b'], ['c']] :: [[Char]]
Prelude> :type "How are you?"
"How are you?" :: [Char]
Prelude> :type (False, 'c')
(False, 'c') :: (Bool, Char)
Prelude> :type not
not :: Bool -> Bool
Prelude> :type (++)
(++) :: [a] -> [a] -> [a] -- Polymorphic function
```

## Curried functions

(See also Chapter 3.6 of "Programming in Haskell")

This section is discussed in a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=60ca35b8-6c4c-46c6-897d-adab008c5c55).
(The video also discusses the exercise at the end of the section.)

Consider the function
```haskell

add :: (Integer, Integer) -> Integer
add (x,y) = x + y
```
Since a function can also return a function, we can instead write this function as follows:
```haskell
add' :: Integer -> (Integer -> Integer)
add' x y = x + y
```
**Notes:**
1. This is the same principle as the fact that proving `A → B → C` is the same as proving `A ∧ B → C` in propositional logic!
2. The parentheses in `Integer -> (Integer -> Integer)` are not necessary; we write `Integer -> Integer -> Integer` instead (as we did in Lean).
3. Similarly, `add' x y` actually means `(add' x) y`; again the parentheses are not needed. (But they are needed when we want to write something like `f (g x)`).

This principle generalizes to functions with more than two arguments:
```haskell
add3 :: Integer -> Integer -> Integer -> Integer
add3 x y z = x + y + z
```
**Exercise:** Insert all the omitted parentheses into this function and its type annotation.

(The solution is given in the [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=60ca35b8-6c4c-46c6-897d-adab008c5c55) for this section.)



## A useful `ghci` command: `:info`

Another very useful command besides `:type` is `:info`:
```hs
Prelude> :info Bool
data Bool = False | True 	-- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Bounded Bool -- Defined in ‘GHC.Enum’
```
The first line gives the definition of the datatype `Bool` and its two values `False` and `True`. The meaning of the following lines starting with `instance` will be explained in the lesson on [type classes](../4_Type_classes_and_instances/typeclasses.md).

```hs
Prelude> :info not
not :: Bool -> Bool 	-- Defined in ‘GHC.Classes’
```

Unfortunately, `ghci` does not currently seem to have a command to show you the **body** of a function, e.g., the implementation of the function `not :: Bool -> Bool`.
For this, you need to look at the source code, e.g., on [Hackage](https://hackage.haskell.org/).


## Exercise
Read Chapters 3.2-3.6 of "Programming in Haskell", and type the examples into `ghci`. Use `:type` and `:info` generously in your explorations.

## Summary
1. Haskell expressions are type-checked before they are evaluated.
2. Only well-typed expressions can be evaluated.
3. Run-time errors can still happen in Haskell, but not errors related to typing.
4. In `ghci`, `:type` (= `:t`) and `:info` (= `:i`) can be used to find out more about types and expressions.



# Polymorphism

## Introduction
Some functions act on elements of different types, independently of the precise shape of the elements. This is known as **polymorphism**.
The topic is introduced in the [video "Polymorphism - Introduction"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=68f0b311-947c-40b0-9dc4-adab008c597d) on Canvas.

## Example: the type family of lists
Let's look at the datatype of lists. We have seen in Section 3.3 of our textbook that lists can be filled with elements of different types:
```
Prelude> :t ['a', 'b', 'c']   -- a list of characters
['a', 'b', 'c'] :: [Char]
Prelude> :t [False, True, True, True]   -- a list of Booleans
[False, True, True, True] :: [Bool]
Prelude> :t ["foo", "bar"]   -- a list of lists of characters, aka, a list of strings
["foo", "bar"] :: [[Char]]
```
For any type `a`, we can form the type `[a]`. Its elements are lists of elements of type `a`.

## How to build lists

There are two ways of creating a list:
1. The empty list is a list, `[]`.
2. To a given list `xs` we can prepend an element `x`, written `x:xs`.


### The empty list

Firstly, for any type `a`, there is the empty list of elements of `a`:
```
Prelude> :t []
[] :: [a]
```
Here, `a` in `[] :: [a]` is a **type variable**, which can be instantiated with any type. For instance, `a` can be set to be `Integer`, the type of integer numbers in GHC:
```
Prelude> :t [] :: [Integer]
[] :: [Integer] :: [Integer]
```
In this example, we did not use type _inference_, but instead type _checking_: we suggest to `ghci` that `[]` should have type `[Integer]`, and ask `ghci` to confirm, which it does.
Similarly, `ghci` is happy to confirm that `[]` has type `[Char]`:
```
Prelude> :t [] :: [Char]
[] :: [Char] :: [Char]
```
Or:
```
Prelude> :t [] :: [[Char]]
[] :: [[Char]] :: [[Char]]
```
Or, for that matter,
```
Prelude> :t [] :: [[[[[[[Char]]]]]]]
[] :: [[[[[[[Char]]]]]]] :: [[[[[[[Char]]]]]]]
```
----

**Exercise**

For each of these examples, what is the type that the variable `a` is instantiated with?

----
**Explanation:** Watch the [video "Polymorphism: empty list  []"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=2c0c0300-4ccf-42c3-95c1-adab008c57ca) on Canvas.

----

### Adding an element to a list

There is also a way to build a new list from a smaller one, by adding a new element at the beginning:
```
Prelude> :t (:)
(:) :: a -> [a] -> [a]
```
The operator `:` is an **infix** operator; that means that it is used **between** its two arguments (similar to the symbol "+" for addition):
```
Prelude> :t False:[True, False]
False:[True, False] :: [Bool]
```
We can evaluate the expression `False:[True, False]`:
```
Prelude> False:[True, False]
[False,True,False]
```

It is **crucial** that in the expression `x:xs`, where `x :: a`, `xs :: [a]`, **for the same `a`**.
The following fails:
```hs
Prelude> 'a':[True, False]

<interactive>:11:6: error:
    • Couldn't match expected type ‘Char’ with actual type ‘Bool’
    • In the expression: True
      In the second argument of ‘(:)’, namely ‘[True, False]’
      In the expression: 'a' : [True, False]

<interactive>:11:12: error:
    • Couldn't match expected type ‘Char’ with actual type ‘Bool’
    • In the expression: False
      In the second argument of ‘(:)’, namely ‘[True, False]’
      In the expression: 'a' : [True, False]
```

----

**Exercise**

Read the error message in the example above, and explain in your own words what it means.

----
**Explanation:** Watch the [video "Polymorphism: consing (:)"](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=d9295418-77e6-4c5b-96d2-adab008c5855) on Canvas.

----

## Polymorphism

A function whose type contains one (or several) variables is called [**polymorphic**](https://en.wikipedia.org/wiki/Polymorphism_(computer_science) "Wikipedia entry on Polymorphism").
The functions `[]` and `(:)` are examples of polymorphic functions.

Here is an example of a function whose type contains **two** type variables:
```hs
Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]
```

## Exercises:
1. Explain, in your own words, what the function `zip` does. In the expression `zip ['x', 'y'] [False]`, what are the type variables `a` and `b` of `zip :: [a] -> [b] -> [(a, b)]` instantiated by?
2. Find out the types of the following functions. Decide if they are polymorphic.
    1. `fst`
    2. `(++)`
    3. `not`
    4. `head`
    5. `tail`
    6. `id`
3. Find a polymorphic function in the GHC standard library whose type contains 3 type variables or more.
4. Read Section 3.7 of Programming in Haskell. Compare the types of the examples given there with the types `ghci` indicates. (Note: some of the types that `ghci` shows use "type classes" - you will learn about these in the next lesson.)


## Summary:
1. A **polymorphic** function is a function whose type contains **type variables** (which are typically called `a`, `b`, etc.).
2. When applying a polymorphic function to an input, the type variables are suitably **instantiated**, e.g., for `(++) : [a] -> [a] -> [a]` applied to `[True, False]` and `[False, False]`, Haskell instantiates `a` to `Bool`.

# Type classes and instances


* See the [prelude for the current version of the language](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) for all predefined classes and their instances.


## Introduction

**Video:** This introduction is also available as a [recording](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=0495c024-653b-48da-87df-ac3d0154eaf9).

We frequently want to check if two values in Haskell are equal.

**Observation:** We only want to compare two values **of the same type** for equality.
It does not make any sense to ask whether, e.g., a Boolean is equal to a character.

We would hence be tempted to write a **polymorphic function**
```hs
(==) : a -> a -> Bool
```
However, it is not always possible to decide if two values of a given type are equal.

**Exercise:** Find a type `a` whose values cannot be compared for equality with a Boolean function `a -> a -> Bool`.

(One such type is given in the video.)


The solution is given by **type classes**:
1. A **type class** is an interface for a set of operations on one (or more) types.
2. An **instance** of a type class is any type for which we have implemented the interface.

In Haskell, the operation `(==)` has the following type:
```hs
Prelude> :type (==)
(==) :: Eq a => a -> a -> Bool
```
Here,
1. `Eq` is a **type class**, and
2. for any type `a` that is an **instance** of the type class `Eq`, `(==)` is a function of type `a -> a -> Bool` - but only for such `a`.
3. `Eq a` in `Eq a => a -> a -> Bool` is a **class constraint**:

----

**Exercise**

1. Run, and understand, the following examples:
    1. `False == 'c'`
    2. `False == True`
    3. `False == not`
    4. `False == not True`
    5. `not == id`
    6. `[not] == [ (id :: Bool -> Bool) ]`

**Explanation:** See the [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=d808dc3f-e37b-4e3a-8197-ac3d015b9ced).

For the example 6., Haskell understands that it should look for an instance of `Eq [Bool -> Bool]`. Since there is a generic instance `Eq a => Eq [a]`, Haskell proceeds to look for an instance of `Eq (Bool -> Bool)`. Alas, there is no such instance, as we know from example 5.

2. Explain, in your own words, why `(++)` does not require any class constraints, but `(==)` does.
----

## The type class `Eq`

**Video:** See [here](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=02d9fdae-ae5f-4d3e-bf5e-ac3d0165178a).

We obtain information about the type class `Eq` as follows (some text is removed for legibility):
```hs
Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in ‘Data.Either’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
instance Eq Bool -- Defined in ‘GHC.Classes’
...
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
instance Eq () -- Defined in ‘GHC.Classes’
instance Eq Integer
  -- Defined in ‘integer-gmp-1.0.2.0:GHC.Integer.Type’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Base’
```
This tells us the following:
1. The type class `Eq` provides two functions, `(==)` and `(/=)`.
2. To implement the type class on a type `a`, we have to implement at least one of `(==) :: a -> a -> Bool` or `(/=) :: a -> a -> Bool`.
3. Many instances of `Eq` are implemented, e.g., for `Word`, `Ordering`, `Int`,...
   Furthermore, we have **derived** instances:
   1. If types `a` and `b` are instances, then the type `(a,b)` of pairs of values in `a` and `b` is also an instance.
   2. Similarly, if `a` is an instance of `Eq`, so is the type `[a]` of lists of values in `a`.

Further information that is **not** printed above:
1. When only one of `(==)` and `(/=)` is provided by the user when implementing the instance, the other is defined automatically as its **negation**, e.g.,
   `x /= y = not (x == y)`.
2. The information does not say what the implementation of `(==)` is for any of the instances. This requires looking at the source code.


## Summary: type classes and instances

* A `class` in Haskell is like an interface in Java.

* We implement a class in Haskell using the keyword `instance`.

* Only types that are introduced using `data` or `newtype` can be made instances of classes (although GHC has some extensions to get around this...).

* It is only possible to declare a **single** instance of a class for any given `data` type or `newtype` (although GHC has some extensions to get around this...).

* In a function type, everything before `=>` is a **class constraint**: the function is only available for such types that are an instance of the mentioned classes.


## Inheritance: Extending a type class

Just like a Java interface can extend an interface, a type class can extend a type class.
Consider the following example:
```hs
Prelude> :i Ord
class Eq a => Ord a where
...
```
Here, the type class `Ord` (which we look at in detail below) extends the type class `Eq`. In other words, in order to turn a type `a` into an instance of `Ord`, we first need to turn it into an instance of `Eq`.
This will be studied in more detail later.

## Exercises:

1. Find all the basic instances of the type class `Bounded` that are defined in the GHC Prelude (the libraries that are loaded when starting `ghci`, without importing any additional libraries). Find out what `minBound` and `maxBound` are for each of the instances.
2. What type classes do the type classes `Fractional`, `Floating`, `Integral` extend? What functions do they provide? Which type class would you choose to implement a trigonometric calculus?
3. Another type class:
    1. Which type class defines the function `enumFromTo`?
    2. Evaluate `enumFromTo` on elements of each instance of that type class.
    3. Explain the different output between `:type enumFromTo 4 8` and `:type enumFromTo 4 (8 :: Int)`. If you are unsure about the answer, ask in the Teams chat.
4. Why does Haskell only allow **one** instance of any type class on a given type?

# Functions in Haskell

* Read also Chapter 4 of the text book "Programming in Haskell.

## Overview

In this lesson, we study various ways how functions can be defined in Haskell.
We will study the following ways:

1. Composition of existing functions
2. Conditionals (`if _ then _ else _ `)
3. Guarded equations
4. Pattern matching
5. Lambda expressions

At the end, we will also look at **operators** (infix function symbols such as `++`), and how to turn them into functions.


## Composing functions

A video for this section, including explanation of the exercise, is [here](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=fbf5d940-700b-46ab-aee2-ac3e016514bd).

We can compose existing functions to get new ones.
For instance:
```haskell
removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ drop n xs
```

**Exercise:** Using the functions above, write a function that removes both the first and the last element of a list.

## Conditionals

Haskell provides `if _ then _ else _`. It is typed `Bool -> a -> a -> a`, polymorphically.
```haskell
abs' :: Integer -> Integer
abs' n = if n >= 0 then n else -n
```
**Note:** The `else` branch is mandatory.

We can nest `if _ then _ else _`:
```haskell
howMuchDoYouLikeHaskell :: Int -> String
howMuchDoYouLikeHaskell x = if x < 3 then "I dislike it!" else
                               if x < 7 then "It's ok!" else
                                 "It's fun!"
```
This is difficult to read, however; guarded equations (see below) can be more pleasant to read.
We will avoid conditionals.

**Exercise:** Read the [discussion about `if _ then _ else _ ` on the Haskell wiki](https://wiki.haskell.org/If-then-else).


## Guarded equations

A video for this section, including explanation for the exercise, is [here](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=b80a3cd6-5293-4d0b-8177-ac3e01692c24).

Guarded equations are an alternative to `if _ then _ else _` expressions. They are often more readable:
```haskell
abs :: Int -> Int
abs n | n >= 0    = n
      | otherwise = -n
```
Here, `n >= 0` and `otherwise` are called **guards**; they are Booleans.
The function returns the value after the **first** guard that evaluates to `True`.

Guarded equations are more convenient to use than `if _ then _ else _`:
```haskell
howMuchDoYouLikeHaskell2 :: Int -> String
howMuchDoYouLikeHaskell2 x | x < 3       = "I dislike it!"
                           | x < 7       = "It's ok!"
                           | otherwise   = "It's fun!"
```

**Exercise:** Using guarded equations, write a function of type `Int -> Int -> Bool` that returns `True` if the first argument is greater than the second and less than twice the second.


## Pattern matching

**Pattern matching** analyses the input according to how it is built.
The input is matched against a sequence of patterns; the first pattern that matches determines the output of the function.

### Overview

There are only two possibilities for what a Boolean value can be: `True` or `False`.
It is hence sufficient to have patterns for these two cases:
```haskell
notB :: Bool -> Bool
notB False = True
notB True = False
```

There is only one way to make a pair:
```haskell
swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)
```

There are two ways to make a list:
```haskell
isEmpty :: [a] -> Bool
isEmpty []     = True
isEmpty (x:xs) = False
```
We will look at all of these in detail now.

### On Booleans

One of the simplest patterns is to match for Booleans.

If the input is just one Boolean, there are only two patterns:
```haskell
notB' :: Bool -> Bool
notB' False = True
notB' True = False
```
If a function takes two Booleans as input, there are 2^2 = 4 patterns:
```haskell
andB :: Bool -> Bool -> Bool
andB True True = True
andB True False = False
andB False True = False
andB False False = False
```
The last three patterns can be combined. Here, the wildcard pattern `_` matches anything, and discards it:
```haskell
andB' :: Bool -> Bool -> Bool
andB' True True = True
andB' _ _      = False
```
There is a difference between these two versions: in the latter, if the first argument is `False`, then the second argument does not need to be evaluated: `False` is returned immediately.

In the next example, the pattern `b` matches anything. However, in contrast to `_`, **we can use `b`** on the right-hand side of `=`:
```haskell
andB'' :: Bool -> Bool -> Bool
andB'' True b  = b
andB'' False _ = False
```

**Exercise:** Write a function `orB :: Bool -> Bool -> Bool` that returns `True` if at least one argument is `True`.


### Non-exhaustive patterns

Consider the following example:
```haskell
isTrue :: Bool -> Bool
isTrue True = True
```
**Question:** What will `isTrue False` evaluate to?

**Answer:** This is a non-exhaustive pattern, and `isTrue False` will raise an exception:
```hs
*Main> isTrue False
*** Exception: defining-functions.hs:36:1-18: Non-exhaustive patterns in function isTrue
```
We can choose to throw a custom-made exception instead:
```haskell
isTrue' :: Bool -> Bool
isTrue' True = True
isTrue' False = error "not True"
```


### On tuples

If the function we are defining expects as input a **tuple**, we can match against the individual components:

```haskell
fst :: (a,b) -> a
fst (x,y) = x
```
We actually don't use `y` in the output of the function, so we can use `fst (x,_) = x` instead.
Similarly,
```haskell
snd :: (a,b) -> b
snd (_,y) = y
```
This generalizes to tuples of three or more components:
```haskell
third :: (a, b, c) -> c
third (_, _, z) = z
```
We can match several tuples at the same time:
```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```

**Exercise:** Write a function `swap :: (a, b) -> (b, a)` that swaps the elements of a pair.

### On lists

See also this [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=3eebebd8-9a8c-49ef-8a85-ac3f0003b4ee).

All lists are built by prepending `(:)`, successively, elements to an existing list, starting with the empty list `[]`.
That means, the list `[1, 2, 3]` has been obtained as `1:[2,3]`, etc. - `[1, 2, 3]` is short for `1:(2:(3:[]))`.
In other words, every list in `[a]` is either
1. the empty list; or
2. of the form `x:xs` for `x :: a` and `xs :: [a]`.

```haskell
isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' (x:xs) = False
```
We are not actually using `x` or `xs` in the output of the second pattern, so we can write the function more simply as
```haskell
isEmpty'' :: [a] -> Bool
isEmpty'' [] = True
isEmpty'' (_:_) = False
```
Note that the parentheses around `_:_` in the second pattern are necessary!

We can write more complex list patterns. To return the second element of a list:
```haskell
sndElem :: [a] -> a
sndElem (_:x:_) = x
```

### Case expressions

The aforementioned patterns are special forms, for Booleans, and lists.
The general form for such pattern matching is via `case` expressions:

```haskell
isEmpty2 :: [a] -> Bool
isEmpty2 x = case x of [] -> True
                       (_:_) -> False
```
Here, it is important that all the patterns are exactly aligned, i.e., the `[]` and `(_:_)` must start in exactly the same column.

## Lambda expressions

Lambda expressions are **nameless** functions. They are particularly useful in **higher-order functions**, which will be discussed in a later lesson.
A video accompanying this section is [here](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=5734065c-57af-43b0-a5be-ac400000eb9c).

Lambda expressions are of the form `\<input variables> -> <output>`.
For instance, we can define a function that returns its double as `\x -> 2 * x`.
Here, the input variable is indicated by the backslash `\`. After the arrow `->`, the output of the function is specified.
(`\` stands for the greek letter λ (lambda), see [Wikipedia](https://en.wikipedia.org/wiki/Lambda)).
Thus, the following definitions are equivalent:
```haskell
double :: Int -> Int
double x = 2 * x

double' :: Int -> Int
double' = \x -> 2 * x
```
Lambda expressions can have **several** input variables:
```haskell
mult :: Int -> Int -> Int
mult x y = x * y

mult' :: Int -> Int -> Int
mult' = \x y -> x * y
```
Here, the second variant is a short form of
```haskell
mult'' :: Int -> (Int -> Int)
mult'' = \x -> (\y -> x * y)
```
Just like a pattern can ignore (part of) the input, a lambda expression can ignore its input
```haskell
alwaysZero :: Bool -> Int
alwaysZero = \_ -> 0
```

One important application of lambda expressions are **higher-order functions**, where functions are arguments to other functions.
Consider
```haskell
apply :: (a -> b) -> a -> b
apply f x = f x
```

```hs
*Main> apply (\_ -> 5) 'r'
5
*Main> apply (\ x -> if x < 0 then "Less than zero!" else "Greater or equal than zero!") (-3)
"Less than zero!"
```


## Operators and sections

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=361993f2-6410-4be3-8f23-ac46017605da) on operators and sections.

When a function has two arguments, such as `(:)`, we can write it infix, between its two arguments.
A function that is used infix (hence necessary binary) is called an **operator**.
1. Any binary function can be turned into an operator by enclosing it in backticks. E.g. `div 7 2` can be written ``7 `div` 2``.
2. Conversely, any operator can be used prefix by enclosing it in parentheses, e.g., `(:) 1 [2,3]`.

Every operator `⊗` with inputs of type `a` and `b` and output of type `c` gives rise to three **sections**:
1. `(⊗) :: a -> b -> c`. Here, `(⊗) = \x y -> x ⊗ y`.
2. `(x ⊗) :: b -> c`, where `x :: a`. Here, `(x ⊗) = \y -> x ⊗ y`.
3. `(⊗ y) :: a -> c`, where `y :: b`. Here, `(⊗ y) = \x -> x ⊗ y`.

Sections can be used to concisely define functions:
```haskell
square :: Int -> Int
square = (^2)

reci :: Fractional a => a -> a
reci = (1 /)
```
Remarks:
1. An operator `⊗` by itself is not a valid Haskell expression: it needs to be used as a section, e.g., `(⊗)`.
2. Sections are useful when programming with higher-order functions (cf. later lesson.).


## Exercises
(Adapted and expanded from the book "Programming in Haskell)
1. Define three variants of a function `third :: [a] -> a` that returns the third element in any list that contains at least this many elements, using
    1. `head` and `tail`
    2. list indexing `!!`
    3. pattern matching
2. Define a function `safetail :: [a] -> [a]` that behaves like tail except that it maps `[]` to `[]` (instead of throwing an error). Using `tail` and `isEmpty :: [a] -> Bool`,
   define `safetail` using
   1. a conditional expression
   2. guarded equations
   3. pattern matching

## See also
1. [Chapter 3, "Syntax in Functions" of "Learn You a Haskell"](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/Resources/LearnYouaHaskell/LearnYouaHaskell.pdf)
2. Haskell Wiki on [Sections](https://wiki.haskell.org/Section_of_an_infix_operator)

## Summary
1. We have seen several ways to define functions: composition, conditionals, guard equations, pattern matching, lambda expressions.
2. When patterns are not exhaustive, functions raise an exception whenever no pattern matches. To avoid this, one may use a catch-all `otherwise` pattern at the end.
3. Any pattern matching can be expressed using a `case` expression.
4. Anonymous functions can concisely be written using lambda expressions.

# More on type classes and instances


In this section, we study in detail two important type classes:
1. The type class `Ord` for ordered types.
2. The type class `Num` for numeric types.

We also study an example of an *instance declaration with constraints*: `instance Ord a => Ord [a]`.
This instance tells us that whenever `a` is an instance of `Ord`, then so is `[a]`.



## The type `Ordering` and the typeclass `Ord`

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c796ea2f-759d-4e2f-ae62-ac3d0170e5b7) on this section.

The type class `Ord` implements the idea that elements of a type can be **compared not only for equality, but also for less/greater than**.
A comparison on a type `a` is a map `compare : a -> a -> Ordering`, where the type `Ordering` is defined as follows:
```hs
data  Ordering  =  LT | EQ | GT
          deriving (Eq, Ord, Enum, Read, Show, Bounded)
```
Here, `LT` stands for *less than*, `EQ` stands for *equal*, and `GT` stands for *greater than*.

The following defines the `Ord` class for types `a` which are in the `Eq` class.
```hs
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y
```

There is a seeming circularity between the definition of `Ordering` and `Ord`, in that each one refers to the other. We may see this as a mutually recursive definition.


For example, the `compare` function on lists over `a` (where `a` is already equipped with a function `compare`) is implemented as follows:
```hs
instance (Ord a) => Ord [a] where
  compare [] []         = EQ
  compare [] (_:_)      = LT
  compare (_:_) []      = GT
  compare (x:xs) (y:ys) = case compare x y of
                              EQ    -> compare xs ys
                              other -> other
```

----

**Exercises**

1. From reading the code, explain how two lists are compared.
2. Run some examples of comparisons between lists to confirm or refute your explanation.

This [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a6abe9f7-b812-4af3-91b7-ac4200b9b26c) gives an explanation of the implementation of `compare :: [a] -> [a] -> Ordering`.

----



## The type class `Num`

Consider the following examples
```hs
Prelude> 5 + 3
8
Prelude> 3.14159 + 2.71828
5.85987
```
Here, the operator `+` acts on any type that is an instance of the `Num` typeclass: for any such type `a`, the function `(+)` is of type `a -> a -> a`, used as an infix operator.

We can use the `:info` command to find out what operations the `Num` typeclass provides:

```hs
Prelude> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```
This shows that any type `a` that is an instance of `Num` comes with operations `(+)`, `(-)`, `(*)`,..., `fromInteger`.
(In particular, `1` without a type annotation is not an integer, but instead an element of any type `a` that is an instance of `Num`; specifically, the image of the integer `1 :: Integer` under the function `fromInteger :: Integer -> a`.)

We also see that there are five **instances** of the type class `Num` defined, for the types `Word`, `Integer`, `Int`, `Float`, and `Double`.
When we want to consider `1` as an element of a particular numeric type, we can do that by **annotating** it with that type, e.g.,
```hs
Prelude> :type 1 :: Word
1 :: Word :: Word
```
Here, we **check** that `1 :: Word` instead of asking `ghci` to **infer** the type of `1` for us. `ghci` then just needs to check that `Word` is an instance of `Num`.
Similarly, for `(+)` we can check
```
Prelude> :type (+) :: Integer -> Integer -> Integer
(+) :: Integer -> Integer -> Integer
  :: Integer -> Integer -> Integer
```
This check fails for types for which no instance of `Num` has been declared, e.g., for `Char`:
```hs
Prelude> :type (+) :: Char -> Char -> Char

<interactive>:1:1: error:
    • No instance for (Num Char) arising from a use of ‘+’
    • In the expression: (+) :: Char -> Char -> Char
```

## See also

3. A blog post comparing Java and Haskell: https://mmhaskell.com/blog/2019/1/28/why-haskell-iv-typeclasses-vs-inheritance. Do you agree with it? Why (not)?

## Summary
1. We have studied in detail one function that uses both pattern matching on lists and a `case` expression (a more general form of pattern matching).
1. We have seen how instances can be derived automatically from other instances, using the example `instance Ord a => Ord [a]`.
1. We have taken a look at the type class `Num` for number types.
1. We have seen how to use a type annotation to force a Haskell expression to have a particular type, e.g., `1 :: Word`.


# List Comprehensions

These notes should be read in conjunction with chapter 5 of our textbook Programming in Haskell.

* We discuss some examples from the [Haskell'98 standard prelude](https://www.haskell.org/onlinereport/standard-prelude.html) for pedagogical purposes.

* See the [prelude for the current version of the language](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) for all predefined classes and their instances.

## Note:

Please ignore this declaration until you have read the *Encoding and Decoding* section in the **The Caesar Cipher** example at the end of this handout. We mention it here, in order to ensure that the generated haskell file contains this import statement at the beginning.

```haskell
import Data.Char
```

## Basic Concepts

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=1bd321bf-1f1c-45fd-8e9a-ac3e01048680) on this section.

In mathematics, the _comprehension_ notation can be used to construct new sets from existing sets. For example:

![squarenumbers](./images/eq_1.png)

Produces the set {1, 4, 9, 16, 25} of all numbers x^2 such that x is an element of the set {1...5}

In Haskell, a similar comprehension notation can be used to construct __new lists__ from old lists. For example: 

```hs
> [x^2 | x <- [1..5]] 
[1,4,9,16,25]
```
The symbol `|` is read is _such that_, `<-` is read as _drawn from_, and the expression `x <- [1..5]` is called a __generator__. A generator states how to generate values for x. 

Comprehensions can have _multiple_ generators, separated by commas. For example:

```hs
> [(x,y) | x <- [1,2,3], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
```

Changing the order of the generators changes the order of the elements in the final list:

```hs
> [(x,y) | y <- [4,5], x <- [1,2,3]]
[(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]
```

Multiple generators are like nested loops, with later generators as more deeply nested loops whose variables change value more frequently.


## Dependent Generators

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a5ce4d07-dd54-47cc-8c4d-ac3e010489c3) on this section.

Later generators can depend on the variables that are introduced by earlier generators:

```hs
> [(x,y) | x <- [1..3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
```
The above list includes all pairs of numbers `(x,y)` such that `x,y` are elements of the list `[1..3]` and `y` >= `x`.

Using a dependent generator we can define the library function that _concatenates_ a list of lists:

```hs
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
```
For example:
```hs
> concat [[1,2,3],[4,5],[6]]
[1,2,3,4,5,6]
```
The wildcard pattern `_` is sometimes useful in generators to discard certain elements from a list. For example, a function that selects all the first components from a list of pairs can be defined as follows:

```haskell
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]
```

Similarly, the library function that calculates the length of a list can be defined by replacing each element by one and summing the resulting list:

```hs
length :: [a] -> Int
length xs = sum [1 | _ <- xs]
```

In the above case, the generator `_ <- xs` simply serves as a counter to govern the production of the appropriate number of ones. 

## Guards

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=b18b42b5-0509-4976-a51c-ac3e01048c63) on this section.

List comprehensions can use **guards** to restrict the values produced by earlier generators. If the guard is _true_, then the current values are retained; if it is _false_, then they are discarded. 

For example:

```hs
> [x | x <- [1..10], even x]
[2,4,6,8,10]
```

Similarly, a function that maps a positive integer to its list of positive factors can be defined by:

```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
```
For example:
```hs
> factors 15
[1,3,5,15]
```

A positive integer is prime if its only factors are 1 and itself. Hence, using `factors` we can define a function that decides if a number is prime:

```haskell
prime :: Int -> Bool
prime n = factors n == [1,n]
```
For example:

```hs
> prime 15
False

> prime 7
True
```

Note: To decide that a number such as 15 is not prime does not require the function `prime` to produce all of its factors, because under lazy evaluation the result `False` is returned as soon as any factor other than one or the number itself is produced. 

Using a guard we can now define a function that returns the list of all primes up to a given limit:

```haskell
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
```

For example:

```hs
> primes 40
[2,3,5,7,11,13,17,19,23,29,31,37]
```

## The Zip Function

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=aa33daea-3f09-40a7-9e74-ac3e01048ee7) on this section.

A useful library function is `zip`, which maps two lists to a list of pairs of their corresponding elements.

```hs
zip :: [a] -> [b] -> [(a,b)]
```
For example:

```hs
> zip ['a','b','c'] [1,2,3,4]
[('a',1),('b',2),('c',3)]
```
Using `zip` we can define a function that returns the list of all pairs of adjacent elements from a list:

```haskell
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
```

For example:
```hs
> pairs [1,2,3,4]
[(1,2),(2,3),(3,4)]
```

Using `pairs` we can define a function that decides if the elements in a list are sorted:

```haskell
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]
```

For example:
```hs
> sorted [1,2,3,4]
True
> sorted [1,3,2,4]
False
```

Using `zip` we can define a function that returns the list of all positions of a value in a list:

```haskell
positions :: Eq a => a -> [a] -> [Int]
positions x xs =
   [i | (x',i) <- zip xs [0..], x == x']
```

For example:
```hs
> positions 0 [1,0,0,1,0,1,1,0]
[1,2,4,7]

> positions False [True, False, True, False]
[1,3]
```

## String Comprehensions

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=bd34ce3f-2698-4ee6-8c4a-ac3e0104a34e) on this section.

A string is a sequence of characters enclosed in double quotes. Internally, however, strings are represented as lists of characters.

For example the string `"abc" :: String` is just an abbreviation for the list of characters `['a', 'b', 'c'] :: [Char]`.

Because strings are just special kinds of lists, any polymorphic function that operates on lists can also be applied to strings. For example:

```hs
> "abcde" !! 2
'c'
> take 3 "abcde"
"abc"
> length "abcde"
5
> zip "abc" [1,2,3,4]
[('a',1),('b',2),('c',3)]
```

Similarly, list comprehensions can also be used to define functions on strings, such as counting how many times a character occurs in a string:

```haskell
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
```

For example:
```hs
> count 's' "Mississippi"
4
```

Similarly, we can define a function that returns the number of lower-case letters and particular characters that occur in a string:

```haskell
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
```

For example
```hs
> lowers "Haskell"
6
```

## Extended Programming Example - The Caesar Cipher

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=9b744b7b-a16b-4f6d-b901-ac3e0104c35d) on this section.

The Caesar cipher is a well-known encoding method for strings, although its a primitive one. It simply replaces each letter in the string by the letter _n_ places (aka _shift factor_) further down the alphabet, wrapping around at the end of alphabet. For example, the string 

"haskell is fun" would be encoded as "kdvnhoo lv ixq" (for n = 3). 

"haskell is fun" would be encoded as "rkcuovv sc pex" (for n = 10). 

### Encoding and Decoding

For this example, we will use a number of standard functions on characters that are provided in a library called `Data.Char`, which can be loaded into a Haskell script by including the following declaration at the start of the script:

```hs
import Data.Char
```

Note: For simplicity, we will only encode the lower-case letters within a string, leaving other characters such as upper-case letters and punctuation unchanged. 

Lets start by defining a function `let2int` that converts a lower-case letter into corresponding integer between 0 and 25. We will also define the opposite function `int2let`, which converts a number to the corresponding letter. 

```haskell
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
```

For example
```hs
> let2int 'a'
0

> int2let 0
'a'
```

Using the above two functions, we can define a function `shift` that applies a shift factor to a lower-case letter by converting the letter into the corresponding integer, adding on the shift factor and taking the remainder when divided by twenty-six, and converting the resulting integer back into a lower-case letter:

```haskell
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c
```

For example: 

```hs
> shift 3 'a'
'd'

> shift 3 'z'
'c'

> shift (-3) 'c'
'z'
```
Now we can define the `encode` function that uses the shift function within a list comprehension:

```haskell
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
```

For example:
```hs
> encode 3 "haskell is fun"
"kdvnhoo lv ixq"
```

We don't need a separate function to decode a string. We can reuse the `encode` function and provide it with a negative shift factor. 

For example:
```hs
> encode (-3) "kdvnhoo lv ixq"
"haskell is fun"
```

### Frequency Tables

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=874a8714-47e2-45ea-bfe0-ac3e0104d9f2) on this section.

The key to cracking the Caeser cipher is the observation that some letters are used more frequently than others in English. By analyzing a large volume of English text, we can derive the following approximate percentage frequencies. 

```haskell
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
```

For example the letter 'e' occurs most often (12.7%) and the letters 'q' and 'z' occur least often (0.1% each).

Lets define a function that calculates the percentage of one integer with respect to another:

```haskell
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
```

For example:

```hs
> percent 5 15
33.333336
```

We can now define a function that returns a frequency table for any given string:

```haskell
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs
```

For example:

```hs
> freqs "abbcccddddeeeee"
[6.666667, 13.333334, 20.0, 26.666667, ..., 0.0]
```
### Cracking the Cipher

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=29b5fe9f-ee0c-4f9b-b883-ac3e0104ff1f) on this section.

A standard method for comparing a list of observed frequencies _os_ with a list of expected frequencies _es_ is the _chi-square statistic_, which is defined by the following summation:

![chisquare](./images/eq_2.png)

where _n_ denotes the length of the two lists.

Note: We are interested in the fact that the smaller the value it produces, the better is a match between the two frequency lists. The above formula can be translated into a function definition:

```haskell
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
```

We also define a function `rotate` that rotates the elements of a list by _n_ places to the left, wrapping around at the start of the list, and assuming that the integer argument _n_ is between zero and the length of the list:

```haskell
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
```

For example:
```hs
> rotate 3 [1,2,3,4,5]
[4,5,1,2,3]
```

_Now suppose that we are given an encoded string, but not the shift factor that was used to encode it, and wish to determine this number in order to decode the string. How can we do this?_

We can achieve this by producing the frequency table of the encoded string, calculating the chi-square statistic for each possible rotation of this table with respect to the table of expected frequencies, and using the position of the minimum chi-square value as the shift factor. 

For example, if we let 
```hs
table' = freqs "kdvnhoo lv ixq"
```
then

```hs
[chisqr (rotate n table') table | n <- [0..25]]
```
gives the result

```hs
[1408.8524, 640.0218, 612.3969, 202.42024, ..., 626.4024]
```

where the value 202.42024 appearing at position 3 is the minimum value. We conclude that 3 is the most likely shift factor that we used to encode this string (which we know is the case!).

Now we can write the complete function for cracking the cipher:

```haskell
crack :: String -> String
crack xs = encode (-factor) xs
  where
     factor = head (positions (minimum chitab) chitab)
     chitab = [chisqr (rotate n table') table | n <- [0..25]]
     table' = freqs xs
```

For example:

```hs
> crack "kdvnhoo lv ixq"
"haskell is fun"

> crack "vscd mywzboroxcsyxc kbo ecopev"
"list comprehensions are useful"
```

Note: The _crack_ function can decode most strings produced using the Caesar cipher, however, it may not be successful if the string is short or has an unusual distribution of letters.

For example:

```hs
> crack (encode 3 "haskell")
"piasmtt"

> crack (encode 3 "boxing wizards jump quickly")
"wjsdib rduvmyn ephk lpdxfgt"
```

## Exercises

(1) A triple (x,y,z) of positive integers is called pythagorean if x^2 + y^2 = z^2 . Using a list
comprehension, define a function:
```hs
pyths :: Int -> [(Int,Int,Int)]
```
that maps an integer n to all such triples with components in [1..n].
For example:
```hs
> pyths 5
[(3,4,5),(4,3,5)]
```

(2) A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. Using a list comprehension, define a function

```hs
perfects :: Int -> [Int]
```
that returns the list of all perfect numbers up to a given limit. For example:
```hs
> perfects 500
[6,28,496]
```

(3) The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of the corresponding integers:

![squarenumbers](./images/ex_3.png)

Using a list comprehension, define a function that returns the scalar product of two lists.


# Recursive Functions

These notes should be read in conjunction with chapter 6 of our textbook Programming in Haskell.

* We discuss some examples from the [Haskell'98 standard prelude](https://www.haskell.org/onlinereport/standard-prelude.html) for pedagogical purposes.

* See the [prelude for the current version of the language](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) for all predefined classes and their instances.


## Basic Concepts

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=48fbd212-bfcd-4f05-9630-ac4f012526fb) on this section.

As we have seen, many functions can naturally be defined in terms of other functions. For example, a function that returns the factorial of a non-negative integer can be defined by using library functions to calculate the product of the integers between one and the given number:

```hs
fac :: Int -> Int
fac n = product [1..n]
```

Expressions are evaluated by a stepwise process of applying functions to their arguments.

For example:

```hs
fac 5
= product [1..5]
= product [1,2,3,4,5]
= 1*2*3*4*5
= 120
```

In Haskell, functions can also be defined in terms of themselves. Such functions are called __recursive__.

```haskell
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
```
The function __fac__ maps 0 to 1 (_base case_), and any other integer to the product of itself and the factorial of its predecessor (_recursive case_).

For example:

```hs
fac 3
= 3 * fac 2
= 3 * (2 * fac 1)
= 3 * (2 * (1 * fac 0))
= 3 * (2 * (1 * 1))
= 3 * (2 * 1)
= 3 * 2
= 6
```

Note:

* fac 0 = 1 is appropriate because 1 is the identity for multiplication: 1 * x = x = x * 1.

* The recursive definition diverges on integers < 0 because the base case is never reached:

```hs
> fac (-1)
*** Exception: stack overflow
```

### Why is Recursion Useful?

* Some functions, such as factorial, are simpler to define in terms of other functions.

* As we shall see, however, many functions can naturally be defined in terms of themselves.

* Properties of functions defined using recursion can be proved using the simple but powerful mathematical technique of _induction_.

## Recursion on Lists

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=8f28b057-a77c-4b81-8769-ac4f01252b5a) on this section.

Recursion is not restricted to numbers, but can also be used to define functions on lists.

```haskell
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns
```

The `product'` function maps the empty list to 1 (_base case_), and any non-empty list to its head multiplied by the `product'` of its tail (_recursive case_).

For example:

```hs
product' [2,3,4]
= 2 * product' [3,4]
= 2 * (3 * product' [4])
= 2 * (3 * (4 * product' []))
= 2 * (3 * (4 * 1))
= 24
```

Note: Lists in Haskell are actually constructed one element at a time using the __cons__ operator. Hence, [2,3,4] is just an abbreviation for 2:(3:(4:[])).

Using the same pattern of recursion as in `product'` we can define the length function on lists.

```hs
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
```

The length function maps the empty list to 0 (_base case_), and any non-empty list to the successor of the length of its tail (_recursive case_).

For example:

```hs
length [1,2,3]
= 1 + length [2,3]
= 1 + (1 + length [3])
= 1 + (1 + (1 + length []))
= 1 + (1 + (1 + 0))
= 3
```

Using a similar pattern of recursion we can define the reverse function on lists.

```hs
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
```

The function reverse maps the empty list to the empty list, and any non-empty list to the reverse of its tail appended to its head.

For example:

```hs
reverse [1,2,3]
= reverse [2,3] ++ [1]
= (reverse [3] ++ [2]) ++ [1]
= ((reverse [] ++ [3]) ++ [2]) ++ [1]
= (([] ++ [3]) ++ [2]) ++ [1]
= [3,2,1]
```

Interestingly, the append opearator ++ used in the above example can also be defined using recursion. 

```hs
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

For example:

```hs
[1,2,3] ++ [4,5]
= 1 : ([2,3] ++ [4,5])
= 1 : (2 : ([3] ++ [4,5]))
= 1 : (2 : (3 : ([] ++ [4,5])))
= 1 : (2 : (3 : [4,5]))
= [1,2,3,4,5]))
```

The recursive definition of ++ formalises the idea that two lists can be appended by copying elements from the first list until it is exhausted, at which point the second list is joined-on at the end.

## Multiple Arguments

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=dcc491f3-e715-4223-a9b7-ac4f01252e28) on this section.

Functions with more than one argument can also be defined using recursion. 

* Zipping the elements of two lists:

```hs
zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
```
For example:

```hs
zip ['a', 'b', 'c'] [1,2,3,4]
= ('a',1) : zip ['b', 'c'] [2,3,4]
= ('a',1) : ('b',2) : zip ['c'] [3,4]
= ('a',1) : ('b',2) : ('c',3) : zip [] [4]
= ('a',1) : ('b',2) : ('c',3) : []
= [('a',1), ('b',2), ('c',3)]
```

* Remove the first n elements from a list:

```hs
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs
```
For example:

```hs
drop 3 [4,6,8,10,12]
= drop 2 [6,8,10,12]
= drop 1 [8,10,12]
= drop 0 [10,12]
[10,12]
```

## Multiple Recursion

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=1ebd39f3-edad-4c4d-b777-ac4f01253209) on this section.

Functions can also be defined using _mulitple recursion_, in which a function is applied more than once in its own definition.

A function that calculates the nth Fibonacci number (0, 1, 1, 2, 3, 5, 8, 13, ...) for any integer n >= 0 can be defined using double recursion as follows:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
```

For example:

```hs
fib 4
= fib (2) + fib (3)
= fib (0) + fib (1) + fib (1) + fib (2)
= 0 + 1 + 1 + fib (0) + fib (1)
= 0 + 1 + 1 + 0 + 1
= 3
```

## Mutual Recursion

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=88804dd0-9656-4969-af7d-ac4f01253d4c) on this section.

Functions can also be defined using _mutual recursion_, where two or more functions are defined recursively in terms of each other. For non-negative integers, we can define even and odd numbers using mutual recursion.

```hs
even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)
```

For example:

```hs
even 4
= odd 3
= even 2
= odd 1
= even 0
= True
```

Similarly, functions that select the elements from a list at all even and odd positions (counting from zero) can be defined as follows:

```haskell
evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs
```
For example:

```hs
evens "abcde"
= 'a' : odds "bcde"
= 'a' : evens "cde"
= 'a' : 'c' : odds "de"
= 'a' : 'c' : evens "e"
= 'a' : 'c' : 'e' : odds []
= 'a' : 'c' : 'e' : []
= "ace"
```

## Programming Example - Quicksort

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e818b628-dc42-4b7f-96ca-ac4f012557da) on this section.

The quicksort algorithm for sorting a list of values can be specified by the following two rules:

* The empty list is already sorted;

* Non-empty lists can be sorted by sorting the tail values <= the head, sorting the tail values > the head, and then appending the resulting lists on either side of the head value.

Using recursion, this specification can be translated directly into an implementation:

```haskell
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]
```
Note: This is probably the simplest implementation of quicksort in any programming language!

For example (abbreviating qsort as q):

![qsortexample](./images/qsort.png)

## Advice on Recursion

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a733c6a0-01c7-4769-95f0-ac4f01256f33) on this section.

In this section, we offer some advice for defining functions in general, and recursive functions in particular, using a five-step process.

Example - drop: that removes a given number of elements from the start a list.

* Step 1: define the type

The drop function takes an integer and a list of values of some type a, and produces another list of such values.

```hs
drop :: Int -> [a] -> [a]
```
We have already made four design decisions in defining this type:

(i) using integers rather than a more general numeric type - for simiplicity

(ii) using currying rather than taking arguments as a pair - for flexibility

(iii) supplying the integer argument before the list argument - for readability (_drop_ __n__ _elements from_ __xs__)

(iv) making the function _polymorphic_ in the type of the list elements - for generality.

* Step 2: enumerate the cases

We have a total of four possible cases i.e. two possible values for integer argument (0 and n) and two possibilities for the list argument ([] and (x:xs)) thus giving us four possible combinations (cases). 

```hs
drop 0 []     = 
drop 0 (x:xs) = 
drop n []     = 
drop n (x:xs) = 
```

* Step 3: define the simple cases

By definition, removing zero elements from the start of any list gives the same list.

```hs
drop 0 []     = []
drop 0 (x:xs) = x:xs
drop n []     = 
drop n (x:xs) = 
```

Attempting to remove one or more elements from the empty list is invalid, so the third case could be omitted, which would result in an error being produced if this situation arises. However, we choose to avoid the production of an error by returning the empty list in this case:

```hs
drop 0 []     = []
drop 0 (x:xs) = x:xs
drop n []     = []
drop n (x:xs) = 
```

* Step 4: define the other cases

For removing one or more elements from a non-empty list, we drop the head of the list and recursively call itself with one less than the previous call on the tail of the list.

```hs
drop 0 []     = []
drop 0 (x:xs) = x:xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs
```
* Step 5: generalise and simplify

The first two equations for drop can be combined into a single equation that states that removing zero elements from any list gives the same list:

```hs
drop 0 xs     = xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs
```

The variable n in the second equation and x in the third can be replaced by the wildcard pattern \_, as these variables are not being used in the bodies of their equations.

```hs
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs
```
This is precisely the same definition of drop function as available in the standard prelude.

## Exercises

(1) Without looking at the standard prelude, define the following library functions using recursion:

* Decide if all logical values in a list are true:

```hs
and :: [Bool] -> Bool
```
* Concatenate a list of lists:

```hs
concat :: [[a]] -> [a]
```
* Produce a list with n identical elements:

```hs
replicate :: Int -> a -> [a]
```

* Select the nth element of a list:

```hs
(!!) :: [a] -> Int -> a
```

* Decide if a value is an element of a list:

```hs
elem :: Eq a => a -> [a] -> Bool
```

(2) Define a recursive function

```hs
merge :: Ord a => [a] -> [a] -> [a]
```
that merges two sorted lists of values to give a single sorted list. 

For example:

```hs
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
```


# Higher-order Functions

These notes should be read in conjunction with chapter 7 - Higher-order functions of our textbook Programming in Haskell.

* We discuss some examples from the [Haskell'98 standard prelude](https://www.haskell.org/onlinereport/standard-prelude.html) for pedagogical purposes.

* See the [prelude for the current version of the language](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html) for all predefined classes and their instances.

## Note:

Please ignore this declaration until you have read the *Base Conversion* section in the **Binary String Transmitter** example at the end of this handout. We mention it here, in order to ensure that the generated haskell file contains this import statement at the beginning.

```haskell
import Data.Char
```

## Basic Concepts

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=582e9fdc-df8a-423f-ac3a-ac590154bfa7) on this section.

A function is called `higher-order` if it takes a function as an argument or returns a function as a result.

```haskell
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

For example:

```hs
> twice (*2) 3
12

> twice (+10) 5
25

> twice (\ x -> x ^ 2) 3
81

> twice reverse [1,2,3]
[1,2,3]
```
The function `twice` is higher-order because it takes a function as its first argument.

### Why Are They Useful?

Higher-order functions allow for more reusable code, for example the `twice` function shows how we can apply the same function multiple times. The `map` function is another good example of code reusability.

## Processing Lists

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a3e15303-0a7e-4a0c-979a-ac590154c56d) on this section.

### The Map Function

The higher-order library function called `map` applies a function to every element of a list.

```hs
map :: (a -> b) -> [a] -> [b]
```
For example:

```hs
> map (+1) [1,3,5,7]
[2,4,6,8]

> map (^3) [1,3,5,7]
[1,27,125,343]

> map reverse ["conversation", "talking", "discussion"]
["noitasrevnoc","gniklat","noissucsid"]

```
The `map` function can be defined in a particularly simple manner using a list comprehension:

```hs
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
```
Alternatively, for the purposes of proofs, the map function can also be defined using recursion:

```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

### The Filter Function

The higher-order library function `filter` selects every element from a list that satisfies a predicate.

```hs
filter :: (a -> Bool) -> [a] -> [a]
```
For example:

```hs
> filter even [1..10]
[2,4,6,8,10]
```

Filter can be defined using a list comprehension:

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
```

Alternatively, it can be defined using recursion:

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
   | p x       = x : filter p xs
   | otherwise = filter p xs
```

## The Foldr Function

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=fa187ee8-66fe-442b-a566-ac590154c9bc) on this section.

A number of functions on lists can be defined using the following simple pattern of recursion:

```hs
f :: Num a => [a] -> a
f []     = v
f (x:xs) = x # f xs
```
The function `f` maps the empty list to some value `v`, and any non-empty list to some function `#` applied to its head and `f` of its tail.

For example:

```hs
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs
```

The higher-order library function `foldr` (_fold right_) encapsulates this simple pattern of recursion, with the function `#` and the value `v` as arguments.

For example:

```hs
sum = foldr (+) 0

product = foldr (*) 1

or = foldr (||) False

and = foldr (&&) True
```

`foldr` itself can be defined using recursion:

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```

However, it is best to think of `foldr` _non-recursively_, as simultaneously replacing each (`:`) in a list by a given function, and `[]` by a given value, as summarized by:

```hs
foldr (#) v [x0,x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
```

For example:

```hs
sum [1,2,3]
= foldr (+) 0 [1,2,3]
= foldr (+) 0 (1:(2:(3:[])))
= 1+(2+(3+0))
= 6
```
Replace each (:) by (+) and [] by 0.

```hs
product [1,2,3]
= foldr (*) 1 [1,2,3]
= foldr (*) 1 (1:(2:(3:[])))
= 1*(2*(3*1))
= 6
```
Replace each (:) by (*) and [] by 1.

### Other Foldr Examples

Even though `foldr` encapsulates a simple pattern of recursion, it can be used to define many more functions than might first be expected.

Recall the length function:

```hs
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
```
For example:

```hs
length [1,2,3]
= length (1:(2:(3:[])))
= 1+(1+(1+0))
= 3
```

We can replace each (:) by `\_ n -> 1+n` and [] by `0` to get:

```hs
length :: [a] -> Int
length = foldr (\_ n -> 1+n) 0
```
Now recall the reverse function:

```hs
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
```

For example:

```hs
reverse [1,2,3]
= reverse (1:(2:(3:[])))
= (([] ++ [3]) ++ [2]) ++ [1]
= [3,2,1]
```

Replace each (:) by `\x xs -> xs ++ [x]` and [] by `[]`. Hence, we have:

```hs
reverse :: [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []
```

Finally, we note that the append function (++) has a particularly compact definition using `foldr`:

```hs
(++ ys) = foldr (:) ys
```

Here we replace each (:) by `(:)` and [] by `ys`.

An even more concise definition is the following:

```hs
(++) = foldr (:)
```


## The Foldl Function

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=d5364f51-89e2-4f6d-87ae-ac590154cd35) on this section.

It is also possible to define recursive functions on lists using an operator that is assumed to _associate to the left_.

For example, the function `sum` can be redefined in this manner by using an auxiliary function `sum'` that takes an extra argument `v` that is used to accumulate the final result:

```hs
sum :: Num a => [a] -> a
sum = sum' 0
      where
         sum' v []     = v
         sum' v (x:xs) = sum' (v+x) xs
```

For example

```hs
sum [1,2,3]
= sum' 0 [1,2,3]
= sum' (0+1) [2,3]
= sum' ((0+1)+2) [3]
= sum' (((0+1)+2)+3) []
= (((0+1)+2)+3)
= 6
```

Generalizing from the above `sum` example, many functions on lists can be defined using the following simple pattern of recursion.

```hs
f :: Num a => a -> [a] -> a
f v []     = v
f v (x:xs) = f (v # x) xs
```

That is, the function maps the empty list to the _accumulator_ value `v`, and any non-empty list to the result of recursively processing the tail using a new accumulator value obtained by applying an operator `#` to the current value and the head of the list.

The above `sum` function can be re-written using the higher-order library function `foldl` (_fold left_) as:

```hs
sum :: Num a => [a] -> a
sum = foldl (+) 0
```

Similarly, we can define:

```hs
product :: Num a => [a] -> a
product = foldl (*) 1

or :: [Bool] -> Bool
or = foldl (||) False

and :: [Bool] -> Bool
and = foldl (&&) True

length :: [a] -> Int
length = foldl (\n _ -> n+1) 0
```

The `foldl` function can be defined using recursion:

```hs
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs
```
However, it is best to think of `foldl` _non-recursively_, in terms of an operator `#` that is assumed to associate to the left, as summarized by the following equation:

```hs
foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1) ...) #xn
```

## The Composition Operator

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=1ecaa5b5-799c-40cc-ba68-ac590154d461) on this section.

The library function (.) returns the composition of two functions as a single function.

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

That is, `f . g` is read as `f` _composed with_ `g`, is the function that takes an argument `x`, applies the function `g` to this argument, and applies the function `f` to the result.

Composition can be used to simplify nested function applications, by reducing the parenthesis and avoid the need to explicitly refer to the initial argument.

For example:

```hs
odd :: Int -> Bool
odd n = not (even n)
```

Can be defined as:

```hs
odd :: Int -> Bool
odd = not . even
```

Similarly, the following definitions

```hs
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

```haskell
sumsqreven :: Integral a => [a] -> a
sumsqreven ns = sum (map (^2) (filter even ns))
```
can be rewritten more simply as (adding prime to give these different names):

```haskell
twice' :: (a -> a) -> a -> a
twice' f = f . f

sumsqreven' :: Integral a => [a] -> a
sumsqreven' = sum . map (^2) . filter even
```

### Other Library Functions

The library function `all` decides if every element of a list satisfies a given predicate.

```hs
all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]
```

For example:

```hs
> all even [2,4,6,8,10]
True

> all odd [1,3,7,9,10]
False
```

Dually, the library function `any` decides if at least one element of a list satisfies a predicate.

```hs
any :: (a -> Bool) -> [a] -> Bool
any p xs = or [p x | x <- xs]
```

For example:

```hs
> any (== ' ') "abc def"
True

> any (> 10) [1,5,4,8,7]
False
```

The library function `takeWhile` selects elements from a list while a predicate holds of all the elements.

```hs
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
   | p x       = x : takeWhile p xs
   | otherwise = []
```

For example:

```hs
> takeWhile (/= ' ') "abc def"
"abc"

> takeWhile even [2,4,6,7,8]
[2,4,6]
```

Dually, the function `dropWhile` removes elements while a predicate holds of all the elements.

```hs
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
   | p x       = dropWhile p xs
   | otherwise = x:xs
```

For example:

```hs
> dropWhile (== 'a') "aaabcadef"
"bcadef"

> dropWhile odd [1,3,5,6,7]
[6,7]
```

## Programming Example - Binary String Transmitter

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=75f228df-249a-4265-8d51-ac590154e7f9) on this section.

A binary number is a sequence of zeros and ones, called _bits_, in which successive bits as we move to the left increase in weight by a factor of two.

For example the binary number `1101` can be understood as follows:

```hs
1101 = (8 * 1) + (4 * 1) + (2 * 0) + (1 * 1)
```

To simplify the definition of certain functions, we assume for the remainder of this example that binary numbers are written in _reverse_ order.

For example, `1101` would now be written as `1011`, with successive bits as we move to the right increasing in weight by a factor of two:

```hs
1011 = (1 * 1) + (2 * 0) + (4 * 1) + (8 * 1)
```

### Base Conversion

We begin by importing the library of useful functions on characters and declaring the type of bits as a synonym for the type of integers:

```hs
import Data.Char
```

```haskell
type Bit = Int
```

We can convert a binary number, represented as a list of bits, to an integer using the `bin2int` function:

```haskell
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1
```

The higher-order library function `iterate` produces an infinite list by applying a function an increasing number of times to a value:

```hs
iterate f x = [x, f x, f (f x), f (f (f x)), ...]
```

Hence, the expression `iterate (*2) 1` in the above definition of `bin2int` produces the list of weights `[1,2,4,8,...]`, which is then used to compute the weighted sum by means of a list comprehension.

```hs
> bin2int [1,0,1,1]
13
```

There is, however, a simpler way to define `bin2int` function, based on the algebraic properties of binary numbers. Consider an arbitrary four-bit number [a,b,c,d]. Applying `bin2int` to it produces the following weighted sum:

```hs
(1 * a) + (2 * b) + (4 * c) + (8 * d)
```
which can be restructured as follows:

```hs
(1 * a) + (2 * b) + (4 * c) + (8 * d)
= a + (2 * b) + (4 * c) + (8 * d)
= a + 2 * (b + (2 * c) + (4 * d))
= a + 2 * (b + 2 * (c + (2 * d)))
= a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))
```

From the above result, we see that the conversion can be written as replacing each `cons` by the function that adds its first argument to twice the second argument, and replacing the empty list by zero. Therefore, `bin2int` can be rewritten as (using a slightly different name):

```haskell
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0
```

The opposite function for converting a non-negative integer into binary number can be written as:

```haskell
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
```

For example:
```hs
> int2bin 13
[1,0,1,1]
```

We can now define a function `make8` that ensures we have binary numbers of the same length i.e. 8 bits. It either truncates or extends a binary number as appropriate to make it 8 bits long:

```haskell
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
```
For example:

```hs
> make8 [1,0,1,1]
[1,0,1,1,0,0,0,0]
```

### Transmission

There is also a [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=8d63a917-4e2a-43ad-8177-ac590155117d) on this section.

We can now define a function that encodes a string of characters as a list of bits by converting each character into a Unicode number, converting each such number into an eight-bit binary number, and concatenating each of these numbers together to produce a list of bits.

```haskell
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)
```

For example:
```hs
> encode "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
```

To decode a list of bits, we firstly define a function `chop8` that chops such a list up into eight-bit binary numbers:

```haskell
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
```
Now we can define the `decode` function that chops a list of bits, converts each resulting binary number into a Unicode number and then a character:

```haskell
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8
```
For example:

```hs
> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
"abc"
```
Finally, we can define the function `transmit` that simulates the transmission of a string of characters as a list of bits, using a perfect communication channel that we model using the identity function:

```haskell
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
```

For example:
```hs
> transmit "higher-order functions are easy"
"higher-order functions are easy"
```

The above example is actually encapsulating three functions i.e. encoding, transmission and decoding. We can separate the encoding and decoding steps to see what happens in between. The channels is an identity function i.e. it outputs whatever is given to it as input.

```hs
> encode "higher-order functions are easy"
[0,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,1,1,0,0,1,1,0,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,1,0,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,1,1,1,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,0,1,0,0,1,1,1,1,0]

> decode [0,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,1,1,0,0,1,1,0,0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,1,0,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0,1,0,0,1,0,1,1,0,1,1,1,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,0,1,0,0,1,1,1,0,1,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,0,1,0,0,1,1,1,1,0]
"higher-order functions are easy"
```

## Exercises

(1) What are higher-order functions that return functions as results better known as?

(2) Express the comprehension `[f x | x <- xs, p x]` using the functions `map` and `filter`. The function type is given as:
```hs
fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
```
For example:
```
> fun (^2) even [1..20]
[4,16,36,64,100,144,196,256,324,400]

> fun (^2) odd [1..20]
[1,9,25,49,81,121,169,225,289,361]
```
(3) Redefine `map f` and `filter p` using `foldr`. For your reference, here are the definitions of `map` and `filter` from lecture notes.
```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
   | p x       = x : filter p xs
   | otherwise = filter p xs
```

(4) Define a function `altMap :: (a -> b) -> (a -> b) -> [a] -> [b]` that alternatively applies the two argument functions to successive elements in a list.

For example:
```hs
> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]
```

# User defined data types - part 1

## Level of difficulty of this handout

This handout includes material of easy, medium, hard and advanced level. If some of the material feels difficult, it is probably because it is difficult rather than your fault. This means you have to work hard if you want to achieve a high mark in the module, as is the case for all modules.

<a name="videolectures"></a>
## Video lectures for this handout

The following videos are also linked at appropriate points of this handout for your convenience.

1. [Introduction, the booleans revisited, isomorphisms, Weekdays and the new `Maybe` type constructor](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=98b350c3-ec60-47c4-b2fb-ac610127b135) (35 min)
1. [Type retracts](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c78bfae6-79d6-4a09-bc70-ac6200c363c9) (13 min)
1. [Either and And and pairs](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a2cf8bd9-109b-43d9-ae30-ac620091d8bc) (9 min)
1. [Lists revisited](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=985bb5d7-a2a2-4511-a6fb-ac620095003d) (9 min)
1. [Binary trees](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=dbfdfb07-23e8-4b8b-a167-ac6200988381) (12 min)
1. [Directions, addresses and paths in binary trees](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=0904c115-0ad1-486c-945f-ac62009d2772) (15 min)
1. [Traversals in binary trees](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e194464a-dc4d-4fd5-8e03-ac6200a0ae73) (10 min)
1. [Inverting traversals](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e2c84108-de51-42ce-a42b-ac6200bcd280) (18 min)

Total 2hrs.

## Experimenting with the Haskell code included here

You should experiment with the Haskell code in theses notes in order to achieve full understanding. This means running the code and adding things to it, such as
solutions of exercises and puzzles, or your own brilliant ideas.

These lecture notes are in [markdown](https://docs.gitlab.com/ee/user/markdown.html) format including Haskell code. To get the Haskell code out of the markdown code, we can use the program [`mdtohs.hs`](Resources/mdtohs.hs) included in the [Resources](/Resources) directory, as follows in a Unix/Linux terminal:
```
$ cat Data1.md | runhaskell ../../Resources/mdtohs.hs > Data1.hs
```
This means "copy the contents of the file `Data1.md` to the standard input of the Haskell program `mdtohs.hs` and store the output of the program in the file `Data1.hs`". This can be equivalently written as
```
$ runhaskell ../../Resources/mdtohs.hs < Data1.md > Data1.hs
```
This removes all the markdown code and keeps only the Haskell code, so that we can work with it.

We have already run this for you, and the file [Data1.hs](/files/LectureNotes/Sections/Data1.hs) is available in this GitLab repository. Make your own **copy** of this file to avoid conflicts when we update it.

## How to run [Data1.hs](/LectureNotes/Sections/Data1.hs) with `ghci`

The import `System.Random` will fail if we don't specify which package it comes from, which is `random`. You specify this as follows:
```
$ ghci -package random Data1.hs
```

## Haskell imports in these lecture notes

Any needed library imports should be mentioned here at the top of the file. We need the following for generating random inputs for testing:
```haskell
module Data1 where

import System.Random
```

## Contents

* [Type synonyms](#typesynonyms)
* [User defined data types](#datatypes)
  * [The booleans revisited](#booleans)
  * [Type isomorphisms](#typeisos)
  * [Weekdays](#weekdays)

* [Some important type constructors](#logic)
  * [The `Maybe` type constructor](#maybe)
  * [Type retracts](#retracts)
  * [The `Either` type constructor](#either)
  * [The `And` type constructor, defined by ourselves](#and)

* [Lists revisited](#lists)
  * [Implementing some basic operations on lists](#listops)
  * [An aside on accumulators](#accum)

* [Binary trees](#bintrees)
  * [Basic functions on binary trees](#bintreefns)
  * [Directions, addresses and paths in binary trees](#bintreeaddr)
  * [Proofs on binary trees by induction](#bintreepf)
  * [Traversals in binary trees](#traversals)
  * [Inverting traversals (generating trees)](#gentree)

<a name="typesynonyms"></a>
# Type synonyms

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=98b350c3-ec60-47c4-b2fb-ac610127b135).

Sometimes, mainly for the sake of clarity, we may wish to give a new name to an existing type.
For example, the Haskell prelude defines a string to be a list of characters:
```hs
type String = [Char]
```
Since `String` is just a type synonym, operations such as list concatenation and reverse
```hs
(++) :: [a] -> [a] -> [a]
reverse :: [a] -> [a]
```
can be freely applied to strings:
```hs
> "abc" ++ reverse "def"
"abcfed"
```
Type synonyms can also have parameters, as in e.g.
```haskell
type Lst a = [a]
```

<a name="datatypes"></a>
# User defined data types

<a name="booleans"></a>
## The booleans revisited

The booleans are defined as follows in Haskell, in the prelude:
```hs
data Bool = False | True
```
This defines a new type, called `Bool`, with two elements (or *constructors*), called `False` and `True`:
```hs
   False :: Bool
   True  :: Bool
```
Functions over a data type can be conveniently defined by **pattern-matching** on its constructors.
For example, in the prelude, the conjunction operation
```hs
(&&) :: Bool -> Bool -> Bool
```
is defined as follows:
```hs
False && _ = False
True  && x = x
```
A slightly subtle aspect of the semantics of pattern-matching in Haskell is that:

1. the different pattern-matching clauses are tried in order from top to bottom, and
2. the input arguments of the function are only evaluated to the extent needed to check whether they match the current pattern.

A consequence of this semantics is that the above definition of conjunction implements [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation): if the first argument is `False`, then the function returns `False` without even evaluating the second argument.

In contrast, consider the following alternative definition of conjunction:
```haskell
conj :: Bool -> Bool -> Bool
conj False False = False
conj False True  = False
conj True  False = False
conj True  True  = True
```
This version does *not* implement short-circuit evaluation: the second argument will always be evaluated regardless of the value of the first.
We can observe the difference between these two versions of conjunction by running the following experiment in the GHC interpreter:
```hs
> False && undefined
False
> False `conj` undefined
*** Exception: Prelude.undefined
#CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:28:11 in interactive:Ghci5
```

<a name="typeisos"></a>
## Type isomorphisms

Let's introduce another data type `BW` defined by
```haskell
data BW = Black | White
```
This type is *isomorphic* to the type `Bool`, via the type-conversion functions
```haskell
bw2bool :: BW -> Bool
bw2bool Black = False
bw2bool White = True

bool2bw :: Bool -> BW
bool2bw False = Black
bool2bw True  = White
```
That the pair of functions `(bw2bool,bool2bw)` is an isomorphism means that they are mutually inverse, in the sense that
```hs
   bw2bool(bool2bw b) = b
```
for all `b :: Bool`, and
```hs
   bool2bw(bw2bool c) = c
```
for all `c :: BW`.

Type isomorphisms should *not* be confused with type synonyms.
For example, if we try to directly use a value of type `BW` where a value of type `Bool` is expected, we get a type error:
```hs
> let test = Black && True

<interactive>:39:1: error:
    • Couldn't match expected type ‘Bool’ with actual type ‘BW’
    • In the first argument of ‘(&&)’, namely ‘Black’
      In the expression: Black && True
      In an equation for ‘it’: it = Black && True
```
On the other hand, if we wrap up the values using the explicit coercions `bw2bool` and `bool2bw`, then everything checks:
```hs
> let test = bool2bw (bw2bool Black && True)
```

Of course, the names `Black` and `White` are arbitrary, and there is another isomorphism between `BW` and `Bool` that swaps `Black` with `True` and `White` with `False` instead.
```haskell
bw2bool' :: BW -> Bool
bw2bool' Black = True
bw2bool' White = False

bool2bw' :: Bool -> BW
bool2bw' False = White
bool2bw' True  = Black
```
And both of the types `Bool` and `BW` are of course isomorphic (again in two different ways each) to the type
```haskell
data Bit = Zero | One
```
of binary digits. One of the isomorphisms is the following:
```haskell
bit2Bool :: Bit  -> Bool
bool2Bit :: Bool -> Bit

bit2Bool Zero  = False
bit2Bool One   = True

bool2Bit False = Zero
bool2Bit True  = One

```
Another one is the following:
```haskell
bit2Bool' :: Bit  -> Bool
bool2Bit' :: Bool -> Bit

bit2Bool' Zero  = True
bit2Bool' One   = False

bool2Bit' False = One
bool2Bit' True  = Zero
```



> **Note:** The syntax rules of Haskell require that both type names (here `Bool`, `BW`, `Bit`) and constructor names (here `False`, `True`, `Black`, `White`, `Zero`, `One`) should start with a capital letter.

<a name="weekdays"></a>
## Weekdays

Another example of a data type is
```hs
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
```

We can ask Haskell to do some jobs for free for us (there are alternative ways of doing them ourselves with our own sweat, using type class instances, which we will discuss later):

```haskell
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Read, Eq, Ord, Enum)
```
This automatically adds the type `WeekDay` to the type classes with these five names, which give functions
```hs
   show :: WeekDay -> String
   read :: String -> WeekDay
   (==) :: WeekDay -> WeekDay -> Bool
   (<), (>), (<=), (>=) :: WeekDay -> WeekDay -> Bool
   succ, pred :: WeekDay -> WeekDay
```
Look this up in our adopted textbook. Notice that `show` is the counterpart of Java's `toString`, and `read` does the opposite.
Some examples are:
```hs
> show Tue
"Tue"
> read "Tue" :: WeekDay  -- (the type annotation tells Haskell to try to parse the string as a WeekDay)
Tue
> read "Dog" :: WeekDay
*** Exception: Prelude.read: no parse
> Mon == Tue
False
> Mon < Tue
True
> succ Mon
Tue
> pred Tue
Mon
> [Mon .. Fri]
[Mon,Tue,Wed,Thu,Fri]
```

Monday doesn't have a predecessor, and Sunday doesn't have a successor:

```hs
> pred Mon
*** Exception: pred{WeekDay}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at Data1.hs:20:47 in main:Main
> succ Sun
*** Exception: succ{WeekDay}: tried to take `succ' of last tag in enumeration
CallStack (from HasCallStack):
  error, called at Data1.hs:20:47 in main:Main
```

Notice that the choice of names in the type of weekdays is arbitrary. An equally good, isomorphic definition is
```haskell
data WeekDay' = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```

<a name="logic"></a>
# Some important type constructors

<a name="maybe"></a>
## The `Maybe` type constructor

Sometimes a function may not be able to give a result, in which case we would like it to say explicitly that it cannot. We use the `Maybe` type from the prelude for that purpose:
```hs
data Maybe a = Nothing | Just a
```
Here `a` is a type parameter, and we have the following types for `Nothing` and `Just`:
```hs
   Nothing :: Maybe a
   Just    :: a -> Maybe a
```
This means that the constructor `Just` is a function. It [converts](https://en.wikipedia.org/wiki/Type_conversion) an element of type `a` into an element of type `Maybe a`. So this function `Just` is a so-called *type coercion*, also known as a *type cast*.

For example:
```hs
   Just 17 :: Maybe Integer
```
In summary, the only possible elements of the type `Maybe a` are `Nothing` and `Just x` where `x` has type `a`.

In Java the `Maybe` type constructor is called `Optional`.

### Example: integer computations that may give errors

In the following definition of division, if the denominator is zero, then division is impossible, and so the result is `Nothing`. If it is possible, we simply perform the division, and we convert the resulting `Int` to `Maybe Int` using the type conversion function `Just`, which then gives the result of the function `dive`:
```haskell
dive :: Int -> Int -> Maybe Int
x `dive` y = if y == 0 then Nothing else Just (x `div` y)
```
For example, we get
```hs
> 10 `dive` 2
Just 5
> 10 `dive` 0
Nothing
```

But now suppose you want to do ``3 + (10 `dive` 0)``. You would expect `Nothing` but instead this expression doesn't even type check:
```hs
> 3 + (10 `dive` 0)

<interactive>:11:1: error:
    • No instance for (Num (Maybe Int)) arising from a use of ‘+’
    • In the expression: 3 + (5 `dive` 0)
      In an equation for ‘it’: it = 3 + (5 `dive` 0)
```
What this is saying is that ``(10 `div` 0)`` is expected to be an `Int`, because `+` expects an `Int` as its right argument, but it isn't. That's because it is a `Maybe Int`. So we need a version of addition that can cope with errors as its possible inputs:

```haskell
adde :: Maybe Int -> Maybe Int -> Maybe Int
adde Nothing  Nothing  = Nothing
adde Nothing  (Just y) = Nothing
adde (Just x) Nothing  = Nothing
adde (Just x) (Just y) = Just (x + y)
```
Now to fix ``3 + (5 `dive` 0)`` we replace `+` by `adde`, but also we need to convert the number `3` to the type `Maybe Int` with the *type coercion* or *type cast* `Just`.
```hs
> Just 3 `adde` (10 `dive` 2)
Just 8
> Just 3 `adde` (10 `dive` 0)
Nothing
```


A more concise definition of `adde`:

```haskell
adde' :: Maybe Int -> Maybe Int -> Maybe Int
adde' (Just x) (Just y) = Just (x+y)
adde' _       _         = Nothing
```
This works because the execution of Haskell programs tries patterns from top to bottom, and the last pattern catches all remaining possibilities.

A definition using `cases` is also possible:

```haskell
adde'' :: Maybe Int -> Maybe Int -> Maybe Int
adde'' xm ym = case xm of
                Nothing -> Nothing
                Just x  -> case ym of
                            Nothing -> Nothing
                            Just y  -> Just (x+y)
```

Later we will see that there is a much more concise way of making such definitions using *monads*. But for now we will stick to pattern matching and cases.

### Example: find the first position an element occurs in a list

Since the first position is undefined if the element doesn't occur in the list, in that case we answer `Nothing`:
```haskell
firstPosition :: Eq a => a -> [a] -> Maybe Int
firstPosition x []     = Nothing
firstPosition x (y:ys)
           | x == y    = Just 0
           | otherwise = case firstPosition x ys of
                           Nothing -> Nothing
                           Just n  -> Just (n+1)
```
For example:
```hd
> firstPosition 'a' ['a'..'z']
Just 0
> firstPosition 'b' ['a'..'z']
Just 1
> firstPosition 'z' ['a'..'z']
Just 25
> firstPosition '!' ['a'..'z']
Nothing
```
which we summarize as
```hs
    firstPosition 'a' ['a'..'z'] = Just 0
    firstPosition 'b' ['a'..'z'] = Just 1
    firstPosition 'z' ['a'..'z'] = Just 25
    firstPosition '!' ['a'..'z'] = Nothing
```
A precise specification of `firstPosition` is that if `firstPosition x ys = Just n` then `ys !! n = x`, and if `firstPosition x ys = Nothing` then `ys !! i ≠ x for all `i` in the list `[0..length ys-1]`. We can actually use this specification to test our implementation for correctness:
```haskell
testFirstPosition :: Eq a => a -> [a] -> Bool
testFirstPosition x ys =  case firstPosition x ys of
                           Nothing -> and [ ys !! i /= x | i <- [0 .. length ys - 1]]
                           Just n  -> ys !! n == x
```
Here are some tests:
```hs
> testFirstPosition 'a' ['a'..'z']
True
> testFirstPosition 'b' ['a'..'z']
True
> testFirstPosition 'z' ['a'..'z']
True
> testFirstPosition '!' ['a'..'z']
True
```
All tests are successful, and so we get some confidence about the correctness of our implementation. Of course, it is not possible to prove correctness by testing all cases, as there is an infinite amount of them.

You are required to use the book to find out what `case` is and how it works in general, but in this example it should be clear. You are also required to use the book to find out about conditional definitions using `|` to indicate *guards* for equations.

We will use the `Maybe` type constructor very often, because there are many occasions in which some inputs are *invalid*.

*Task*. Define a function `allPositions :: Eq a => a -> [a] -> [Int]` that finds all positions in which an element element occurs in a list.
For example, we should have `allPositions 17 [13,17,17,666] = [1,2]` and `allPositions 17 [1,2,3] = []`.

<a name="retracts"></a>
## Type retracts

A [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=c78bfae6-79d6-4a09-bc70-ac6200c363c9) discussing this section is available.

This section may be rather hard at your current level. Consider it as a challenge. If you get lost, feel free to skip it at a first reading, go to the next section, and come back to it later. This is important because it deals with data coding, which is a crucial, aspect of Computer Science. What we say here is in Haskell, but it actually applies to any programming language.

We have already seen examples of type *isomorphisms*. For example, the type `Bool` is isomorphic to the type `BW` of black-and-white colours, and also to the type `Bit` of binary digits `Zero` and `One`.

More generally, an isomorphism of types `a` and `b` is a pair of functions
```
f :: a -> b
g :: b -> a
```
such that
 * `f (g y) = y` for all `y :: b`, and
 * `g (f x) = x` for all `x :: a`.

We summarize these two equations by saying that these two functions are *mutually inverse*. This means that we can convert back and forth between elements of the type `a` and elements of the type `b`, like we did when we converted `False` to `Zero` and `True` to `One`. You can think of `f` and `g` as *renaming* functions: the function `f = bit2Bool` renames a bit to a boolean, and the function `g = bool2Bit` renames a boolean to a bit.

In practice, this means that it doesn't matter whether we work with
the type `Bool` with elements `True` and `False`, or the type `Bit`
with elements `Zero` and `One`. In fact, computers work by exploting
this identification of booleans with binary digits.

There is another relationship between types that is also useful in
practice: a type `b` can "live" inside another type `a`, in the sense
of having a "copy" in the type `a`. A simple example is that the type
`Bool` has a copy inside the type `Int`:

```haskell
bool2Int :: Bool -> Int
bool2Int False = 0
bool2Int True  = 1
```
Not only do we have a copy of `Bool` inside `Int`, but also we can go back, so that we get `False` and `True` from `0` and `1`:
```haskell
int2Bool :: Int -> Bool
int2Bool n | n == 0    = False
           | otherwise = True
```
However, notice that not only `1` is converted back to `True`, but also everything other than `0` is converted to `True`.

We have
```
   int2Bool (bool2Int y) = y
```
for every `y :: Bool`, but we don't have `bool2Int (int2Bool x) = x` for all `x :: Int`, as this fails for e.g. `x = 17` because `bool2Int (int2Bool 17)` is `1` rather than `17`.

We can say that there is enough room in the type integers for it to
host a copy of the type of booleans, but there isn't enough room in
the type of booleans for it to host a copy of the type of integers.

When there are functions
```
f :: a -> b
g :: b -> a
```
such that
 * `f (g y) = y` for all `y :: b`,

but not necessarily  `g (f x) = x` for all `x :: a`,
we say that the type `b` is a *retract* of the type `a`

Our discussion above shows that the type `Bool` is a retract of the
type `Int`. This retraction is the same as that performed in the
programming language `C`, where the integer `0` codes `False` and
everything else codes `True`.

But notice that there are other ways in which the type `Bool` lives
inside the type `Int` as a retract: for example, we can send `False`
to `13` and `True` to `17`, and then send back everything bigger than
`15` to `True` and everything else to `False`. We are free to code
things as we wish.

**Task**. Show that the type `Maybe a` is a retract of the type
`[a]`. The idea is that `Nothing` corresponds to the empty list `[]`
and that `Just x` corresponds to the one-element list `[x]`. Make this
idea precise by writing back and forth functions between these types
so that they exhibit `Maybe a` as a retract of `[a]`. Our adopted
textbook exploits such a retraction often, albeit without making it
explicit. In fact, what the book does very often is to avoid the
type `Maybe a` and instead work with the type `[a]`, considering only
the list `[]` (corresponding to `Nothing`) and singleton lists `[x]`
(corresponding to `Just x`), and ignoring lists of length `2` or
greater. (The reason the book does that is to avoid monads (before
they are taught) in favour of list comprehensions (which are taught
early on), as list comprehensions happen to accomplish the same thing
as "do notation" for monads, in the particular case of the list
monad. So this coding is done for pedagogical purposes in this case.)

If we have a type retraction `(f,g)` as above, then:
 * `f` is a __surjection__.

    This means that for every `y :: b` there is __at least one__ `x :: a` with `f x = y`.

    For example, in the case of the booleans as a retract of the
    integers, this means that every boolean is coded by at least one integer.


 * `g` is an __injection__.

   This means that for every `x :: a` there is __at most one__ `y :: b` with `g y = x`.

   In the first example of the booleans as a retract of the integers, this is the case:
    * For `x = 0` we have exactly one `y` with `bool2Int y = x`, namely `y=False`.
    * For `x = 1` we have exactly one `y` with `bool2Int y = x`, namely `y=True`.
    * For `x` different from `0` and `1` we have no `y` with `bool2Int y = x`.

   So for every `x` there is at most one such `y` (i.e. exactly one or none).

**Task**. Define
```haskell
data WorkingWeekDay = Mon' | Tue' | Wed' | Thu' | Fri'
```
We add primes to the names because the names without prime are already used as elements of the type `WeekDay` defined above.
Show that the type `WorkingWeekDay` is a retract of the type `WeekDay`. Arbitrary choices will need to be performed in one direction, like e.g. the language `C` arbitrarily decides that any integer other than `0` codes `true`.

**Puzzle**. Consider the function
```hs
g :: Integer -> (Integer -> Bool)
g y = \x -> x == y
```
We can visualize `g` in the following table:

|  | ... | -5 | -4 | ... | -1 | 0 | 1 | ... | 4 | 5 | ... |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| g(-5)= | ... | `True` | False | ... | False | False | False | ... | False | False | ... |
| g(-4)= | ... | False | `True` | ... | False | False | False | ... | False | False | ... |
| ... |
| g(-1)= | ... | False | False | ... | `True` | False | False | ... | False | False | ... |
| g(0)= | ... | False | False | ... | False | `True` | False | ... | False | False | ... |
| g(1)= | ... | False | False | ... | False | False | `True` | ... | False | False | ... |
| ... |
| g(4)= | ... | False | False | ... | False | False | False | ... | `True` | False | ... |
| g(5)= | ... | False | False | ... | False | False | False | ... | False | `True` | ... |

That is, the function `g` codes the integer `y` as the function `h` such that `h y = True` and `h x = False` for `x` other than `y`. Convince yourself that the function `g` is an injection. In this sense, the type `Integer` lives inside the function type `Integer -> Bool`. Do you think `g` has a companion `f : (Integer -> Bool) -> Integer` that "decodes" functions `Integer -> Bool`  back to integers, such that for any code `g y` of the integer `y` we get the integer back as `f (g y) = y`? If yes, then give a Haskell definition of such an `f` and convince yourself that indeed `f (g y) = y` for all integers `y`. If not, why? This puzzle is rather tricky, and none of the possible answers "yes" or "no" to the question is obvious.

<a name="either"></a>
## The `Either` type constructor

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a2cf8bd9-109b-43d9-ae30-ac620091d8bc).

It is defined in the prelude as follows:
```hs
data Either a b = Left a | Right b
```
Then we have
```hs
    Left  :: a -> Either a b
    Right :: b -> Either a b
```
For example:
```hs
    Left 17     :: Either Integer String
    Right "abd" :: Either Integer String
```
The idea is that the type `Either a b` is the *disjoint union* of the types `a` and `b`, where we tag the elements of `a` with `Left` and those of `b` with `Right` in the union type. An example of its use is given below.

<a name="and"></a>
## The `And` type constructor, defined by ourselves

The following has an isomorphic version predefined in the language, as we shall see soon:
```haskell
data And a b = Both a b
```
This is a type constructor with two parameters, and with an element constructor `Both`, which is a function
```hs
   Both :: a -> b -> And a b
```

For example, assuming we have defined types `MainDish`, `Dessert`, `Drink`,
```haskell
data MainDish = Chicken | Pasta | Vegetarian
data Dessert = Cake | IceCream | Fruit
data Drink = Tea | Coffee | Beer
```
we can define:
```haskell
type SaverMenu = Either (And MainDish Dessert) (And MainDish Drink)
```
which can be equivalently written
```hs
type SaverMenu = Either (MainDish `And` Dessert) (MainDish `And` Drink)
```
(Choose which form of definition you like better. Haskell accepts both.)

So what is available in the saver menu is either a main dish and a dessert, or else a main dish and a drink. It should be intuitively clear that this is isomorphic to
```haskell
type SaverMenu' = And MainDish (Either Dessert Drink)
```
meaning that you can have a main dish and either dessert or a drink. This intuition is made precise by the isomorphism
```haskell
prime :: SaverMenu -> SaverMenu'
prime (Left (Both m d)) = Both m (Left  d)
prime (Right(Both m d)) = Both m (Right d)

unprime :: SaverMenu' -> SaverMenu
unprime (Both m (Left  d)) = Left (Both m d)
unprime (Both m (Right d)) = Right(Both m d)
```
So, as a software developer, you can choose either `SaverMenu` as your implementation, or else `SaverMenu'`. They are different, but essentially equivalent.

We actually don't need to define `And`, because an equivalent type constructor is already available in Haskell, namely the type of pairs. We have an isomorphism as follows:
```haskell
and2pair :: And a b -> (a,b)
and2pair (Both x y) = (x,y)

pair2and :: (a,b) -> And a b
pair2and (x,y) = Both x y
```
And so we have further isomorphic versions of the saver menu type:
```haskell
type SaverMenu''  = Either (MainDish, Dessert) (MainDish, Drink)
type SaverMenu''' = (MainDish, Either Dessert Drink)
```
Lookup the type of pairs (tuple types) in the book and read about it.

<a name="lists"></a>
# Lists revisited

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=985bb5d7-a2a2-4511-a6fb-ac620095003d).

With a pinch of salt, the type of lists is predefined by
```hs
data [a] = [] | a : [a]  -- not quite a Haskell definition
```
which says that a list of `a`'s is either empty, or else an element of the type `a` followed (indicated by `:`) by a list of `a`'s.
This is an example of a *recursive* data type definition. We have the following types for the list constructors:
```hs
    []  :: [a]
    (:) :: a -> [a] -> [a]
```

Although the above not-quite-a-Haskell-definition is semantically correct, it is syntactically wrong, because Haskell (unfortunately) doesn't accept this kind of syntactical definition.
If we don't care about syntax, we can define an isomorphic version as follows:
```haskell
data List a = Nil | Cons a (List a)
```
Then the types of the constructors are
```hs
   Nil  :: List a
   Cons :: a -> List a -> List a
```
For example, the native list `[1,2,3]` is written `Cons 1 (Cons 2 (Cons 3 Nil))` in our isomorphic version of the type of lists. Let's define the isomorphism to make this clear:
```haskell
nativelist2ourlist :: [a] -> List a
nativelist2ourlist []     = Nil
nativelist2ourlist (x:xs) = Cons x (nativelist2ourlist xs)

ourlist2nativelist :: List a -> [a]
ourlist2nativelist Nil         = []
ourlist2nativelist (Cons x xs) = x:ourlist2nativelist xs
```
Notice that these coercions are defined recursively, corresponding to the fact that the data type itself is defined recursively.

<a name="listops"></a>
## Implementing some basic operations on lists

Let's write our own versions of the list concatenation ("append") and reverse operations from the prelude:

```haskell
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

rev :: List a -> List a
rev Nil         = Nil
rev (Cons x xs) = rev xs `append` (Cons x Nil)
```

We can try to test that these do the right thing by comparing them to the implementations of list concatenation and reversal in the Haskell prelude, using the isomorphism between `List a` and `[a]`.
Indeed, we expect that
```hs
ourlist2nativelist (append (nativelist2ourlist xs) (nativelist2ourlist ys)) == xs ++ ys
```
and
```hs
ourlist2nativelist (rev (nativelist2ourlist xs)) == reverse xs
```
should evaluate to `True` for all native lists `xs, ys :: [a]`.
Let's test these properties:
```hs
> let xs = [1..5]
> let ys = [6..10]
> ourlist2nativelist (append (nativelist2ourlist xs) (nativelist2ourlist ys)) == xs ++ ys
True
> ourlist2nativelist (rev (nativelist2ourlist xs)) == reverse xs
True
```
Of course, here we have only tested on a couple examples, but it is true in general. (Question: how would you *prove* this?)

Although our definitions are functionally correct, there is a more subtle problem with our implementation of `rev`.
By inspection of the code, `append xs ys` computes the concatenation of two lists in time O(n), where n is the length of `xs`, since each recursive call to `append` decreases the length of `xs` by one, and the calls to `Cons` are constant time.
On the other hand, `rev` is O(n²) by the same argument, since each recursive call to `rev` decreases the length of `xs` by one, and each call to `append` is O(n).

This is not just a theoretical problem &mdash; we quickly bump into it if we compare reversing a reasonably large list using the native `reverse` function versus the implementation `rev` above.
```hs
> let xs = [1..10^5]
> length (reverse xs)  -- this is fast (we return the length of the reversed list in order to keep the output small)
100000
> length (ourlist2nativelist (rev (nativelist2ourlist xs)))  -- this is really slow, so we give up and hit Control-C
  C-c C-cInterrupted.
```
There's a much more efficient way of implementing reversal by introducing a helper function with an extra argument:
```haskell
fastrev :: List a -> List a
fastrev xs = revapp xs Nil
  where
    revapp :: List a -> List a -> List a
    revapp (Cons x xs) ys = revapp xs (Cons x ys)
    revapp Nil         ys = ys
```
One way to think of the second argument of the helper function `revapp` is as a stack, initially set to be empty (`Nil`).
The function recursively scans the input from the first argument, pushing each element onto the stack (second argument).
When there are no more input elements, the stack is simply popped directly to the output, with all of the elements of the original list now in reverse order.

Here's a concrete illustration of how this works, unrolling the definitions of `fastrev` and `revapp` to reverse a four-element list:

```hs
  fastrev (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
= revapp (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) Nil
= revapp (Cons 2 (Cons 3 (Cons 4 Nil))) (Cons 1 Nil)
= revapp (Cons 3 (Cons 4 Nil)) (Cons 2 (Cons 1 Nil))
= revapp (Cons 4 Nil) (Cons 3 (Cons 2 (Cons 1 Nil)))
= revapp Nil (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
= Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))
```

Another way of thinking of the function `revapp` is suggested by its name: given two lists `xs` and `ys`, we have that `revapp xs ys` computes the reversal of `xs` *appended* with `ys`.
It's not hard to see that this binary operation `revapp` is *more general* than the original unary reversal operation: the latter can be recovered by taking `ys = Nil`.
On the other hand, `revapp` is much more efficient than our original function `rev`, being only O(n) in the length of its first argument `xs`.

This pattern &mdash; where we manage to solve a problem or solve it more efficiently by replacing it with a more general and seemingly more difficult problem &mdash; happens again and again in functional programming.

<a name="accum"></a>
## An aside on accumulators

The extra argument `ys` that we used in the helper function `revapp` is sometimes called an "accumulator", since it accumulates a value that is eventually passed to the output.
Above we saw how an accumulator could be used to turn an O(n²) algorithm into an O(n) algorithm for list reversal.
For an even starker example, consider the problem of computing the [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number) Fₙ.

The mathematical definition of the Fibonacci sequence in Wikipedia may be translated directly into the following Haskell code:

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

But although this definition is correct, it is extremely inefficient!

We can already see this if we try to use the above definition to compute, say, the first 32 Fibonacci numbers:

```hs
> :set +s -- ask ghci to print time and space usage
> [fib n | n <- [0..31]]
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269]
(10.23 secs, 4,086,282,024 bytes)
```

Over ten seconds to compute 32 Fibonacci numbers!
Indeed, the running time of `fib n` is roughly O(2ⁿ), since the recursive case makes two calls to `fib` while only decreasing `n` by 1 or 2.

Here is an alternative, much more efficient implementation using a pair of accumulators `x` and `y`:

```haskell
fastfib n = fibAcc n 0 1
  where
    fibAcc 0 x y = x
    fibAcc 1 x y = y
    fibAcc n x y = fibAcc (n-1) y (x+y)
```

With this implementation we have no trouble computing, say, the first 100 Fibonacci numbers in a fraction of a second:

```hs
> [fastfib n | n <- [0..99]]
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497,160500643816367088,259695496911122585,420196140727489673,679891637638612258,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026]
(0.02 secs, 3,057,552 bytes)
```

Again to see what is going on it is helpful to unroll the definitions on a concrete example:

```hs
  fastfib 7
= fibAcc 7 0 1
= fibAcc 6 1 1
= fibAcc 5 1 2
= fibAcc 4 2 3
= fibAcc 3 3 5
= fibAcc 2 5 8
= fibAcc 1 8 13
= 13
```

We can see that this functional implementation of the Fibonacci numbers using a pair of accumulator arguments `x` and `y` works very similarly to the way one might compute the Fibonacci numbers in Java, by updating a pair of variables `x` and `y` inside a loop:

```java
static int fastfib(int n) {
  int x = 0, y = 1;
  while (n > 1) {
     int z = x+y;
     x = y;
     y = z;
     n = n-1;
  }
  return (n == 0 ? x : y);
}
```

In addition to this low-level view of what `fastfib` and in turn `fibAcc` is doing, there is also a more high-level view (as we saw before in the case of `revapp`).
Can you identify a sense in which the helper function `fibAcc n x y` computes a *more general* function than `fib n`?

<a name="bintrees"></a>
# Binary trees

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=dbfdfb07-23e8-4b8b-a167-ac6200988381) .


A binary tree over elements of a type `a` is either empty, or consists of a root labelled by an element of the type `a` followed by two binary trees, called the left and right subtrees:
```hs
data BT a = Empty | Fork a (BT a) (BT a)
```
* We have the empty tree, to be called `Empty`.
  We take the convention that empty trees are drawn as dangling leaves.
  ```
                              |
  ```


* Given two trees `l` and `r` and an element `x::a`, we have the new tree
  ```
                              x
                             / \
                            /   \
                           l     r
  ```
  written `Fork x l r`.

For example, the tree
```
                             8
                            / \
                           /   \
                          4     16
                         / \   / \
                        2        20
                       / \      /  \
```
is written, in this notation, as
```haskell
btexample = Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) (Fork 16 Empty (Fork 20 Empty Empty))
```

We can ask Haskell to do some work for us by deriving things as above.

```haskell
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)
```

We have that
```hs
   Empty :: BT a
   Fork  :: a -> BT a -> BT a -> BT a
```

**Puzzle**. It should be clear what the automatically derived `Show`, `Read` and `Eq` do. But what do you think the order on trees derived with `Ord` should be? *Hint.* This is a non-trivial question. So examine it first for the type of lists. In that case, the automatically derived order is the [lexicographic order](https://en.wikipedia.org/wiki/Lexicographic_order), which is like the dictionary order.


<a name="bintreefns"></a>
## Basic functions on binary trees

To get started, let's mirror trees, so that e.g. from the above we get
```
                             8
                            / \
                           /   \
                          16    4
                         / \   / \
                        20        2
                       / \       / \
```
We do this as follows:
```haskell
mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)
```
Running this on the above example we get
```hs
    mirror btexample = Fork 8 (Fork 16 (Fork 20 Empty Empty) Empty) (Fork 4 Empty (Fork 2 Empty Empty))
```
This notation for trees is not very good for visualizing them, as you
can see, but is very good for computation.

We define the *size* of a tree as its total number of nodes:
```haskell
size :: BT a -> Integer
size Empty        = 0
size (Fork x l r) = 1 + size l + size r
```
Since we are considering binary trees, the size (i.e., the number of nodes) is also equal to the number of leaves minus one:
```haskell
leaves :: BT a -> Integer
leaves Empty        = 1
leaves (Fork x l r) = leaves l + leaves r
```
We define the *height* of a tree to be the length of the longest path from the root, measured in number of nodes:
```haskell
height :: BT a -> Integer
height Empty        = 0
height (Fork x l r) = 1 + max (height l) (height r)
```
A balanced binary tree has height approximately log of its size, whereas a binary tree which is very unbalanced, such as
```
                            20
                           / \
                          16
                         / \
                        8
                       / \
                      4
                     / \
                    2
                   / \
```
```haskell
btleft = Fork 20 (Fork 16 (Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) Empty) Empty) Empty
```
has height approximately equal to its size.

<a name="bintreeaddr"></a>
## Directions, addresses and paths in binary trees

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=0904c115-0ad1-486c-945f-ac62009d2772).


To pick a subtree of a binary tree, we go left or right successively, until
we find it. But a wrong list of directions, called an address here, may be given, and hence we need the `Maybe` type for the output:
```haskell
data Direction = L | R deriving (Show)
type Address   = [Direction]

subtree :: Address -> BT a -> Maybe(BT a)
subtree []     t            = Just t
subtree (_:_)  Empty        = Nothing
subtree (L:ds) (Fork _ l _) = subtree ds l
subtree (R:ds) (Fork _ _ r) = subtree ds r
```
Following the above pattern, we can define a function that checks whether an address in a given tree is valid:
```haskell
isValid :: Address -> BT a -> Bool
isValid []     _            = True
isValid (_:_)  Empty        = False
isValid (L:ds) (Fork _ l _) = isValid ds l
isValid (R:ds) (Fork _ _ r) = isValid ds r
```
The list of valid addresses for subtrees can be computed as follows:
```haskell
validAddresses :: BT a -> [Address]
validAddresses Empty        = [[]]
validAddresses (Fork _ l r) = [[]]
                           ++ [L:ds | ds <- validAddresses l]
                           ++ [R:ds | ds <- validAddresses r]
```
List comprehensions can always be eliminated. In this example they become
```haskell
validAddresses' :: BT a -> [Address]
validAddresses' Empty        = [[]]
validAddresses' (Fork _ l r) = [[]]
                            ++ (map (L:) (validAddresses' l))
                            ++ (map (R:) (validAddresses' r))
```


We expect that
```hs
    isValid ds t = ds `elem` (validAddresses t)
```
Or, in words, an address is valid if and only if it is an element of the list of valid addresses. Should this be intuitively clear? The statement, yes. But the fact, given our definitions, I don't think so. I would say that it requires a convincing argument. In any case, intuition is something we develop based on convincing arguments we learn.

The list of all paths from the root to a leaf has a similar definition:
```haskell
btpaths :: BT a -> [[a]]
btpaths Empty        = [[]]
btpaths (Fork x l r) = [x:xs | xs <- btpaths l]
                    ++ [x:xs | xs <- btpaths r]
```

<a name="bintreepf"></a>
## Proofs on binary trees by induction

If we have a property `P` of trees, and we want to show that
`P(t)` holds for all trees `t`, we can do this by *induction on trees* as follows:
* Argue that `P(Empty)` holds.
* Argue that if `P(l)` and `P(r)` hold for given trees `l` and `r`, then it holds for `P(Fork x l r)` where `x` is arbitrary.

We are not going to emphasize proofs in this module, but we will indicate when some claims genuinely require proofs, and, moreover, we will try to be precise regarding the specifications of the programs we write.

It is often the case that somebody shows us a clever algorithm and we feel stupid because we don't understand it. But this feeling is wrong. If we don't understand an algorithm, what is missing is a proof. A proof is an explanation. This is what proof means. In order to understand an algorithm we need

  * the algorithm itself,
  * a precise description of what it is intended to do, and
  * a convincing explanation that the algorithm does do what it is intended to do.

Programs alone are not sufficient. We need to know what they are intended to accomplish, and we want to know an explanation justifying that they accomplish what we promise. This promise is called the *specification* of the algorithm / program. Program correctness means "the promise is fulfilled". One way to attempt to prove that the promise is fulfilled is to *test* the program. But actually, all that testing can do it to show that the promise is *not* fulfilled, by finding counterexamples. When the good examples work, we have some kind of evidence that the algorithm works, but not full confidence, because we may have missed examples of inputs that give wrong outputs. Full confidence can only be given by a convincing explanation, also known as *proof*. If you ever asked yourself what "proof" really means, the ultimate answer is "convincing argument".

### Functional proofs

The dependently typed language [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) allows to write functional programs *and* their correctness proofs, where the [proofs themselves are written as functional programs](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).
As an example, [here is a computer-checked proof](http://www.cs.bham.ac.uk/~mhe/fp-learning-2017-2018/html/Agda-in-a-Hurry.html) of the above relation between the functions `isValid` and `validAddresses` in Agda.
This is not examinable, and is included here for the sake of illustration only.

<a name="traversals"></a>
## Traversals in binary trees

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e194464a-dc4d-4fd5-8e03-ac6200a0ae73).


We now define the standard in-order and pre-order [traversals](https://en.wikipedia.org/wiki/Tree_traversal).
```haskell
treeInOrder :: BT a -> [a]
treeInOrder Empty = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

treePreOrder :: BT a -> [a]
treePreOrder Empty = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r
```
For instance, for the trees `btexample` and `btleft` considered above,
```
                             8
                            / \
     btexample =           /   \
                          4     16
                         / \   / \
                        2        20
                       / \      /  \
```
```
                            20
                           / \
                          16
                         / \
        btleft =        8
                       / \
                      4
                     / \
                    2
                   / \
```
we get:
```hs
> (treeInOrder btexample, treePreOrder btexample)
([2,4,8,16,20],[8,4,2,16,20])
> (treeInOrder btleft, treePreOrder btleft)
([2,4,8,16,20],[20,16,8,4,2])
```

[Breadth-first traversal](https://en.wikipedia.org/wiki/Breadth-first_search) is trickier. We first define a function that given a tree, produces a lists of lists, with the nodes of level zero (just the root), then the nodes of level one (the successors of the root), then the nodes of level two, and so on:
```haskell
levels :: BT a -> [[a]]
levels Empty        = []
levels (Fork x l r) = [[x]] ++ zipappend (levels l) (levels r)
  where
    zipappend []       yss      = yss
    zipappend xss      []       = xss
    zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss
```
(Compare `zipappend` to the prelude function [zipWith](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#zipWith).)
For example:
```hs
> levels btexample
[[8],[4,16],[2,20]]
> levels btleft
[[20],[16],[8],[4],[2]]
```
With this we can define
```haskell
treeBreadthFirst :: BT a -> [a]
treeBreadthFirst = concat . levels
```
where `.` stands for function composition (look it up in our textbook), and the prelude function `concat :: [[a]] -> [a]` concatenates a list of lists, for example getting `[8,4,16,2,20]` from `[[8],[4,16],[2,20]]`. For further discussion about breadth-first search, see [The under-appreciated unfold](https://dl.acm.org/citation.cfm?doid=289423.289455) (a free version is at the [authors' web page](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/unfold.ps.gz)), but this is probably beyond your current level for most of you.

<a name="gentree"></a>
## Inverting traversals (generating trees)

A video discussing the next few sections is [available on Canvas](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e2c84108-de51-42ce-a42b-ac6200bcd280).

Many different trees can have the same (in-order/pre-order/breadth-first) traversal, as we saw above with `btexample` and `btleft`, which have the same in-order traversal.
In other words, all of the functions
```hs
treeInOrder, treePreOrder, treeBreadthFirst :: BT a -> [a]
```
are *non-injective* and hence non-invertible.
Nonetheless, an interesting and tractable problem is to try to construct a binary tree with a given (in-order/pre-order/breadth-first) traversal, or even to generate *all possible binary trees* with a given traversal.

As an example, the following will produce a *balanced* binary tree given its in-order traversal (which will be a binary *search* tree if the input is sorted):
```haskell
balancedTree :: [a] -> BT a
balancedTree [] = Empty
balancedTree xs = let (ys, x:zs) = splitAt (length xs `div` 2) xs in
                  Fork x (balancedTree ys) (balancedTree zs)
```
(The prelude function [`splitAt`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:splitAt) splits a list in two lists at a given position.)
This satisfies the equation
```hs
    treeInOrder (balancedTree xs) = xs
```
for all `xs :: [a]`.
In the other direction, it is certainly **not** the case that
```hs
    balancedTree (treeInOrder t) = t
```
for all `t :: BT a`, for instance
```hs
balancedTree (treeInOrder btleft) = Fork 8 (Fork 4 (Fork 2 Empty Empty) Empty) (Fork 20 (Fork 16 Empty Empty) Empty)
```
which is not equal to `btleft`.
Indeed, the composite function
```haskell
balance :: BT a -> BT a
balance = balancedTree . treeInOrder
```
which applies `treeInOrder` to a tree followed by `balancedTree` to the resulting list can be seen as an operation for rebalancing a binary tree.

Now, using list comprehensions, it is a small step from the function `balancedTree` above to a function generating *all* binary trees with a given in-order traversal.
```haskell
inOrderTree :: [a] -> [BT a]
inOrderTree [] = [Empty]
inOrderTree xs = [Fork x l r | i <- [0..length xs-1],
                               let (ys, x:zs) = splitAt i xs,
                               l <- inOrderTree ys, r <- inOrderTree zs]
```
This satisfies the property that
```hs
elem t (inOrderTree xs)
```
if and only if
```hs
treeInOrder t = xs
```
for all `t :: BT a` and `xs :: [a]`.
For example, running
```hs
> inOrderTree [1..3]
[Fork 1 Empty (Fork 2 Empty (Fork 3 Empty Empty)),Fork 1 Empty (Fork 3 (Fork 2 Empty Empty) Empty),Fork 2 (Fork 1 Empty Empty) (Fork 3 Empty Empty),Fork 3 (Fork 1 Empty (Fork 2 Empty Empty)) Empty,Fork 3 (Fork 2 (Fork 1 Empty Empty) Empty) Empty]
```
successfully computes all five binary search trees whose in-order traversal is `[1,2,3]`:
```
   1
  / \
     2
    / \
       3
      / \
Fork 1 Empty (Fork 2 Empty (Fork 3 Empty Empty))

   1
  / \
     3
    / \
   2
  / \
Fork 1 Empty (Fork 3 (Fork 2 Empty Empty) Empty)

     2
    / \
   /   \
  1     3
 / \   / \
Fork 2 (Fork 1 Empty Empty) (Fork 3 Empty Empty)

    3
   / \
  1
 / \
    2
   / \
Fork 3 (Fork 1 Empty (Fork 2 Empty Empty)) Empty

      3
     / \
    2
   / \
  1
 / \
Fork 3 (Fork 2 (Fork 1 Empty Empty) Empty) Empty
```

**Task:** write a function `preOrderTree :: [a] -> [BT a]`, with the property that `elem t (preOrderTree xs)` if and only if `treePreOrder t = xs` for all `t :: BT a` and `xs :: [a]`.

**Very hard task:** write a function `breadthFirstTree :: [a] -> [BT a]`, with the property that `elem t (breadthFirstTree xs)` if and only if `treeBreadthFirst t = xs` for all `t :: BT a` and `xs :: [a]`. [solution](https://patternsinfp.wordpress.com/2015/03/05/breadth-first-traversal/))

[1]: https://git.cs.bham.ac.uk/mhe/fp-learning-2021-2022/-/blob/master/Assignments/Formative2/README.md





# Lazy natural numbers

We introduce the lazy natural numbers, with a sample application to
make a certain algorithm faster.

## Motivating example

If the list `xs` is large, the following is slow. Moreover, it loops
without giving an answer if the list `xs` is infinite:
```haskell
checkLengthBiggerThan :: [a] -> Int -> Bool
checkLengthBiggerThan xs n = length xs > n
```
Examples:
```hs
*Main> :set +s
*Main> checkLengthBiggerThan [1..10^6] 3
True
(0.04 secs, 72,067,432 bytes)
*Main> checkLengthBiggerThan [1..10^7] 3
True
(0.19 secs, 720,067,488 bytes)
*Main> checkLengthBiggerThan [1..10^8] 3
True
(1.47 secs, 7,200,067,600 bytes)
*Main> checkLengthBiggerThan [1..10^9] 3
True
(14.35 secs, 72,000,067,640 bytes)
```

We can make this faster as follows, in such a way that it will take at
most `n` steps, regardless of the length of `xs`.
```haskell
checkLengthBiggerThan' :: [a] -> Int -> Bool
checkLengthBiggerThan' []     0 = False
checkLengthBiggerThan' xs     0 = True
checkLengthBiggerThan' []     n = False
checkLengthBiggerThan' (x:xs) n = checkLengthBiggerThan' xs (n-1)
```
We are ignoring negative numbers.

Example:
```hs
*Main> checkLengthBiggerThan' [1..10^9] 3
True
(0.01 secs, 68,408 bytes)
```

## Lazy natural numbers

There is another way to make the above fast, by replacing the use of type `Int` by
the type `Nat` of lazy natural numbers in the original algorithm:

```haskell
data Nat = Zero | Succ Nat deriving (Eq,Ord)
```

The idea is that this represents natural numbers 0,1,2,3,... as follows
```hs
         Zero
         Succ Zero
         Succ (Succ Zero)
         Succ (Succ (Succ Zero))
         ...
```
which we can define in Haskell as
```haskell
one, two, three :: Nat
one   = Succ Zero
two   = Succ one
three = Succ two
```
Moreover, we can convert a non-negative integer to a lazy natural number as follows:
```haskell
toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n-1))
```
But we also have infinity in the type `Nat`:
```haskell
infty = Succ infty
```

This computes for ever, producing an infinite pile ` Succ (Succ (Succ
(Succ ...` of successor constructors, but, crucially, this computation
is lazy.

We can now define a length function as follows:
```haskell
length' :: [a] -> Nat
length' []     = Zero
length' (x:xs) = Succ (length' xs)
```
For example, we have that
   * `length [0..]` loops without giving any answer, but
   * `length' [0..] = infty`.

Now define a lazy comparison algorithm as follows:
```haskell
biggerThan :: Nat -> Nat -> Bool
Zero     `biggerThan` y        = False
(Succ x) `biggerThan` Zero     = True
(Succ x) `biggerThan` (Succ y) = x `biggerThan` y
```

The point is that in the second equation, `x` doesn't need to be
evaluated in order to give the answer `True`. For example:

```hs
*Main> infty `biggerThan` three
True
```

With this, the first algorithm becomes fast, and also works for
infinite lists:

```haskell
checkLengthBiggerThan'' :: [a] -> Int -> Bool
checkLengthBiggerThan'' xs n = (length' xs) `biggerThan` (toNat n)
```
For example:
```hs
*Main> checkLengthBiggerThan'' [1..10^9] 3
True
(0.02 secs, 69,032 bytes)
```

But actually we can write this as follows, because we derived `Ord`:

```haskell
checkLengthBiggerThan''' :: [a] -> Int -> Bool
checkLengthBiggerThan''' xs n = length' xs > toNat n
```

And this is just as fast, which means that the derivation mechanism
for `Ord` must produce a lazy comparison algorithm similar to ours:

```hs
*Main> checkLengthBiggerThan''' [1..10^9] 3
True
(0.01 secs, 70,200 bytes)
```

The point is that the use of the lazy natural numbers makes the natural
algorithm fast, which is slow for integers. Moreover, an advantage of
the use of the lazy natural numbers is that it makes the length of an
infinite list well defined, because of the availability of the lazy
natural number `infty`













