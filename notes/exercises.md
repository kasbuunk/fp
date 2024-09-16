# Exercises

## Chapter 1

T1.1
double (double 2)
= double 2 + double 2
= double 2 + (2 + 2)
= double 2 + 4
= (2 + 2) + 4
= 4 + 4
= 8

W1.3
a) See Spec.
b) See Spec.
c) The expression sum [1..5] can be formulated as such: `sum (numbers 5)`. See Spec.

T1.2
Prove that `sum [x] = x` for any integer `x`.

Proof:

```
sum [x]
= x + sum []
= x + 0
= x

So, sum [x] = x.

QED.
```

## Chapter 2

T2.2
(2^3)*4
(2*3)+(4*5)
2+(3*(4^5))

T2.3
Corrected script:

```haskell
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
```

T2.4
See `last'` in the Playground module.

T2.5
See `init'` in the Chapter2 module.

W2.5
a) The error arises because the length function accepts a list, but is given a function `drop`.
b) The parentheses are not necessary, because drop is a curried function. So after applying it to `2`, it returns a function that accepts the list that follows. Only after that application does it return a list that will be the argument to the length function.

W2.8
a) `take` takes an integer n and list a and returns a list with the first n elements of a.
b) When the first argument to `take` is 0, it will return an empty list whatever the second argument is.

## Chapter 3

See the Chapter3 and Chapter3Spec modules.

T3.5
Functions in general cannot be checked for equality, because of the halting problem. It would lead to a contraction. It may be feasible if the types are known in advance and possibly other constraints are at place to make the functions less powerful.

W3.1

- `7` is an expression and a value of a Num type class.
- `7 + 3` is an expression, not a value, and evaluates to the value 10 of a Num type class.
- `7 + x` is an expression, not a value, that evaluates the value equal to x + 7, which will be of a Num type class. If x is not of a Num type, then a type error will occur during evaluation.
- `double x + 7` is an expression, not a value, and will evaluate to the value equal to x * 2 + 7, which will be of a Num type class.

W3.2
Types play two roles in a functional programming language. They eliminate a class of programming errors, due to type mismatches. As such it constrains variables to be of a certain (intended and meaningful) type in the context of the expression and not of other values that may lead to unexpected results the expressions were not inteded for. And they allow the programmer to be more explicit in intent, so people and the compiler is aided in reasoning about the program.

W3.3

- `div`: integer division of Integral numbers. Takes two Integrals of the same type and returns an Integral of that same type.
- `mod`: gives the modulus of two Integral numbers. Same type as `div`.
- `abs`: takes a Num and returns the absolute value of that Num.
- `negate`: takes an Num and returns the negation of that Num.

For examples on usage of these functions, see the Chapter's Spec.

W3.11

Tuples and lists are both types that represent an ordered collection of elements, but differ as such:

- Tuples have a fixed length of values. All elements of a particular Tuple type have exactly the same amount of values. Lists have a variable, and potentially even infinite, amount of elements.
- The elements in a Tuple may be of different types. The elements of a list must be of the same type.

W3.15

We write `f x y = (f x) y`, because function application is left-associative. This means that if we leave out brackets, we basically mean to first apply the left-most function the the next argument, and the result of that (also a function) will be applied to the right-most argument.


W3.16

a) Yes, `Int -> Int -> Int` and `Int -> (Int -> Int)` can be used for the same function, because they are equivalent. Because of the rule that the function construction operator (the arrow `->`) is right-associative, we can leave out brackets knowing this.

b) No, `(Int -> Int) -> Int` and `Int -> (Int -> Int)` cannot be used to describe the same function. The former describes a function that takes a function (that takes an Int and returns an Int) and returns an Int. The latter takes an Int, and returns a function (that takes an Int and returns an Int).

W3.18

a) See Chapter3Spec

b) The order matters, because we can only choose to curry with the next argument of the function.

W3.20

The function `sum :: [a] -> a` does not cover its intention, because we expect a sum to return a number. It only works for types where a has the Num constraint.

W3.21

Polymorphic functions are important, because it allows functions to be defined generically for various concrete types. It reduces repetition of the same functions for different types that may be of the same form, apart from their type. For example, the `length :: [a] -> Int` function's implementation counts the number of elements in the list of type `a`. But if you were to define it for any type `t`, then the implementation would be exactly the same, because the length of a list is independent of the type or value of its elements.

W3.22

Yes, sum is a polymorphic function, because it uses a type variable `a`. It is not overloaded, because it is defined only once for all different types of `a`. Just because `a` is a `Num`, it is the `+` operator as part of the implementation that will need to be overloaded.

W3.23

The function is defined as:

```haskell
multiply x y = x * y
```

The type inference is left to the compiler. This will look for the two most general types that * is defined for. But there are no two types for which * is defined, so the compiler cannot infer instance `Num Bool`.

W3.24

The function `minimum` determines the smallest value of a non-empty list `[a]`. Presumably, it will need to compare individual elements with each other and determine each time which is the smallest. Hence it makes sense to constrain `a` with the `Ord` type constraint. We then need to define the function such that it retains the lowest value and each time checks if the next value is lower, and if so, retain that whilst iterating through all values in the list, finally returning the lowest retained value. It seems that we can just define that function somewhere with a simple `<=` operator, which will need to be overloaded, but I don't think the `minimum` function itself needs to be defined more than once. Of course, it will be polymorphic, because it is defined for abstract type `a`, which multiple concrete types can be used for.


W3.25

a)

`mod :: Integral a => a -> a -> a`
`div :: Integral a => a -> a -> a`

The functions are defined for `Integral`. That is the type class that encompasses the integers, including the fixed-size `Int` and dynamically sized `Integer`.

b) For performance reasons it makes more sense to define them separately for `Int`. Fixed-size integer values (`Int`) have a much faster implementation than `Integer` types. We cannot mix them, because both types must be the same `a`.

c) The type of the literal `7.3` is `Fractional a => a`.

d) The types that are part of the `Fractional` type class are: `Float` and `Double`.

## Chapter 7

W7.1

a) The function `twice` is a function that takes a function `f :: a -> a` and an argument of the type `a`, and applies `f` to a and once again to its output of type `a`. That's why the output must be of the same type as its input.

b) `twice :: (a -> a) -> a -> a`

c)

```
twice :: (b -> b) -> b -> b
init :: [a] -> [a]
twice init :: [a] -> [a]
```

d) `twice init [1, 2, 3] :: Num a => [a]`
