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
