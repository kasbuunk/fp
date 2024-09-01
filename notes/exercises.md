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
