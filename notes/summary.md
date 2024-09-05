# Summary

## Chapter 1

### 1.2: Functional programming

Functional programming is a style of programming in which the basic method of computation is application of functions to arguments.

A functional programming language is one that supports and encourages the functional style.

The difference between a mutable variable in imperative languages and a mathematical variable is that the former can have its value changed during the execution of the program, whilst the latter remains constant once it has been bound at the function application.

### Chapter 3

A type is a collection of related values. We write `v :: T` to mean `v` is a value in the type `T`.

Type inference takes place before evaluation of an expression. All expressions have a type.

The basic data types are:

- Bool
- Char
- String
- Int
- Integer
- Float
- Double

The list type is an ordered collection of values of the same type, without a fixed size. It may even be inifinite.

The tuple type is an ordered set of values of potentially different types. Tuples with zero, two or more elements are allowed. One element is not allowed, due to the role of parentheses in expressions.

The function is a type of its own. It maps arguments from one type to results of another.

An expression is inductively defined as such:

- a value is an expression (atom or base case)
- a function (or operator) applied to a value

Expressions can be evaluated to values. If the expression is atomic, i.e. a value like `6`, then evaluation yields the same value.

An if-then-else clause is also an expression, but really just a ternary operator. So the syntax is different from other operators, but it really is a function with three arguments: `ifthenelse :: Bool -> a` where `a` is the type of then and else. Both branches must be of the same type.

### Chapter 5

The expression `x <- [1..5]` is a generator that is read as "the list of numbers x squared such that x is drawn from the list of numbers 1 to 5".
