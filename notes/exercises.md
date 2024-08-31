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
