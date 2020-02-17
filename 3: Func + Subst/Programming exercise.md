Example Programs

Implement the four programs described below. The programs are interpreted by an interpreter of week 3.
1. Conditional Expression

Make an expression with a free identifier x that does the following: When x + 5 equals 13, return 0, otherwise return 1.
2. Curried cons

Make an expression that returns a function implementing a curried version of cons.
3. Recursive Factorial Function

Make an expression that returns a recursive function that computes the factorial of its first argument.
4. Capture Avoidance

Define an expression with a free identifier that could violate name capture avoidance. Your expression should crash with a capture-avoiding interpreter, and return a result when run with a wrong interpreter.
Recursion Hint

You can make functions recur by passing a function its own definition as an extra argument. For example, the following program calls itself recursively 10 times before terminating:
```scala
(let ((f (lambda (self x) 
           (if (num= x 0)
             0
             (+ 1 (self self (- x 1)))
           )
         )))
  (f f 10))
```


```scala
object Solution {

  /** 1. Conditional Expression */
  def numerical = "(if (num= (+ x 5) 13) 0 1)"

  /** 2. Curried `cons` */
  def curriedCons = "(lambda (y) (lambda (x)(cons y x)))"

  /** 3. Recursive Factorial Function */
  def fact = "(lambda (x) (let ((f (lambda (self x) (if (num= x 0) 1 (* x (self self (- x 1))) ) )))(f f x)))"

  /** 4. Capture Avoidance */
  def capturing = "(lambda (x) (+ 1 y y))"

}


```
