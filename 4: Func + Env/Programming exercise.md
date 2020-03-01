Example Programs

Write four programs in the language that you are interpreting. The programs are interpreted by an interpreter of week 4.

def sample =
        """
          (- x
            (+ 3 5)
          )
        """
1. Define an expression that fails under interpretation with static scope, but succeeds under interpretation with dynamic scope.
2. Implement an eager equivalent of the Y combinator (sometimes known as the Z combinator). See, e.g., https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
3. Use your Y combinator to define an expression that returns the factorial function.
4. Use your Y combinator to define an expression that returns the Fibonacci function.

```scala
object Solution {

  /**
    * Define an expression that fails under interpretation with static scope,
    * but succeeds under interpretation with dynamic scope.
    */
  def scope = "(let ((f1 (lambda (x) (f2 4))) (f2 (lambda (y) (+ x y)))) (f1 3))"

  /**
    * Implement an eager equivalent of the Y combinator (somtimes known as the Z combinator).
    * See, e.g.,
    * https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus
    */
  def Y = "(lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))"

  /**
    * Use your Y combinator to define and return the factorial function as a higher-order function.
    */
  def fact = "(" + Y + "(lambda (fact) (lambda (n) (if (num= n 1) 1 (* n (fact (- n 1)))))))"
  // Using rec-lam: def fact = "(rec-lam fact(n) (if (num= n 1) 1 (* n (fact(- n 1)))))"
  
  /**
    * Use your Y combinator to define and return the fibonacci function as a higher-order function.
    */
  
  def fib = "(" + Y + "(lambda (fib) (lambda (n) (if (num< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))"
  // Using rec-lam: def fib = "(rec-lam fib (n) (if (num< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"

}
```
