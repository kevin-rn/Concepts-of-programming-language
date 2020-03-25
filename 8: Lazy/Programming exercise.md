Implement four example programs following the directions given in the solution template.
You are given the following set of utility functions.
These utility functions can be used in the context of a letrec.
The functions are:

    a zip-with function;
    a map function;
    a difference function which takes two ordered lists of numbers l1 and l2 as input,
    and returns the list of all numbers that are in l1 but not l2; and
    a take function which takes the first n elements of a list.

Using these utilities, we can define a stream of natural numbers. See nats for an example.

    Write a program that returns a stream of even natural numbers.
    Use the the evens, nats, and diff functions to define a stream of odd natural numbers.
    Define a function multiples that takes a number n as input and returns a stream containing all multiples of n. This stream of multiples should not include zero.
    Define a stream of primes, using the sieve of Eratosthenes.
    Hint: you can use nats, diff, and multiples.

For these programs you will use the language of week 4 extended with lazy evaluation.

### Test
```scala
//test: Test


import org.scalatest.FunSuite
import Interpreter._

class Test extends FunSuite with CatchErrorSuite {

  /**
    * This test demonstrates how to to interpret expressions that contain identifiers.
    *
    * By giving the interp-method a list of (identifier, expression) tuples,
    * these will be bound to the scope of your program
    */
  test("If statement with identifiers") {
    assertResult("1") {
      val bindings = List(("x", "true"),
        ("y", "1"),
        ("z", "2"))
      interp("(force (if x y z))", bindings)
    }
  }

  /**
    * You can test functions by referring to the definitions in the
    * solution templates, as follows:
    */
  test("The first two primes, using Eratosthenes' sieve") {
    assertResult("ConsV(NumV(2),ConsV(NumV(3),NilV()))") {
      interp("""(force (letrec (""" + Solution.util +""") (force (take 2 """ + Solution.eratosthenes + """))))""", List())
    }
  }
  
  test("even") {
    assertResult("ConsV(NumV(0),ConsV(NumV(2),NilV()))") {
      interp("""(force (letrec (""" + Solution.util +""") (force (take 2 """ + Solution.evens + """))))""", List())
    }
  }
  
  test("odds") {
    assertResult("ConsV(NumV(1),ConsV(NumV(3),NilV()))") {
      interp("""(force (letrec (""" + Solution.util +""") (force (take 2 """ + Solution.odds + """))))""", List())
    }
  }
  
  test("multiplies") {
    assertResult("ConsV(NumV(5),ConsV(NumV(10),NilV()))") {
      interp("""(force (letrec (""" + Solution.util +""") (force (take 2 (""" + Solution.multiples + """ 5)))))""", List())
    }
  }

}


```

### Template
```scala
object Solution {

  /**
    * Use these utility functions to solve the assignment.
    */
  def util =
       """
      (zip-with
        (lambda (f xs ys)
          (if (and (is-nil xs) (is-nil ys))
            nil
            (cons
              (f (head xs) (head ys))
              (zip-with f (tail xs) (tail ys))))))
      (map
        (lambda (f xs)
          (if (is-nil xs) nil
            (cons (f (head xs)) (map f (tail xs))))))
      (diff
        (lambda (xs ys)
          (if (is-nil xs)
            nil
            (if (is-nil ys)
              xs
              (if (num< (head xs) (head ys))
                (cons (head xs) (diff (tail xs) ys))
                (if (num= (head xs) (head ys))
                  (diff (tail xs) (tail ys))
                  (diff xs (tail ys))))))))
      (take
        (lambda (n xs)
          (if (num= n 0)
            nil
            (cons (head xs) (take (- n 1) (tail xs))))))
    """

  def nats =
    """
      (letrec ("""+util+"""
               (nats (cons 0 (map (lambda (x) (+ x 1)) nats))))
        nats)
    """

  def evens =
    """
    
  """

  def odds =
    """
   
  """

  def multiples =
    """

  """

  def eratosthenes =
    """

    """

}

```

____________________________________________________________________________________________________________________
### Solution
```scala
object Solution {

  /**
    * Use these utility functions to solve the assignment.
    */
  def util =
       """
      (zip-with
        (lambda (f xs ys)
          (if (and (is-nil xs) (is-nil ys))
            nil
            (cons
              (f (head xs) (head ys))
              (zip-with f (tail xs) (tail ys))))))
      (map
        (lambda (f xs)
          (if (is-nil xs) nil
            (cons (f (head xs)) (map f (tail xs))))))
      (diff
        (lambda (xs ys)
          (if (is-nil xs)
            nil
            (if (is-nil ys)
              xs
              (if (num< (head xs) (head ys))
                (cons (head xs) (diff (tail xs) ys))
                (if (num= (head xs) (head ys))
                  (diff (tail xs) (tail ys))
                  (diff xs (tail ys))))))))
      (take
        (lambda (n xs)
          (if (num= n 0)
            nil
            (cons (head xs) (take (- n 1) (tail xs))))))
    """

  def nats =
    """
      (letrec ("""+util+"""
               (nats (cons 0 (map (lambda (x) (+ x 1)) nats))))
        nats)
    """

  def evens =
    """
    (letrec ("""+ util  +"""
              (even (cons 0 (map (lambda (x) (+ x 2)) even))))
    even)
  """

  def odds =
    """
    (letrec ("""+ util +""" (nats (cons 0 (map (lambda (x) (+ x 1)) nats))) 
              (even (cons 0 (map (lambda (x) (+ x 2)) even))))
      (diff nats even))
  """

  def multiples =
    """
    (lambda (n) (letrec ("""+ util +""" (natsAlt (cons 2 (map (lambda (x) (+ x 1)) natsAlt))) 
              (multiples (cons n (map (lambda (x) (* n x)) natsAlt))))
    multiples))
  """

  def eratosthenes =
    """
      (letrec ("""+ util +""" (natsAlt (cons 2 (map (lambda (x) (+ x 1)) natsAlt)))
              (multiples (lambda (n) (cons n (map (lambda (x) (* n x)) natsAlt))))
              (sieve (lambda (xs) (cons (head xs) (diff (sieve (tail xs)) (multiples (head xs)))))))
            (sieve natsAlt))
    """

}


```
