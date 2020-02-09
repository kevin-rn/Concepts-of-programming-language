This exercise focuses on defining and using a list data structure using case classes.

The “Test” tab contains illustrative test programs that you can use as a guide when implementing your solutions. 
You are encouraged (but not required) to extend this set of tests. What are the corner cases to test for?

```scala
object Solution {

  /**
    * We will now define a slightly less trivial case class,
    * representing a list structure containing integers.
    *
    * Any list can be one of two types:
    * it can be empty, or it can be an element joined to the remaining list.
    *
    * Notice that the structure of these case classes is recursive.
    */
  sealed abstract class IntList
  case class Empty()                        extends IntList // The empty list, often called Nils
  case class Element(n: Int, tail: IntList) extends IntList // Element is usually called Cons

  /**
    * As an example, let's create a function that lists descending integers from n to 1.
    */
  def listFrom(n: Int): IntList = {
    if (n == 0) Empty()
    else Element(n, listFrom(n-1))
  }

  /**
    * EXERCISE:
    * Implement the function sumIntList(xs).
    * It should take an IntList, and return the sum of all it's elements.
    * Use pattern matching!
    */
  def sumIntList(xs: IntList): Int = xs match {
    case Empty() => 0
    case Element(n, t) => n + sumIntList(t)
  }

  /**
    * EXERCISE:
    * Implement the function head(xs).
    * It should return the first element in a list.
    * If the list is empty, throw a NoSuchElementException.
    */
  def head(xs: IntList): Int = xs match {
    case Empty() => throw new NoSuchElementException()
    case Element(n, t) => n
  }

  /**
    * EXERCISE:
    * Define the function tail(xs).
    * It should accept an IntList and return the same IntList, but without the first element.
    * If the list is empty, throw a NoSuchElementException.
    */
  def tail(xs: IntList): IntList = xs match {
    case Empty() => throw new NoSuchElementException()
    case Element(n, t) => t
  }

  /**
    * EXERCISE:
    * Define the function concat(xs, ys).
    * It should concatenate two IntLists.
    */
  def concat(xs: IntList, ys: IntList): IntList = (xs, ys) match {
      case (Empty(), Empty()) => Empty()
      case (Empty(), _) => ys
      case (Element(c, t), _) => Element(c, concat(t, ys))
  }

  /**
    * EXERCISE:
    * Define the function take(n, xs).
    * It should return the first n elements of xs.
    * This function should never throw an exception.
    */
  def take(n: Int, xs: IntList): IntList = xs match {
    case Empty()  => Empty()
    case _ if(n==0) => Empty() 
    case Element(c, t) => Element(c, take(n-1, t))  
  }

  /**
    * EXERCISE:
    * Define the function drop(n, xs).
    * It should return the list xs, without the first n elements.
    * This function should never throw an exception.
    */
  def drop(n: Int, xs: IntList): IntList = xs match {
    case Empty() => Empty()
    case x if(n ==0) => xs
    case Element(c, t) => drop(n-1, t)
  }
}


```

```scala
//test: StudentTest

import org.scalatest.FunSuite
import Solution._

class StudentTest extends FunSuite {


  test("Concat more elements") {
    assertResult(Element(34, Element(3, Element(6, Empty())))) {
      concat(Element(34, Element(3, Empty())), Element(6, Empty()))
    }
  }

  test("Head empty") {
    intercept[NoSuchElementException] {
      head(Empty())
    }
  }

  test("Tail") {
    assertResult(Element(3, Empty())) {
      tail(Element(34, Element(3, Empty())))
    }
  }

  test("take") {
    assertResult(Element(34, Element(3, Element(6, Empty())))) {
      take(10, Element(34, Element(3, Element(6, Empty()))))
    }
  }

  test("drop") {
    assertResult(Element(34, Element(3, Element(6, Empty())))) {
      drop(0, Element(34, Element(3, Element(6, Empty()))))
    }
  }
}


```
