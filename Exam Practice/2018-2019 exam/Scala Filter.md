Write a function in Scala which takes two integer lists xs and ys of equal length, and does the following:

    Pointwise subtract xs and ys. For example, the result of pointwise subtracting List(4, 3, 6) and List(1, 5, 2) is List(3, -2, 4).

    For the list resulting from step 1, filter all elements less than 0. For example, the result of filtering the List(3, -2, 4) is List(3, 4).

    For the list resulting from step 2, compute the sum of all elements in the list. For example, the sum of the elements in List(3, 4) is 7.

You are encouraged to use the list functions from the Scala Standard Library (but you are not required to).

### Template:
```scala
object Solution {
  
  def subFilterSum(xs: List[Int], ys: List[Int]): Int = ???
  
}

case class UnequalLengths() extends RuntimeException

```

### Test:
```scala
//test: Test

import org.scalatest.FunSuite

import Solution._

class Test extends FunSuite {
  
  test("List(4, 3, 6) and List(1, 5, 2)") {
    assertResult(7) {
      subFilterSum(List(4, 3, 6), List(1, 5, 2))
    }
  }
  
}

```

__________________________________________________________________________________________________________________________________


### Solution:
```scala
object Solution {
  
  def subFilterSum(xs: List[Int], ys: List[Int]): Int = {
    if(xs.size != ys.size) throw UnequalLengths()
    xs.zip(ys).map{ case (x, y) => x-y}.filter(_ >= 0).sum
  }
  
}

case class UnequalLengths() extends RuntimeException

```
