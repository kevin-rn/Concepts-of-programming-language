The “Solution” editor contains hints on how you should define the following functions:

    Ints.asString(Int) : String
    Ints.max(Int,Int):Int
    Ints.min(Int,Int):Int

Write some tests for them, then implement each function. Run your tests using [▶ Your Test]. 
Don’t forget to [▶ Spec-test] before you [Submit].

```scala
// the solution 

object Ints { 

  /*
  * asString(Int) : String
  * @args
  *   one integer
  * @return
  *   the String representation of the argument
  */
  def asString(i: Int) : String = {
    i.toString()
  }
  
  /*
  * max(Int,Int):Int
  * @args
  *   two integers
  * @return
  *   the highest of the arguments
  */
  def max(a: Int, b: Int):Int = {
    if(a > b) return a
    else return b
  }
  
  /*
  * min(Int,Int):Int
  * @args
  *   two integers
  * @return
  *   the lowest of the arguments
  */
  def min(a: Int, b: Int):Int = {
    if(a > b) return b
    else return a
  }

}
```

```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

import Ints._

class Test extends FunSuite {

  test("testZero") {
    assertResult(0) {
      min(0, 0)
    }
  }
}

```
