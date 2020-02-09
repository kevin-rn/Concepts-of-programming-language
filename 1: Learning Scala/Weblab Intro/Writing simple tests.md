Writing tests

Writing software can be frustrating because errors can come up anywhere. It’s not because the assignments are hard, it’s just that we forget what the behavior should be in special cases. To make our collective lives easier (and programming less frustrating) we write tests. There are different types of tests:

    Unit tests
    Integration tests

By ‘unit’ we mean the smallest testable piece of functionality. You can imagine each unit as a building block in the software. With unit testing we test each building block independently against a variety of inputs. For example if we were making a pocket calculator program we would have units tests that exercised addition, subtraction, multiplication, and division separately.
Assignment

Say we want to write a function that does division of two integers. We’ve made a skeleton for this the “Solution” editor. Before you start implementing it, just go to the “Test” editor and write some tests for our Calc.div(Int,Int) method. Look through the example there. The first test there reads: “When I call div(2,2) I expect 1”. The second test reads: “When I call div(2,0) I expect an ArithmeticException to be thrown”.

Your assignment is to write tests for normal and special cases and create an implementation of Calc.div(Int,Int) that passes those tests. You’re done when all of our Specification tests pass. Don’t forget to [Submit] your solution when you are done.
A note on problem solving and method

Whenever you need to solve a programming problem, try to follow this method: first write down some tests that a solution must pass, and subsequently implement your program. By running your program on your test programs, you will see which tests fail, which will help you figure out what is wrong with your solution. For big programming tasks, think about how you can factor the task into sub-tasks, and follow the same method: write tests that must pass for a correct solution to the sub-task, then implement your program addressing the sub-task, and run the tests. 
Only proceed to consider the next sub-tasks once you’ve got the previous task solved.

```scala
// the solution 

object Calc { 

  def div(div : Int, by: Int) : Int = {
    // your code here
    if(by == 0) throw new ArithmeticException()
    div/by
  }
  
}
```

```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

import Calc._

class Test extends FunSuite {

  test("testDiv1") {
    assertResult(1) {
      div(2, 2)
    }
  }

  test("testDivideByZero") {
    intercept[ArithmeticException] {
      div(2, 0)
    }
  }

}


```
