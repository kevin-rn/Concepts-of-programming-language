For most assignments we will provide you with a solution skeleton that you have to modify as part of the assignment. 
To the right of this text you can see a text box containing such a solution skeleton. 
That text box is actually an editor. 
When you answer questions you will have to place your solution in the editors.

At the top right of the page there is a pencil icon. 
You should click that now and it will take you to your submission page for the current assignment.

To the right of this text you have an editor with two tabs:

    Solution
    Test

Go to the “Solution” tab for the rest of these instructions.

```scala
/*
Welcome to the editor where you will provide your solution
code to assignments. You can create objects and classes with
methods in this editor. We will normally give you a solution
skeleton such as the one below. Go ahead and press the
[Save] button.

It looks like there is a compilation error: we've forgotten to
close the string. Go ahead and correct the error then [Save] again.
Now that the compilation is successful you see two buttons:
* [▶ Your Test]
* [▶ Spec-test]

Whenever you run [▶ Your Test] we will run your tests against your _own_ set of tests.
You can see and modify these tests under the "Test" tab.
If you run [▶ Spec-test] we will test your solution against _our tests_.
Go ahead and run [▶ Your Test].


It looks like one of your own tests is failing.
Switch to the "Test" editor and try to fix the failing test.

*/

object Speak {

  def greet() = {
    "Hello world"
  }

}
```

```Scala
//test: Test

/*
This is the editor where you can write tests for your own code.
The test below is currently incorrect. Correct the expected value to the one
the Speak.greet() function actually returns, then [Save] then
[▶ Your Test]. Your test now passes.

Whenever you think your solution is complete you can [▶ Spec-test].
This will run your solution against our tests. Go ahead. You can now see that
your code passed all tests. 

After running [▶ Spec-test] you can [Submit] your solution for us to review.
Go ahead and [Submit] your solution if all the tests have succeeded and I'll
see you at the next assignment. Remember that you cannot [Submit] a solution 
that fails [Save].

*/

import org.scalatest.FunSuite

import Speak._

class Test extends FunSuite {

  test("Test hello world") {
    assertResult("Hello world") {
      greet()
    }
  }

}
```
