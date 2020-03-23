Develop a test suite for type checking. You should include:

    Several tests to test good weather cases of type-checking
    Several tests to test bad weather cases of type-checking
    Two test cases to test bad weather cases of interpreting (safeInterp)

An example of testing a good weather case:
```scala
test("Verify correct types") {
  assertResult(NumT()) {
    typeOf("5")
  }
}

test("Verify correct interp behavior") {
  assertResult(NumV(3)) {
    safeInterp("(+ 1 2)")
  }
}
```
Note that safeInterp runs the type-checker before interpreting your code, and may thus throw a TypeException.

Examples of testing a bad weather case:
```scala
test("Catch erroneous types") {
  intercept[TypeException] {
    typeOf("x")
  }
}
```
Now that we have a type-checker, testing for runtime-errors is less essential. Think of what can still go wrong when interpreting programs that pass type-checking. There should not be many of there cases! Therefore, we only require you to have two tests that check for an InterpException:
```scala
test("Catch bad weather case interp") {
  intercept[InterpException] {
    safeInterp("(?????)")
  }
}
```
You will know when you have found these two cases when you no longer get hints about test cases for interpreting.


### Solution:
```scala


```
