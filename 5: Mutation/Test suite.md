Develop a test suite for mutation. Include:

    Several tests to test good weather cases of interpreting mutation
    Several tests to test bad weather cases of interpreting mutation

For robustness, the interp function “hides” the output store of the interpreter. This way, tests do not depend on how the store is laid out.

An example of testing a good weather case:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp("5")
  }
}

or with pre-defined bindings in scope:

test("Verify correct implementation") {
  assertResult(NumV(5)) {
    interp(desugar(parse("x")), List(Bind("x", NumV(5))))
  }
}

Examples of testing a bad weather case:

test("Catch bad weather case parse") {
  intercept[ParseException] {
    parse("()")
  }
}

test("Catch bad weather case interp") {
  intercept[InterpException] {
    interp("(+ true 5)")
  }
}

```scala


```
