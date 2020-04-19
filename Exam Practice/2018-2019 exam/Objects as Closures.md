An object encapsulates in a coherent whole a piece of state together with some behavior that relies on that state.

Use recursive functions (rec-lam) to implement an object that encapsulates a counter variable that can be either incremented, decremented, or reset to zero.

The language that you must use to implement this function is a purely functional language with support for strings and string equality checking (string=) but without mutation. The grammar for the language is summarized below.

The solution template contains a skeleton for the object you should implement. The object in the solution template only understands a single message, "get". Modify the solution template to implement an object that understands these messages:

    get gets the value of the encapsulated counter variable.
    inc returns a counter object whose encapsulated variable has been incremented by one.
    dec returns a counter object whose encapsulated variable has been decremented by one.
    reset returns a counter object whose encapsulated variable has been reset to zero.

See the “Test” tab for example programs that should work for your counter object implementation.
Grammar

<expr> ::= <num>
         | (+ <expr> <expr>)
         | (- <expr> <expr>)
         | (* <expr> <expr>)
         | (<expr> <expr>...)
         | <id>
         | (lambda (<id>...) <expr>)
         | (rec-lam <id> (<id>) <expr>)
         | (let ((<id> <expr>) (<id> <expr>)...) <expr>)
         | nil
         | (cons <expr> <expr>)
         | (head <expr>)
         | (tail <expr>)
         | (is-nil <expr>)
         | (is-list <expr>)
         | (if <expr> <expr> <expr>)
         | true
         | false
         | (and <expr> <expr>)
         | (or <expr> <expr>)
         | (not <expr>)
         | (num= <expr> <expr>)
         | (num< <expr> <expr>)
         | (num> <expr> <expr>)
         | <string>
         | (string= <expr> <expr>)

### Template:
```scala
object Solution {
  
  def counter =
    """
      (rec-lam counter (val)
        (lambda (msg)
          (if (string= msg "get")
            val
            "did not understand message")))
    """
  
}

```

### Test:
```scala
//test: Test

// test of the solution

import org.scalatest.FunSuite

import Solution._

import Parser._
import Interp._

class Test extends FunSuite {

  def runWithCounter(s: String) =
    interp(parse(
      s"""
        (let ((counter ($counter 0)))
          $s)
      """), Nil)

  test("Get initial") {
    assertResult(NumV(0)) {
      runWithCounter("""
        (counter "get")
      """)
    }
  }
  
  test("Inc") {
    assertResult(NumV(1)) {
      runWithCounter("""
        ((counter "inc") "get")
      """)
    }
    
    assertResult(NumV(2)) {
      runWithCounter("""
        (((counter "inc") "inc") "get")
      """)
    }
    
    assertResult(StrV("BOOM!")) {
      runWithCounter("""
        ((rec-lam tick (counter)
           (if (num= (counter "get") 13)
             "BOOM!"
             (tick (counter "inc")))) counter)
      """)
    }
  }
  
  test("Dec") {
    assertResult(NumV(-1)) {
      runWithCounter("""
        ((counter "dec") "get")
      """)
    }
    
    assertResult(NumV(-2)) {
      runWithCounter("""
        (((counter "dec") "dec") "get")
      """)
    }
  }
  
  test("Reset") {
    assertResult(NumV(0)) {
      runWithCounter("""
        (((counter "inc") "reset") "get")
      """)
    }
    
    assertResult(NumV(1)) {
      runWithCounter("""
        ((((counter "inc") "reset") "inc") "get")
      """)
    }
  }
  
  test("Nested loops") {
    for (x <- 1 until 10)
    assertResult(NumV(x * x)) {
      runWithCounter(s"""
        ((
          (rec-lam continue (r)
            (lambda (i j)
              (if (num= $x (i "get"))
                (r "get")
                (if (num= $x (j "get"))
                  ((continue r) (i "inc") (j "reset"))
                  ((continue (r "inc")) i (j "inc"))))))
          counter
        ) counter counter)
      """)
    }
  }
  
  test("Lists that count") {
      val listCounter =
        """
        (
          (rec-lam continue (c)
            (lambda (xs)
              (if (is-nil xs)
                (c "get")
                (if (head xs)
                  ((continue (c "inc")) (tail xs))
                  ((continue (c "dec")) (tail xs))))))
          counter
        )
        """
    
    assertResult(NumV(-2)) {
      runWithCounter("(" + 
        listCounter + 
        " (cons true (cons false (cons false (cons false nil)))))")
    }
    
    assertResult(NumV(2)) {
      runWithCounter("(" + 
        listCounter + 
        " (cons true (cons false (cons false (cons true (cons true (cons true nil)))))))")
    }
    
    assertResult(NumV(1)) {
      runWithCounter("(" + 
        listCounter + 
        " (cons true nil))")
    }
    
    assertResult(NumV(0)) {
      runWithCounter("(" + 
        listCounter + 
        " nil)")
    }
  }

}



```

__________________________________________________________________________________________________________________________________


### Solution:
```scala

```
