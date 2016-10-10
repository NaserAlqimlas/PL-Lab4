import org.scalatest._
import jsy.tester.JavascriptyTester
import jsy.lab4.ast._
import jsy.lab4.Parser.parse

import jsy.student.Lab4
import Lab4._

class HigherOrderFunctionsExercisesSpec extends FlatSpec {

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst { (i: Int) => if (i < 0) Some(-i) else None } (l1)
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  } 
}

class Lab4InterpreterSpec extends FlatSpec {

  val xtype = TNumber
  val tenvx = extend(emp, "x", xtype)

  "TypeVar" should "perform TypeVar" in {
    assertResult(xtype) {
      typeInfer(tenvx, Var("x"))
    }
  }

  // Probably want to write some more tests for typeInfer, substitute, and step.

}

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", Lab4)

class Lab4Suite extends Suites(
  new HigherOrderFunctionsExercisesSpec,
  new Lab4InterpreterSpec,
  new Lab4JsyTests
)