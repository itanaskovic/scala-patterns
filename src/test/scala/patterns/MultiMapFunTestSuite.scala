package patterns

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.async.Async.{ async, await }
import scala.util.{ Try, Success, Failure }

/**
 * Tests for the MultiMapFun class
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class MultiNamedFunTestSuite extends FunSuite {

  trait TestSetNoException {
    type Operands = (Int, Int)
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val functions = List[Operands => Double](add, sub, mul, div)
    val functionsMap = Map[String, Operands => Double]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  test("MultiMapFun factory with all defaultNames") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("fun_ 0" -> 3.0, "fun_ 1" -> -1.0, "fun_ 2" -> 2.0, "fun_ 3" -> 0.5)

      val complexFun = MultiMapFun(functions)
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiMapFun factory with some defaultNames") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "fun_ 1" -> -1.0, "fun_ 2" -> 2.0, "fun_ 3" -> 0.5)

      val complexFun = MultiMapFun(functions, List("add"))
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiMapFun factory with more names than methods") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = MultiMapFun(functions, List("add", "sub", "mul", "div", "log", "sqr"))
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiMapFun factory with no defaultNames") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = MultiMapFun(functions, List("add", "sub", "mul", "div"))
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiMapFun factory with map and single input") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = MultiMapFun(functionsMap)
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiMapFun factory with map and multiple inputs") {
    new TestSetNoException {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val complexFun = MultiMapFun(functionsMap)
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = inputs.map(testComplexFun(_))
      assert(actual === expect)
    }
  }

  test("MultiMapFun constructor exception ") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      intercept[IllegalArgumentException] { new MultiMapFun(functions, List("add", "sub")) }
    }
  }

}