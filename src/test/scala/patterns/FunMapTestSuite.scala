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

import patterns.multifun._
/**
 * Tests for the multifun pattern with FunMap
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class FunMapTestSuite extends FunSuite {

  type Operands = (Int, Int)

  trait TestSimpleFunctions {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val funSeq = List[Operands => Double](add, sub, mul, div)
    val funMap = Map[String, Fun[Operands, Double]]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  trait TestMapNoException extends TestSimpleFunctions {
    val multiFunObject = FunMap(funMap)
    def multiFun(in: Operands) = multiFunObject(in)
  }

  trait TestMapWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val funMap = Map[String, Fun[Operands, Double]]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

    val multiFunObject = FunMap(funMap)
    def multiFun(in: Operands) = multiFunObject.apply(in)
  }

  trait TestMapWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val funMap = Map[String, Fun[Operands, Try[Double]]]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

    val multiFunObject = FunMap(funMap)
    def multiFun(in: Operands) = multiFunObject.apply(in)
  }

  trait TestMapWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val funMap = Map[String, Fun[Operands, Future[Double]]]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

    val multiFunObject = FunMap(funMap)
    def multiFun(in: Operands) = multiFunObject.apply(in)

  }

  test("FunMap no ops") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map()

      val noFun = FunMap()
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("FunList factory list") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("F$00" -> 3.0, "F$01" -> -1.0, "F$02" -> 2.0, "F$03" -> 0.5)

      val multiFunObject = FunMap(funSeq)
      def multiFun(in: Operands) = multiFunObject(in)

      val actual = multiFun(input)
      assert(actual === expect)
    }
  }

  test("FunMap normal case") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val actual = multiFun(input)
      assert(actual === expect)
    }
  }

  test("FunMap normal case infinity") {
    new TestMapNoException {

      val input = (1, 0)
      val expect = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)

      val actual = multiFun(input)
      assert(actual === expect)
    }
  }

  test("FunMap Exception thrown") {
    new TestMapWithException {

      val input = (1, 2)
      intercept[Exception] { multiFun(input) }
    }
  }

  test("FunMap with multiple inputs") {
    new TestMapNoException {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val actual = inputs.map(multiFun(_))
      assert(actual === expect)
    }
  }

  test("FunMap with Try Success") {
    new TestMapWithTry {

      val input = (1, 2)
      val expect = Map("add" -> Success(3.0), "sub" -> Success(-1.0), "mul" -> Success(2.0), "div" -> Success(0.5))

      val actual = multiFun(input)
      assert(actual === expect)

    }
  }

  test("FunMap with Try Failure") {
    new TestMapWithTry {

      val input = (1, 0)
      val expect = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> None)

      val actual = multiFun(input).map { case (ns, Success(x)) => (ns, x); case (nf, Failure(e)) => (nf, None) }
      assert(actual === expect)

    }
  }

  test("FunMap with Future") {
    new TestMapWithFutures {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val actual = multiFun(input).map { case (n, x) => (n, Await.result(x, 1 second)) }
      assert(actual === expect)

    }
  }

  test("FunMap composite") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add & sub" -> Map("add" -> 3.0, "sub" -> -1.0), "mul & div" -> Map("mul" -> 2.0, "div" -> 0.5))

      val mop1 = FunMap(Map[String, Fun[Operands, Double]]("add" -> add, "sub" -> sub))
      val mop2 = FunMap(Map[String, Fun[Operands, Double]]("mul" -> mul, "div" -> div))

      val multiFunObject = FunMap(Map("add & sub" -> mop1, "mul & div" -> mop2))

      val actual = multiFunObject(input)
      assert(actual == expect)
    }
  }

  test("FunMap ++ associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0)

      val mop11 = FunMap(Map[String, Fun[Operands, Double]]("add" -> add, "sub" -> sub))
      val mop12 = FunMap(Map[String, Fun[Operands, Double]]("mul" -> mul))

      val mop21 = FunMap(Map[String, Fun[Operands, Double]]("add" -> add))
      val mop22 = FunMap(Map[String, Fun[Operands, Double]]("sub" -> sub, "mul" -> mul))

      val mop1 = mop11 ++ mop12
      val mop2 = mop21 ++ mop22

      assert(mop1(input) === mop2(input))
      assert(mop2(input) == expect)
    }
  }

}