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
 * Tests for the multifun pattern with FunList
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class FunListTestSuite extends FunSuite {

  type Operands = (Int, Int)

  trait TestSimpleFunctions {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val funSeq = List[Operands => Double](add, sub, mul, div)
  }

  trait TestSetNoException extends TestSimpleFunctions {
    val multiFunObject = FunList(funSeq)
    def multiFun(in: Operands) = multiFunObject(in)
  }

  trait TestSetWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception

    val multiFunObject = FunList(add, sub, mul, div)
    def multiFun(in: Operands) = multiFunObject.apply(in)
  }

  trait TestSetWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())

    val multiFunObject = FunList(add, sub, mul, div)
    def multiFun(in: Operands) = multiFunObject.apply(in)
  }

  trait TestSetWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }

    val multiFunObject = FunList(add, sub, mul, div)
    def multiFun(in: Operands) = multiFunObject.apply(in)

  }

  test("FunList no ops") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List()

      val noFun = FunList()
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("FunList normal case") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = multiFun(input)
      assert(actual === expect)
    }
  }

  test("FunList normal case infinity") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)

      val actual = multiFun(input)
      assert(actual === expect)
    }
  }

  test("FunList Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)

      intercept[Exception] { multiFun(input) }
    }
  }

  test("FunList with multiple inputs") {
    new TestSetNoException {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = List(3.0, -1.0, 2.0, 0.5)
      val expect2 = List(1.0, 1.0, 0.0, Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val actual = inputs.map(multiFun(_))
      assert(actual === expect)
    }
  }

  test("FunList with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = multiFun(input).map { _.get }
      assert(actual === expect)

    }
  }

  test("FunList with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)

      val actual = multiFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("FunList with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = multiFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }

  test("FunList composite") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(List(3.0, -1.0), List(2.0, 0.5))

      val mop1 = FunList(add, sub)
      val mop2 = FunList(mul, div)
      def rem(in: Operands): Double = in._1 % in._2.doubleValue()

      val multiFunObject = FunList(List(mop1, mop2))

      val actual = multiFunObject(input)
      assert(actual == expect)
    }
  }

  test("FunList ++ associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mop11 = FunList(add, sub)
      val mop12 = FunList(List[Operands => Double](mul))

      val mop21 = FunList(List[Operands => Double](add))
      val mop22 = FunList(sub, mul)

      val complex1 = mop11 ++ mop12
      val complex2 = mop21 ++ mop22

      assert(complex1(input) === complex2(input))
      assert(complex1(input) == expect)
    }
  }

}