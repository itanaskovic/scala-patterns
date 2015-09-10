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
 * Tests for the MultiSeqFun pattern
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class MultiSeqFunTestSuite extends FunSuite {

  trait TestSetNoException {
    type Operands = (Int, Int)
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val functions = List[Operands => Double](add, sub, mul, div)

    val complexFun = MultiSeqFun(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)
  }

  trait TestSetWithException {
    type Operands = (Int, Int)
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val functions = List[Operands => Double](add, sub, mul, div)

    val complexFun = MultiSeqFun(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)

  }

  trait TestSetWithTry {
    type Operands = (Int, Int)
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val functions = List[Operands => Try[Double]](add, sub, mul, div)

    val complexFun = MultiSeqFun(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)

  }
  trait TestSetWithFutures {
    type Operands = (Int, Int)
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val functions = List[Operands => Future[Double]](add, sub, mul, div)

    val complexFun = MultiSeqFun(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)

  }

  test("MultiSeqFun normal case") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiSeqFun normal case infinitty") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)
      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiSeqFun Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      intercept[Exception] { testComplexFun(input) }
    }
  }

  test("MultiSeqFun with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input).map { _.get }
      assert(actual === expect)

    }
  }

  test("MultiSeqFun with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)
      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("MultiSeqFun with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }

}