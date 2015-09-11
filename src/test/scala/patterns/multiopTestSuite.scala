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

import patterns.multiop._
/**
 * Tests for the MultiOp pattern
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class multiopTestSuite extends FunSuite {

  type Operands = (Int, Int)

  trait TestSimpleFunctions {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val ops = List[Operands => Double](add, sub, mul, div)
    val opsMap = Map[String, Operands => Double]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  trait TestSetNoException extends TestSimpleFunctions {
    val mop = MultiOp(ops)
    def testComplexFun(in: Operands) = mop(in)
  }

  trait TestSetWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val ops = List[Operands => Double](add, sub, mul, div)

    val mop = MultiOp(ops)
    def testComplexFun(in: Operands) = mop.apply(in)
  }

  trait TestSetWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val ops = List[Operands => Try[Double]](add, sub, mul, div)

    val mop = MultiOp(ops)
    def testComplexFun(in: Operands) = mop.apply(in)
  }

  trait TestSetWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val ops = List[Operands => Future[Double]](add, sub, mul, div)

    val mop = MultiOp(ops)
    def testComplexFun(in: Operands) = mop.apply(in)

  }

  trait TestMapNoException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val ops = List[Operands => Double](add, sub, mul, div)
    val opsMap = Map[String, Operands => Double]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  test("MultiOp no ops") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List()

      val noFun = MultiOp(Nil)
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("MultiOp normal case") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiOp normal case infinitty") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiOp Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      intercept[Exception] { testComplexFun(input) }
    }
  }

  test("MultiOp with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { _.get }
      assert(actual === expect)

    }
  }

  test("MultiOp with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)

      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("MultiOp with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }

  test("MultiOp factory with map and single input") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val mop = MultiOp(opsMap)
      def testComplexFun(in: Operands) = mop.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiOp with map and multiple inputs") {
    new TestMapNoException {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val mop = MultiOp(opsMap)
      def testComplexFun(in: Operands) = mop.applyToMap(in)

      val actual = inputs.map(testComplexFun(_))
      assert(actual === expect)
    }
  }

  test("MultiOp composite") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(List(3.0, -1.0), List(2.0, 0.5))

      val mop1 = MultiOp(List[Operands => Double](add, sub))
      val mop2 = MultiOp(List[Operands => Double](mul, div))
      def rem(in: Operands): Double = in._1 % in._2.doubleValue()

      val mop = MultiOp(List(mop1, mop2))

      val actual = mop(input)
      assert(actual == expect)
    }
  }

  test("MultiOp associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mop11 = MultiOp(List[Operands => Double](add, sub))
      val mop12 = MultiOp(List[Operands => Double](mul))

      val mop21 = MultiOp(List[Operands => Double](add))
      val mop22 = MultiOp(List[Operands => Double](sub, mul))

      val complex1 = mop11 + mop12
      val complex2 = mop21 + mop22

      assert(complex1(input) === complex2(input))
      assert(complex1(input) == expect)
    }
  }

  test("MultiOp composition map()") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List("(1,2) => 3.0", "(1,2) => -1.0")

      def mkStr(f: Operands => Double): Operands => String = {
        in: Operands => s"$in => ${f(in)}"
      }
      val mop = MultiOp(List[Operands => Double](add, sub))

      val actual = mop.map(mkStr)(input)
      assert(actual == expect)

    }
  }

  test("MultiOp composition flatMap()") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List("(1,2) => 3.0", "(1,2) => -1.0")

      def mkStr(f: Operands => Double): MultiOp[Operands, String] = {
        MultiOp[Operands, String](List[Operands => String] { in: Operands => s"$in => ${f(in)}" })
      }
      val mop = MultiOp(List[Operands => Double](add, sub))

      val actual = mop.flatMap(mkStr)(input)
      assert(actual == expect)
    }
  }

  test("MultiOp ++ associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mop11 = MultiOp(List[Operands => Double](add, sub))
      val mop12 = MultiOp(List[Operands => Double](mul))

      val mop21 = MultiOp(List[Operands => Double](add))
      val mop22 = MultiOp(List[Operands => Double](sub, mul))

      val mop1 = mop11 + mop12
      val mop2 = mop21 + mop22

      assert(mop1(input) === mop2(input))
      assert(mop2(input) == expect)
    }
  }

  test("MultiOp flatMap associativity") {

    val input = (1, 2)
    val expect = List("{(1,2) => 3.0}", "{(1,2) => -1.0}")

    def mkStr(f: Operands => Double): MultiOp[Operands, String] = {
      MultiOp[Operands, String](List[Operands => String] { in: Operands => s"$in => ${f(in)}" })
    }

    def mkStrFun2(f: Operands => String): MultiOp[Operands, String] = {
      MultiOp[Operands, String](List[Operands => String] { in: Operands => s"{${f(in)}}" })
    }

    def testF1 = Op("testF1", mkStr)
    def testF2 = Op("testF2", mkStrFun2)

    def add() = Op("add", { in: Operands => in._1 + in._2.doubleValue() })
    def sub() = Op("sub", { in: Operands => in._1 - in._2.doubleValue() })
    val mop = MultiOp("mop", List(add, sub))

    def fm1 = (mop flatMap testF1) flatMap testF2
    def fm2 = mop flatMap (x => testF1(x) flatMap testF2)

    assert(fm1(input) === fm1(input))
    assert(fm1(input) == expect)

  }

  test("MultiOp left unit") {

    val input = (1, 2)
    val expect = List("(1,2) => 3.0")

    def mkStr(f: Operands => Double): MultiOp[Operands, String] = {
      MultiOp[Operands, String](List[Operands => String] { in: Operands => s"$in => ${f(in)}" })
    }

    def f = Op("f", mkStr)

    def x = Op("add", { in: Operands => in._1 + in._2.doubleValue() })

    val unit = MultiOp("mop", x)

    def actual = unit flatMap f

    assert(actual(input) === f(x)(input))
    assert(actual(input) == expect)

  }

}