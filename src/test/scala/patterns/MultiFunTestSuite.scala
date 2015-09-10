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
 * Tests for the MultiFun pattern
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class MultiFunTestSuite extends FunSuite {

  type Operands = (Int, Int)

  trait TestSetNoException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val functions = List[Operands => Double](add, sub, mul, div)

    val complexFun = FunN(functions)
    def testComplexFun(in: Operands) = complexFun(in)
  }

  trait TestSetWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val functions = List[Operands => Double](add, sub, mul, div)

    val complexFun = FunN(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)
  }

  trait TestSetWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val functions = List[Operands => Try[Double]](add, sub, mul, div)

    val complexFun = FunN(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)
  }

  trait TestSetWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val functions = List[Operands => Future[Double]](add, sub, mul, div)

    val complexFun = FunN(functions)
    def testComplexFun(in: Operands) = complexFun.apply(in)

  }

  trait TestMapNoException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val functions = List[Operands => Double](add, sub, mul, div)
    val functionsMap = Map[String, Operands => Double]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  test("FunN no functions") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List()

      val noFun = FunN(Nil)
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("FunN normal case") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("FunN normal case infinitty") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)
      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("FunN Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      intercept[Exception] { testComplexFun(input) }
    }
  }

  test("FunN with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input).map { _.get }
      assert(actual === expect)

    }
  }

  test("FunN with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)
      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("FunN with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)
      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }

  test("FunN factory with map and single input") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = FunN(functionsMap)
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("FunN factory with map and multiple inputs") {
    new TestMapNoException {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val complexFun = FunN(functionsMap)
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = inputs.map(testComplexFun(_))
      assert(actual === expect)
    }
  }

  test("FunN factory with some default names") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val complexFun = FunN(functions, List("add"))
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("FunN factory with more names than methods") {
    new TestMapNoException {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = FunN(functions, List("add", "sub", "mul", "div", "log", "sqr"))
      def testComplexFun(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiFun associativity") {

    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()

    val complexFun11 = FunN(List[Operands => Double](add, sub))
    val complexFun12 = FunN(List[Operands => Double](mul))

    val complexFun21 = FunN(List[Operands => Double](add))
    val complexFun22 = FunN(List[Operands => Double](sub, mul))

    //    val complex1 = MultiFun(List(complexFun11, complexFun12))
    //    val complex2 = MultiFun(List(complexFun21, complexFun22))
    val complex1 = complexFun11 ++ complexFun12
    val complex2 = complexFun21 ++ complexFun22

    val input = (1, 2)
    val expect = List(3.0, -1.0, 2.0)

    assert(complex1(input) === complex2(input))
    assert(complex1(input) == expect)
  }

  test("MultiFun composition map()") {

    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mkStr(f: Operands => Double): Operands => String = {
      in: Operands => s"$in => ${f(in)}"
    }

    val expect = List("(1,2) => 3.0", "(1,2) => -1.0")
    val complexFun = FunN(List[Operands => Double](add, sub))
    val actual = complexFun.map(mkStr)(1, 2)

    assert(actual == expect)

  }

  test("MultiFun composition flatMap()") {

    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mkStrFun(f: Operands => Double): MultiFun[Operands, String] = {
      FunN[Operands, String](List[Operands => String] { in: Operands => s"$in => ${f(in)}" })
    }

    val expect = List("(1,2) => 3.0", "(1,2) => -1.0")
    val complexFun = FunN(List[Operands => Double](add, sub))
    val actual = complexFun.flatMap(mkStrFun)(1, 2)

    assert(actual == expect)
  }

  test("MultiFun ++ associativity") {

    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()

    val complexFun11 = FunN(List[Operands => Double](add, sub))
    val complexFun12 = FunN(List[Operands => Double](mul))

    val complexFun21 = FunN(List[Operands => Double](add))
    val complexFun22 = FunN(List[Operands => Double](sub, mul))

    val complex1 = complexFun11 ++ complexFun12
    val complex2 = complexFun21 ++ complexFun22

    val input = (1, 2)
    val expect = List(3.0, -1.0, 2.0)

    assert(complex1(input) === complex2(input))
    assert(complex1(input) == expect)
  }

}