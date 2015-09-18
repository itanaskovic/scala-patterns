package tupol.patterns.compofun

import scala.language.postfixOps
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{ Try, Success, Failure }
import tupol.patterns.compofun._

/**
 * Tests for the `CompoFun` pattern
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class CompoFunTestSuite extends FunSuite {

  type Operands = (Int, Int)
  type CxFun = Function1[Operands, Double]

  trait TestSimpleFunctions {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val funList = List[Operands => Double](add, sub, mul, div)
    val funMap = Map[String, Operands => Double]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  trait TestSetNoException extends TestSimpleFunctions {
    val compoFun = CompoFun(funList)
    def testComplexFun(in: Operands) = compoFun(in)
    def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)
  }

  trait TestSetWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val funSeq = List[CxFun](add, sub, mul, div)

    val compoFun = CompoFun(funSeq)
    def testComplexFun(in: Operands) = compoFun.apply(in)
    def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)
  }

  trait TestSetWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val funSeq = List[Operands => Try[Double]](add, sub, mul, div)

    val compoFun = CompoFun(funSeq)
    def testComplexFun(in: Operands) = compoFun.apply(in)
    def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)
  }

  trait TestSetWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val funSeq = List[Operands => Future[Double]](add, sub, mul, div)

    val compoFun = CompoFun(funSeq)
    def testComplexFun(in: Operands) = compoFun.apply(in)
    def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)

  }

  trait TestSetInputClass {
    case class Input(a: Int, b: Int)
    case class DivResult(r: Float)
    type CxFun = Function1[Input, Any]

    def add(in: Input): String = s"${in.a + in.b}"
    def sub(in: Input): Int = in.a - in.b
    def mul(in: Input): Double = in.a * in.b.doubleValue()
    def div(in: Input): DivResult = DivResult(in.a / in.b.floatValue())
    val funList = List[CxFun](add, sub, mul, div)
    val funMap = Map[String, CxFun]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)
  }

  test("CompoFun no functions") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List()

      val noFun = CompoFun(Nil)
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("CompoFun instance.apply(input)") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("CompoFun instance.applyToMap(input)") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("CompoFun instance.apply(input) with infinity in result") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("CompoFun instance.applyToMap(input) with infinity in result") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = Map("Fun$00" -> 1.0, "Fun$01" -> 1.0, "Fun$02" -> 0.0, "Fun$03" -> Double.PositiveInfinity)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("CompoFun instance.apply(input) Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      intercept[Exception] { testComplexFun(input) }
    }
  }

  test("CompoFun instance.applyToMap(input) Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      intercept[Exception] { testComplexFunMap(input) }
    }
  }

  test("CompoFun instance.apply(input) with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)
    }
  }

  test("CompoFun instance.applyToMap(input) with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input).map { case (n, Success(x)) => (n, x); case (n, Failure(e)) => (n, None) }
      assert(actual === expect)
    }
  }

  test("CompoFun instance.apply(input) with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)

      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)
    }
  }

  test("CompoFun instance.applyToMap(input) with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = Map("Fun$00" -> 1.0, "Fun$01" -> 1.0, "Fun$02" -> 0.0, "Fun$03" -> None)

      val actual = testComplexFunMap(input).map { case (n, Success(x)) => (n, x); case (n, Failure(e)) => (n, None) }
      assert(actual === expect)
    }
  }

  test("CompoFun instance.apply(input) with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)
    }
  }

  test("CompoFun instance.applyToMap(input) with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input).map { case (n, x) => (n, Await.result(x, 1 second)) }
      assert(actual === expect)
      assert(compoFun.getNames === expect.keys.toSeq)
    }
  }

  test("CompoFun factory with map and instance.apply(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val compoFun = CompoFun(funMap)
      def testComplexFunMap(in: Operands) = compoFun.apply(in)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("CompoFun factory with map and instance.applyToMap(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val compoFun = CompoFun(funMap)
      def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("CompoFun factory with map and instance.apply(input) mapped to a list of inputs") {
    new TestSimpleFunctions {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = List(3.0, -1.0, 2.0, 0.5)
      val expect2 = List(1.0, 1.0, 0.0, Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val compoFun = CompoFun(funMap)
      def testComplexFunMap(in: Operands) = compoFun.apply(in)

      val actual = inputs.map(testComplexFunMap(_))
      assert(actual === expect)
    }
  }

  test("CompoFun factory with map and instance.applyToMap(input) mapped to a list of inputs") {
    new TestSimpleFunctions {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val compoFun = CompoFun(funMap)
      def testComplexFunMap(in: Operands) = compoFun.applyToMap(in)

      val actual = inputs.map(testComplexFunMap(_))
      assert(actual === expect)
      assert(compoFun.getNames === expect1.keys.toSeq)
    }
  }

  test("CompoFun NamedCompoFun factory mixed named and nameless functions and instance.apply(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val compoFun = CompoFun(List[CxFun](add, sub, Fun("mul", mul), div))

      val actual = compoFun(input)
      assert(actual == expect)
      assert(compoFun.getNames === Seq("Fun$00", "Fun$01", "mul", "Fun$03"))
    }
  }

  test("CompoFun NamedCompoFun factory mixed named and nameless functions and instance.applyToMap(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "Fun$01" -> -1.0, "mul" -> 2.0, "Fun$03" -> 0.5)

      val compoFun = CompoFun(List[CxFun](Fun("add", add), sub, Fun("mul", mul), div))

      val actual = compoFun.applyToMap(input)
      assert(actual == expect)
      assert(compoFun.getNames === expect.keys.toSeq)
    }
  }

  test("CompoFun :+") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val compoFun1 = CompoFun(List[CxFun](add, sub))

      val compoFun = compoFun1 :+ Fun(mul)

      assert(compoFun(input) == expect)
    }
  }

  test("CompoFun +:") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(2.0, 3.0, -1.0)

      val compoFun1 = CompoFun(List[CxFun](add, sub))

      val compoFun = Fun(mul) +: compoFun1

      assert(compoFun(input) == expect)
    }
  }

  test("CompoFun ++") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val compoFun11 = CompoFun(List[CxFun](add, sub))
      val compoFun12 = CompoFun(List[CxFun](mul))

      val compoFun = compoFun11 ++ compoFun12

      assert(compoFun(input) == expect)
    }
  }

  test("CompoFun ++ associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val compoFun11 = CompoFun(List[CxFun](add, sub))
      val compoFun12 = CompoFun(List[CxFun](mul))

      val compoFun21 = CompoFun(List[CxFun](add))
      val compoFun22 = CompoFun(List[CxFun](sub, mul))

      val compoFun1 = compoFun11 ++ compoFun12
      val compoFun2 = compoFun21 ++ compoFun22

      assert(compoFun1(input) === compoFun2(input))
      assert(compoFun2(input) == expect)
    }
  }

  test("CompoFun composite") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(List(3.0, -1.0), List(2.0, 0.5))

      val compoFun1 = CompoFun(List[CxFun](add, sub))
      val compoFun2 = CompoFun(List[CxFun](mul, div))
      def rem(in: Operands): Double = in._1 % in._2.doubleValue()

      val compoFun = CompoFun(List(compoFun1, compoFun2))

      val actual = compoFun(input)
      assert(actual == expect)
    }
  }

  test("CompoFun with input class and mixed result types") {
    new TestSetInputClass {

      val input = Input(1, 2)
      val expect = List("3", -1, 2.0, DivResult(0.5f))

      val compoFun = CompoFun(List[CxFun](add, sub, mul, div))

      val actual = compoFun(input)
      assert(actual == expect)
    }
  }

}