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
class ACompoFunTestSuite extends FunSuite {

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

  test("ACompoFun instance.apply(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val complexFun = ACompoFun(funMap)
      def testComplexFun(in: Operands) = complexFun.apply(in)

      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)
    }
  }

  test("ACompoFun instance.applyToMap(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = ACompoFun(funMap)

      def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFunMap(input).map { case (n, x) => (n, Await.result(x, 1 second)) }
      assert(actual === expect)
      assert(complexFun.getNames === expect.keys.toSeq)
    }
  }

  test("ACompoFun factory from CompoFun") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val compoFun = CompoFun(funMap)
      val aCompoFun = ACompoFun(compoFun)

      def testComplexFunMap(in: Operands) = aCompoFun.applyToMap(in)

      val actual = testComplexFunMap(input).map { case (n, x) => (n, Await.result(x, 1 second)) }
      assert(actual === expect)
      assert(aCompoFun.getNames === expect.keys.toSeq)
    }
  }

  test("ACompoFun ++") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val compoFun11 = ACompoFun(List[CxFun](add, sub))
      val compoFun12 = ACompoFun(List[CxFun](mul))

      val compoFun = compoFun11 ++ compoFun12

      val actual = compoFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }
}