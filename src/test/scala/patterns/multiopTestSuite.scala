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
 * Tests for the `MultiFun` pattern
 *
 * @author oliver
 */

@RunWith(classOf[JUnitRunner])
class multiopTestSuite extends FunSuite {

  type Operands = (Int, Int)
  type CxFun = Fun[Operands, Double]

  trait TestSimpleFunctions {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = in._1 / in._2.doubleValue()
    val funList = List[CxFun](add, sub, mul, div)
    val funMap = Map[String, CxFun]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)

  }

  trait TestSetNoException extends TestSimpleFunctions {
    val complexFun = MultiFun(funList)
    def testComplexFun(in: Operands) = complexFun(in)
    def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)
  }

  trait TestSetWithException {
    def add(in: Operands): Double = in._1 + in._2.doubleValue()
    def sub(in: Operands): Double = in._1 - in._2.doubleValue()
    def mul(in: Operands): Double = in._1 * in._2.doubleValue()
    def div(in: Operands): Double = throw new Exception
    val funSeq = List[CxFun](add, sub, mul, div)

    val complexFun = MultiFun(funSeq)
    def testComplexFun(in: Operands) = complexFun.apply(in)
    def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)
  }

  trait TestSetWithTry {
    def add(in: Operands): Try[Double] = Try(in._1 + in._2.doubleValue())
    def sub(in: Operands): Try[Double] = Try(in._1 - in._2.doubleValue())
    def mul(in: Operands): Try[Double] = Try(in._1 * in._2.doubleValue())
    def div(in: Operands): Try[Double] = Try(if (in._2 == 0) throw new Exception else in._1 / in._2.doubleValue())
    val funSeq = List[Operands => Try[Double]](add, sub, mul, div)

    val complexFun = MultiFun(funSeq)
    def testComplexFun(in: Operands) = complexFun.apply(in)
    def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)
  }

  trait TestSetWithFutures {
    def add(in: Operands): Future[Double] = Future { Thread.sleep(800); in._1 + in._2.doubleValue() }
    def sub(in: Operands): Future[Double] = Future { Thread.sleep(600); in._1 - in._2.doubleValue() }
    def mul(in: Operands): Future[Double] = Future { Thread.sleep(400); in._1 * in._2.doubleValue() }
    def div(in: Operands): Future[Double] = Future { Thread.sleep(200); in._1 / in._2.doubleValue() }
    val funSeq = List[Operands => Future[Double]](add, sub, mul, div)

    val complexFun = MultiFun(funSeq)
    def testComplexFun(in: Operands) = complexFun.apply(in)
    def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)

  }

  trait TestSetInputClass {
    case class Input(a: Int, b: Int)
    case class DivResult(r: Float)
    type CxFun = Fun[Input, Any]

    def add(in: Input): String = s"${in.a + in.b}"
    def sub(in: Input): Int = in.a - in.b
    def mul(in: Input): Double = in.a * in.b.doubleValue()
    def div(in: Input): DivResult = DivResult(in.a / in.b.floatValue())
    val funList = List[CxFun](add, sub, mul, div)
    val funMap = Map[String, CxFun]("add" -> add,
      "sub" -> sub, "mul" -> mul, "div" -> div)
  }

  test("MultiFun no functions") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List()

      val noFun = MultiFun(Nil)
      def testNoFun(in: Operands) = noFun(in)

      val actual = testNoFun(input)
      assert(actual === expect)
    }
  }

  test("MultiFun instance.apply(input)") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiFun instance.applyToMap(input)") {
    new TestSetNoException {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("MultiFun instance.apply(input) with infinity in result") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, Double.PositiveInfinity)

      val actual = testComplexFun(input)
      assert(actual === expect)
    }
  }

  test("MultiFun instance.applyToMap(input) with infinity in result") {
    new TestSetNoException {

      val input = (1, 0)
      val expect = Map("Fun$00" -> 1.0, "Fun$01" -> 1.0, "Fun$02" -> 0.0, "Fun$03" -> Double.PositiveInfinity)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("MultiFun instance.apply(input) Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      intercept[Exception] { testComplexFun(input) }
    }
  }

  test("MultiFun instance.applyToMap(input) Exception thrown") {
    new TestSetWithException {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      intercept[Exception] { testComplexFunMap(input) }
    }
  }

  test("MultiFun instance.apply(input) with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("MultiFun instance.applyToMap(input) with Try Success") {
    new TestSetWithTry {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input).map { case (n, Success(x)) => (n, x); case (n, Failure(e)) => (n, None) }
      assert(actual === expect)

    }
  }

  test("MultiFun instance.apply(input) with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = List(1.0, 1.0, 0.0, None)

      val actual = testComplexFun(input).map { case Success(x) => x; case Failure(e) => None }
      assert(actual === expect)

    }
  }

  test("MultiFun instance.applyToMap(input) with Try Failure") {
    new TestSetWithTry {

      val input = (1, 0)
      val expect = Map("Fun$00" -> 1.0, "Fun$01" -> 1.0, "Fun$02" -> 0.0, "Fun$03" -> None)

      val actual = testComplexFunMap(input).map { case (n, Success(x)) => (n, x); case (n, Failure(e)) => (n, None) }
      assert(actual === expect)

    }
  }

  test("MultiFun instance.apply(input) with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val actual = testComplexFun(input).map { x => Await.result(x, 1 second) }
      assert(actual === expect)

    }
  }

  test("MultiFun instance.applyToMap(input) with Future") {
    new TestSetWithFutures {

      val input = (1, 2)
      val expect = Map("Fun$00" -> 3.0, "Fun$01" -> -1.0, "Fun$02" -> 2.0, "Fun$03" -> 0.5)

      val actual = testComplexFunMap(input).map { case (n, x) => (n, Await.result(x, 1 second)) }
      assert(actual === expect)
      assert(complexFun.getNames === expect.keys.toSeq)

    }
  }

  test("MultiFun factory with map and instance.apply(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val complexFun = MultiFun(funMap)
      def testComplexFunMap(in: Operands) = complexFun.apply(in)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("MultiFun factory with map and instance.applyToMap(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)

      val complexFun = MultiFun(funMap)
      def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)

      val actual = testComplexFunMap(input)
      assert(actual === expect)
    }
  }

  test("MultiFun factory with map and instance.apply(input) mapped to a list of inputs") {
    new TestSimpleFunctions {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = List(3.0, -1.0, 2.0, 0.5)
      val expect2 = List(1.0, 1.0, 0.0, Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val complexFun = MultiFun(funMap)
      def testComplexFunMap(in: Operands) = complexFun.apply(in)

      val actual = inputs.map(testComplexFunMap(_))
      assert(actual === expect)
    }
  }

  test("MultiFun factory with map and instance.applyToMap(input) mapped to a list of inputs") {
    new TestSimpleFunctions {

      val input1 = (1, 2)
      val input2 = (1, 0)
      val inputs = List(input1, input2)
      val expect1 = Map("add" -> 3.0, "sub" -> -1.0, "mul" -> 2.0, "div" -> 0.5)
      val expect2 = Map("add" -> 1.0, "sub" -> 1.0, "mul" -> 0.0, "div" -> Double.PositiveInfinity)
      val expect = List(expect1, expect2)

      val complexFun = MultiFun(funMap)
      def testComplexFunMap(in: Operands) = complexFun.applyToMap(in)

      val actual = inputs.map(testComplexFunMap(_))
      assert(actual === expect)
      assert(complexFun.getNames === expect1.keys.toSeq)
    }
  }

  test("MultiFun.Named factory mixed Fun and NFun and instance.apply(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0, 0.5)

      val complexFun = MultiFun(List[CxFun](add, sub, NamedFun("mul", mul), div))

      val actual = complexFun(input)
      assert(actual == expect)
    }
  }

  test("MultiFun.Named factory mixed Fun and NFun and instance.applyToMap(input)") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = Map("add" -> 3.0, "Fun$01" -> -1.0, "mul" -> 2.0, "Fun$03" -> 0.5)

      val complexFun = MultiFun(List[CxFun](NamedFun("add", add), sub, NamedFun("mul", mul), div))

      val actual = complexFun.applyToMap(input)
      assert(actual == expect)
      assert(complexFun.getNames === expect.keys.toSeq)
    }
  }

  test("MultiFun +") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mfun1 = MultiFun(List[CxFun](add, sub))

      val mmul: Fun[Operands, Double] = mul
      val mfun = mmul +: mfun1

      assert(mfun(input) == expect)
    }
  }

  test("MultiFun ++") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mfun11 = MultiFun(List[CxFun](add, sub))
      val mfun12 = MultiFun(List[CxFun](mul))

      val mfun = mfun11 ++ mfun12

      assert(mfun(input) == expect)
    }
  }

  test("MultiFun ++ associativity") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(3.0, -1.0, 2.0)

      val mfun11 = MultiFun(List[CxFun](add, sub))
      val mfun12 = MultiFun(List[CxFun](mul))

      val mfun21 = MultiFun(List[CxFun](add))
      val mfun22 = MultiFun(List[CxFun](sub, mul))

      val mfun1 = mfun11 ++ mfun12
      val mfun2 = mfun21 ++ mfun22

      assert(mfun1(input) === mfun2(input))
      assert(mfun2(input) == expect)
    }
  }

  test("MultiFun composite") {
    new TestSimpleFunctions {

      val input = (1, 2)
      val expect = List(List(3.0, -1.0), List(2.0, 0.5))

      val mfun1 = MultiFun(List[CxFun](add, sub))
      val mfun2 = MultiFun(List[CxFun](mul, div))
      def rem(in: Operands): Double = in._1 % in._2.doubleValue()

      val complexFun = MultiFun(List(mfun1, mfun2))

      val actual = complexFun(input)
      assert(actual == expect)
    }
  }

  test("MultiFun with input class and mixed result types") {
    new TestSetInputClass {

      val input = Input(1, 2)
      val expect = List("3", -1, 2.0, DivResult(0.5f))

      val complexFun = MultiFun(List[CxFun](add, sub, mul, div))

      val actual = complexFun(input)
      assert(actual == expect)

    }

    import java.net.URL

    val AMAZON_COM_URL = new URL("")
    val AMAZON_UK_URL = new URL("")
    val AMAZON_DE_URL = new URL("")
    val EBAY_URL = new URL("")
    val ALIBABA_URL = new URL("")

    case class Product(name: String, specifications: Map[String, String])
    case class Price(value: Double, currency: String)

    def getPriceFromStore(product: Product, url: URL): Price = ???

    def getPriceFromAmazon_COM(product: Product) = getPriceFromStore(product, AMAZON_COM_URL)
    def getPriceFromAmazon_UK(product: Product) = getPriceFromStore(product, AMAZON_UK_URL)
    def getPriceFromAmazon_DE(product: Product) = getPriceFromStore(product, AMAZON_DE_URL)
    def getPriceFromEBay(product: Product) = getPriceFromStore(product, EBAY_URL)
    def getPriceFromAlibaba(product: Product) = getPriceFromStore(product, ALIBABA_URL)

    def getAmazonPrices = MultiFun(List(NamedFun("amazon.com", getPriceFromAmazon_COM),
      NamedFun("amazon.uk", getPriceFromAmazon_UK),
      NamedFun("amazon.de", getPriceFromAmazon_DE)))

    def getOffersMainStores = getAmazonPrices ++ MultiFun(List(NamedFun("ebay", getPriceFromEBay),
      NamedFun("alibaba", getPriceFromAlibaba)))

    val interestingProduct = Product("Programming in Scala", Map("author" -> "Martin Odersky and Lex Spoon", "type" -> "Paperback"))

    val amazonPrices = getAmazonPrices(interestingProduct)
    val mainStorePrices = getOffersMainStores(interestingProduct)

  }

}