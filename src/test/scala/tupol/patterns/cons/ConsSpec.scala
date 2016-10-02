package tupol.patterns.cons

import org.scalatest.{FunSuite, Matchers}

/**
  *
  */
class ConsSpec extends FunSuite with Matchers {

  test("Cons creation head and tail same types") {
    val c = cons(1, 2)
    c shouldBe a [Function1[_, _]]
    head(c) shouldBe 1
    tail(c) shouldBe 2
  }

  test("Cons creation head and tail different types") {
    val c = cons(1, "2.0")
    c shouldBe a [Function1[_, _]]
    head(c) shouldBe 1
    tail(c) shouldBe "2.0"
  }

  test("Cons creation head and tail of cons") {
    val c1 = cons(1, null)
    val c2 = cons(2, null)
    val c = cons(c1, c2)
    c shouldBe a [Function1[_, _]]
    head(c) shouldBe a [Function1[_, _]]
    head(c) shouldBe c1
    tail(c) shouldBe a [Function1[_, _]]
    tail(c) shouldBe c2
  }

}
