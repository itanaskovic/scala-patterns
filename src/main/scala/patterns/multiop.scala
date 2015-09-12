package patterns

import scala.language.implicitConversions

/**
 * This package contains the `multiop` pattern.
 *
 *
 * @author oliver
 */
package object multiop {

  type Xop[A, B] = Operation[A, B]
  type Ops[A, B] = Seq[Operation[A, B]]
  type Fun[A, B] = Function1[A, B]

  /**
   * We define an `Operation` to be a function of A => B that also has a name
   */
  trait Operation[-A, +B] extends Fun[A, B] {
    val name = ANONYMOUS
    def apply(input: A): B
    override def toString = s"$name"
  }

  /**
   * The `Id` is an operation that returns the input
   */
  case class Id[A]() extends Operation[A, A] {
    override val name = "Identity"
    def apply(input: A) = input
  }

  /**
   * The `Op` is a concrete `Operation`
   */
  case class Op[-A, +B](override val name: String, function: A => B) extends Operation[A, B] {
    def apply(input: A): B = function(input)
  }

  /**
   * Companion object for the `Op` class
   */
  object Op {
    def apply[A, B](function: A => B): Op[A, B] =
      new Op[A, B](ANONYMOUS, function)
    def apply[A, B](op: Op[A, B]): Op[A, B] =
      new Op[A, B](op.name, op.function)
  }

  /**
   * `MultiOp` is a structure (sequence) of `Operation`s that that can be repeatedly
   * applied on various inputs of the same type, producing a sequence of
   * corresponding results.
   *
   * `MultiOp` is also a composite structure, meaning that the input functions can also contain `MultiOp` operations
   *
   * The main use case is one input to multiple outputs.
   * For example, given a pair
   * of numbers, one can produce the list of results by applying +, _, * and /
   * operators, in a corresponding sequence.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output sequence
   * @author oliver
   */
  case class MultiOp[A, +B](override val name: String, operations: Ops[A, B]) extends Operation[A, Seq[B]] {

    def apply(input: A): Seq[B] = operations.map(f => f(input))

    def applyToMap(input: A): Map[String, B] = operations.map(f => (f.name, f(input))).toMap

    def +[A, B](that: MultiOp[A, B]): MultiOp[A, B] =
      MultiOp(this.name, //          "(" + this.name + " + " + that.name + ")",
        (this.operations ++ that.operations).asInstanceOf[Seq[Operation[A, B]]])

    def map[C](g: Operation[A, B] => Operation[A, C]): MultiOp[A, C] =
      MultiOp(s"$name mapped with ${g.name}", this.operations.map(f => g(f)))

    def flatMap[C](g: Operation[A, B] => MultiOp[A, C]): MultiOp[A, C] =
      this.operations.foldLeft(MultiOp[A, C]())((mf, f) => mf + g(f))

    override def toString: String = s"$name: (${operations.map(_.name).mkString(", ")})"
  }

  /**
   * Companion for `MultiOp`
   */
  object MultiOp {

    def apply[A, B](): MultiOp[A, B] = new MultiOp[A, B](ANONYMOUS, Nil)
    def apply[A, B](functions: Seq[A => B]): MultiOp[A, B] = {
      new MultiOp[A, B](ANONYMOUS, functions)
    }

    def apply[A, B](name: String, function: A => B): MultiOp[A, B] = {
      new MultiOp[A, B](name, List(function))
    }

    def apply[A, B](functionsMap: Map[String, A => B]): MultiOp[A, B] =
      apply(functionsMap.foldLeft(List[A => B]())((acc, tup) => Op(tup._1, tup._2) :: acc))

    /*
    def apply[A, B](name: String): MultiOp[A, B] = new MultiOp[A, B](name, Nil)

    def apply[A, B](functions: Seq[A => B]): MultiOp[A, B] = {
      new MultiOp[A, B](ANONYMOUS, functions)
    }

    def apply[A, B](name: String, function: A => B): MultiOp[A, B] = {
      new MultiOp[A, B](name, List(function))
    }

    def apply[A, B](name: String, functionsMap: Map[String, A => B]): MultiOp[A, B] =
      new MultiOp[A, B](name, functionsMap.foldLeft(List[A => B]())((acc, tup) => Op(tup._1, tup._2) :: acc))

    def apply[A, B](functionsMap: Map[String, A => B]): MultiOp[A, B] =
      MultiOp(ANONYMOUS, functionsMap)
*/
  }

  /**
   * Implicit conversion of sequences of functions to sequences of operations
   */
  implicit def functionsToOperations[A, B](funcs: Seq[A => B]): Seq[Operation[A, B]] = {
    (0 until funcs.size).zip(funcs).map {
      case (i, f) => f match {
        case af: Operation[A, B] => af
        case xf: Function1[A, B] => Op("Fun$%02d".format(i), xf)
      }
    }
  }

  /**
   * Implicit conversion of functions to operations
   */
  implicit def functionToOperation[A, B](func: Function1[A, B]): Operation[A, B] = func match {
    case af: Operation[A, B] => af
    case xf: Function1[A, B] => Op(ANONYMOUS, xf)
  }

  final val ANONYMOUS = "#anon"

}