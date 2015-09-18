package tupol.patterns.compofun

import tupol.patterns.compofun._

/**
 * `CompoFun` is a structure (sequence) of `Function1`s that that can be
 * repeatedly applied on various inputs of the same type, producing a
 * sequence of corresponding results.
 *
 * At the same time `CompoFun` is also a function, so calling apply(input)
 * will apply the sequence of functions to the given input and produce a
 * sequence of results.
 *
 * The main use case is mapping one input to multiple outputs.
 *
 * For example, given a pair of numbers, one can produce the list of results
 * by applying +, _, * and / operators, in a corresponding sequence.
 *
 * @tparam A the type of the input value
 * @tparam B the type of the output sequence
 * @author oliver
 */
sealed trait CompoFun[A, B] extends Fun[A, Seq[B]] {

  protected[compofun] def functions: Seq[A => B]

  /**
   * Apply the input to the sequence of functions and produce a sequence
   * of results in the same order as the corresponding functions.
   *
   * @param input
   * @return the sequence of results in the same order as the corresponding
   * functions.
   */
  def apply(input: A): Seq[B] = functions.map(f => f(input))

  /**
   * Apply the input to the sequence of functions and produce a map
   * of results having as a key the name of the functions if it is a
   * NamedFun instance or a generated name indicating the 0 based index of
   * the corresponding function.
   *
   * @param input
   * @return the map of results having as a key the name of the functions
   * if it is a NamedFun instance or a generated name indicating the 0 based
   * index of the corresponding function.
   */
  def applyToMap(input: A): Map[String, B] =
    getNames.zip(functions).toMap.map {
      case (n, f) => (n, f(input))
    }

  /**
   * Prepend function to this `CompoFun` instance
   */
  def +:(function: Fun[A, B]): CompoFun[A, B]

  /**
   * Append function to this `CompoFun`
   */
  def :+(function: Fun[A, B]): CompoFun[A, B]

  /**
   * Add that `CompoFun` object to this `CompoFun` instance
   */
  def ++(that: CompoFun[A, B]): CompoFun[A, B]

  /**
   * Return the list of names or generated names
   */
  lazy val getNames: Seq[String] =
    (0 until functions.size).zip(functions).toMap.map {
      case (i, f) => makeName(f, i)
    }.toSeq

  private val FUN_NAME_FORMAT = "Fun$%02d"

  private def makeName(f: Fun[_, _], i: Int) = f match {
    case op: NamedFun[_, _] => op.name
    case _                  => FUN_NAME_FORMAT.format(i)
  }

}

/**
 * This is a nameless `CompoFun` class
 */
private[compofun] case class NamelessCompoFun[A, B](val functions: Seq[A => B]) extends CompoFun[A, B] {

  def +:(function: Fun[A, B]): CompoFun[A, B] =
    NamelessCompoFun((function +: functions))

  def :+(function: Fun[A, B]): CompoFun[A, B] =
    NamelessCompoFun((functions :+ function))

  def ++(that: CompoFun[A, B]): CompoFun[A, B] =
    NamelessCompoFun(this.functions ++ that.functions)
}

/**
 * This is a `CompoFun` class that also has a name
 */
private[compofun] case class NamedCompoFun[A, B](val name: String, val functions: Seq[A => B]) extends CompoFun[A, B] with NamedFun[A, Seq[B]] {

  def +:(newName: String, function: Fun[A, B]): CompoFun[A, B] =
    NamedCompoFun(newName, function +: functions)

  def +:(function: Fun[A, B]): CompoFun[A, B] =
    +:(this.name, function)

  def :+(newName: String, function: Fun[A, B]): CompoFun[A, B] =
    NamedCompoFun(newName, (functions :+ function))

  def :+(function: Fun[A, B]): CompoFun[A, B] =
    :+(this.name, function)

  def ++(newName: String, that: CompoFun[A, B]): CompoFun[A, B] =
    NamedCompoFun(newName, this.functions ++ that.functions)

  def ++(that: CompoFun[A, B]): CompoFun[A, B] =
    ++(this.name, that)

}

/**
 * Companion for `CompoFun`
 */
object `CompoFun` {

  /**
   * Create a nameless `CompoFun` instance from another `CompoFun` instance.
   */
  def apply[A, B](compoFun: CompoFun[A, B]): CompoFun[A, B] =
    NamelessCompoFun[A, B](compoFun.functions)

  /**
   * Create a nameless `CompoFun` instance from a sequence of functions.
   */
  def apply[A, B](functions: Seq[A => B]): CompoFun[A, B] =
    NamelessCompoFun(functions)

  /**
   * Create a nameless `CompoFun` instance from a map of function names and functions.
   */
  def apply[A, B](functionsMap: Map[String, Fun[A, B]]): CompoFun[A, B] =
    NamelessCompoFun(functionsMap.foldLeft(Seq[Fun[A, B]]())((acc, tup) => acc :+ Fun(tup._1, tup._2)))

  /**
   * Create a named `CompoFun` instance from another `CompoFun` instance.
   */
  def apply[A, B](name: String, compoFun: CompoFun[A, B]): CompoFun[A, B] =
    NamedCompoFun[A, B](name, compoFun.functions)

  /**
   * Create a named `CompoFun` instance from a single function.
   */
  def apply[A, B](name: String, function: Fun[A, B]): CompoFun[A, B] =
    NamedCompoFun[A, B](name, Seq(function))

  /**
   * Create a named `CompoFun` instance from a sequence of functions.
   */
  def apply[A, B](name: String, functions: Seq[A => B]): CompoFun[A, B] =
    NamedCompoFun[A, B](name, functions)

  /**
   * Create a named `CompoFun` instance from a map of function names and functions.
   */
  def apply[A, B](name: String, functionsMap: Map[String, Fun[A, B]]): CompoFun[A, B] =
    NamedCompoFun(name, functionsMap.foldLeft(Seq[Fun[A, B]]())((acc, tup) => acc :+ Fun(tup._1, tup._2)))

}

/**
 * Asynchronous companion for `CompoFun`.
 * Functions are mapped from ( A => B ) to ( A => Future[B] )
 */
object ACompoFun {

  import scala.language.implicitConversions
  import scala.concurrent._

  implicit def fun2fun[A, B](f: A => B)(implicit ec: ExecutionContext): Fun[A, Future[B]] = f match {
    case nf: NamedFun[A, B]  => Fun(nf.name, { (input: A) => Future[B](nf(input)) })
    case f1: Function1[A, B] => Fun { (input: A) => Future[B](f1(input)) }
  }
  implicit def funs2funs[A, B](fx: Seq[A => B])(implicit ec: ExecutionContext): Seq[Fun[A, Future[B]]] = fx.map(fun2fun(_))
  implicit def funMap2funMap[A, B](f: Map[String, A => B])(implicit ec: ExecutionContext) =
    f.map(nf => (nf._1, fun2fun(nf._2)))

  /**
   * Create a nameless `CompoFun` of futures instance from another `CompoFun` instance.
   */
  def apply[A, B](compoFun: CompoFun[A, B])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamelessCompoFun[A, Future[B]](compoFun.functions)

  /**
   * Create a nameless `CompoFun` of futures instance from a sequence of functions.
   */
  def apply[A, B](functions: Seq[A => B])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamelessCompoFun[A, Future[B]](functions)

  /**
   * Create a nameless `CompoFun` of futures instance from a map of function names and functions.
   */
  def apply[A, B](functionsMap: Map[String, Fun[A, B]])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamelessCompoFun[A, Future[B]](functionsMap.foldLeft(Seq[Fun[A, Future[B]]]())((acc, nf) => acc :+ Fun[A, Future[B]](nf._1, nf._2)))

  /**
   * Create a named `CompoFun` of futures instance from another `CompoFun` instance.
   */
  def apply[A, B](name: String, compoFun: CompoFun[A, B])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamedCompoFun[A, Future[B]](name, compoFun.functions)

  /**
   * Create a named `CompoFun` of futures instance from a single function.
   */
  def apply[A, B](name: String, function: Fun[A, B])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamedCompoFun[A, Future[B]](name, Seq(function))

  /**
   * Create a named `CompoFun` of futures instance from a sequence of functions.
   */
  def apply[A, B](name: String, functions: Seq[A => B])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamedCompoFun[A, Future[B]](name, functions)

  /**
   * Create a named `CompoFun` of futures instance from a map of function names and functions.
   */
  def apply[A, B](name: String, functionsMap: Map[String, Fun[A, B]])(implicit ec: ExecutionContext): CompoFun[A, Future[B]] =
    NamedCompoFun[A, Future[B]](name, functionsMap.foldLeft(Seq[Fun[A, B]]())((acc, tup) => acc :+ Fun(tup._1, tup._2)))

}