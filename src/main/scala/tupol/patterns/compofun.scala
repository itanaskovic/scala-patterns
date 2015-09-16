package tupol.patterns

import scala.language.implicitConversions

/**
 * This package contains the `multiop` pattern.
 *
 * The key elements of this pattern are Fun and CompoFun.
 *
 * Fun is a wrapper class for Function1 which can also create named functions.
 * CompoFun is a Fun container containing a sequence of functions that are
 * applied to the same input.
 *
 * @author oliver
 */
package object compofun {

  type Funs[-A, +B] = Seq[Fun[A, B]]
  type Fun1[-A, +B] = A => B

  import scala.language.implicitConversions

  implicit def funstofuns[A, B](f: Seq[A => B]) = f.map {
    case sf: Fun[A, B]       => sf
    case f1: Function1[A, B] => Fun(f1)
  }
  implicit def funMaptofunMap[A, B](f: Map[String, A => B]) = f.map {
    case (n, sf: Fun[A, B])       => (n, sf)
    case (n, f1: Function1[A, B]) => (n, Fun(f1))
  }

  sealed trait Fun[-A, +B] extends Function1[A, B]

  object Fun {
    def apply[A, B](function: A => B): Fun[A, B] = function match {
      case sf: Fun[A, B] => sf
      case f1: Function1[A, B] => new Fun[A, B] {
        def apply(input: A): B = function(input)
      }
    }
    def apply[A, B](fname: String, function: A => B): Fun[A, B] = new NamedFun[A, B] {
      val name = fname
      def apply(input: A): B = function(input)
    }
  }

  /**
   * We define an `NamedFun` to be a function of A => B that also has a name
   */
  private trait NamedFun[-A, +B] extends Fun[A, B] {
    def name: String
    def apply(input: A): B
    override def toString() = s"<$name>"
  }

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
  sealed trait CompoFun[-A, +B] extends Fun[A, Seq[B]] {

    protected def functions: Funs[A, B]

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
    def +:[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y]

    /**
     * Append function to this `CompoFun`
     */
    def :+[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y]

    /**
     * Add that `CompoFun` object to this `CompoFun` instance
     */
    def ++[X <: A, Y >: B](that: CompoFun[X, Y]): CompoFun[X, Y]

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
   * Companion for `CompoFun`
   */
  object CompoFun {

    def apply[A, B](functions: Seq[Fun1[A, B]]): CompoFun[A, B] =
      Nameless(functions)

    def apply[A, B](functionsMap: Map[String, Fun[A, B]]): CompoFun[A, B] =
      Nameless(functionsMap.foldLeft(Seq[Fun[A, B]]())((acc, tup) => acc :+ Fun(tup._1, tup._2)))

    def apply[A, B](name: String, function: Fun[A, B]): CompoFun[A, B] =
      Named(name, Seq(function))

    def apply[A, B](name: String, functions: Funs[A, B]): CompoFun[A, B] =
      Named[A, B](name, functions)

    def apply[A, B](name: String, functionsMap: Map[String, Fun[A, B]]): CompoFun[A, B] =
      Named(name, functionsMap.foldLeft(Seq[Fun[A, B]]())((acc, tup) => acc :+ Fun(tup._1, tup._2)))

    def apply[A, B](name: String, multiFun: CompoFun[A, B]): CompoFun[A, B] =
      Named[A, B](name, multiFun.functions)

    /**
     * This is a nameless CompoFun class
     */
    private case class Nameless[-A, +B](val functions: Funs[A, B]) extends CompoFun[A, B] {

      def +:[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y] =
        Nameless((function +: functions))

      def :+[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y] =
        Nameless((functions :+ function))

      def ++[X <: A, Y >: B](that: CompoFun[X, Y]): CompoFun[X, Y] =
        Nameless(this.functions ++ that.functions)
    }

    /**
     * This is a CompoFun class that also has a name
     */
    private case class Named[-A, +B](val name: String, val functions: Funs[A, B]) extends CompoFun[A, B] with NamedFun[A, Seq[B]] {

      def +:[X <: A, Y >: B](newName: String, function: Fun[X, Y]): CompoFun[X, Y] =
        Named(newName, function +: functions)

      def +:[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y] =
        +:(this.name, function)

      def :+[X <: A, Y >: B](newName: String, function: Fun[X, Y]): CompoFun[X, Y] =
        Named(newName, (functions :+ function))

      def :+[X <: A, Y >: B](function: Fun[X, Y]): CompoFun[X, Y] =
        :+(this.name, function)

      def ++[X <: A, Y >: B](newName: String, that: CompoFun[X, Y]): CompoFun[X, Y] =
        Named(newName, this.functions ++ that.functions)

      def ++[X <: A, Y >: B](that: CompoFun[X, Y]): CompoFun[X, Y] =
        ++(this.name, that)

    }

  }

}