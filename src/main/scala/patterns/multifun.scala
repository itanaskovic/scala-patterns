package patterns

import scala.collection.immutable._
import scala.language.higherKinds

/**
 * This package contains the `multifun` pattern.
 *
 *
 * @author oliver
 */
package object multifun {

  private val NAME_FORMAT = "F$%02d"

  type Fun[-A, +B] = A => B

  /**
   * `FunList` is a structure (sequence) of `Function1`s that that can be
   * repeatedly applied on various inputs of the same type, producing a
   * sequence of corresponding results.
   *
   * `FunList` is also a composite structure, meaning that the input functions
   * can also contain `FunList` objects
   *
   * The main use case is one input to multiple outputs.
   * For example, given a pair of numbers, one can produce the list of results
   * by applying +, _, * and / operators, in a corresponding sequence.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output sequence
   * @author oliver
   */
  case class FunList[-A, +B](val functions: Seq[Fun[A, B]]) extends Fun[A, Seq[B]] {

    def apply(in: A): Seq[B] = functions.map(f => f(in))

    /**
     * Add the `function` at the begining of the `functions` list
     *
     * @param function the functions to be added at the begining of the list
     * @return a new FunList containing as the head element the `function`
     * followed by the current `functions` list
     */
    def +:[X <: A, Y >: B](function: Fun[X, Y]): FunList[X, Y] =
      FunList(function +: functions)

    /**
     * Add the `functions` of that FunList at the end of this functions list.
     *
     * @param that the FunList to be appended
     * @return a new FunList containing the `functions` of this FunList
     * followed by the `functions` of that FunList
     */
    def ++[X <: A, Y >: B](that: FunList[X, Y]): FunList[X, Y] =
      FunList(this.functions ++ that.functions)

  }

  /**
   * Companion object for `FunList` containing factory methods
   */
  object FunList {

    def apply[A, B](): FunList[A, B] = FunList(Nil)

    def apply[A, B](function: Fun[A, B]): FunList[A, B] = FunList(Seq(function))

    def apply[A, B](functions: Fun[A, B]*): FunList[A, B] = FunList(functions.toList)

  }

  /**
   * `FunMap` is a structure (map) of names and `Function1`s that that can be
   * repeatedly applied on various inputs of the same type, producing a
   * sequence of corresponding results.
   *
   * `FunMap` is also a composite structure, meaning that the input functions
   * can also contain `FunMap` objects
   *
   * The main use case is one input to multiple outputs.
   * For example, given a pair of numbers, one can produce the map of results
   * by applying +, _, * and / operators, in a corresponding sequence.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output sequence
   * @author oliver
   */
  case class FunMap[-A, +B](val functions: Map[String, Fun[A, B]]) extends Fun[A, Map[String, B]] {

    def apply(in: A): Map[String, B] =
      functions.map(f => (f._1, f._2(in)))

    def +:[X <: A, Y >: B](name: String, function: Fun[X, Y]): FunMap[X, Y] =
      FunMap(functions + (name -> function))

    def ++[X <: A, Y >: B](that: FunMap[X, Y]): FunMap[X, Y] =
      FunMap(this.functions ++ that.functions)

    def map[X <: A, C](g: (String, Fun[A, B]) => (String, Fun[X, C])): FunMap[X, C] =
      FunMap(this.functions.map(f => g(f._1, f._2)))

    def flatMap[X <: A, C](g: (String, Fun[A, B]) => FunMap[X, C]): FunMap[X, C] =
      this.functions.foldLeft(FunMap[X, C]())((fm, f) => fm ++ g(f._1, f._2))

  }

  object FunMap {

    def apply[A, B](): FunMap[A, B] = FunMap(Map[String, Fun[A, B]]())

    def apply[A, B](functions: Seq[Fun[A, B]]): FunMap[A, B] =
      FunMap((0 until functions.size).zip(functions).map { case (i, f) => (NAME_FORMAT.format(i), f) }.toMap)

  }

}
