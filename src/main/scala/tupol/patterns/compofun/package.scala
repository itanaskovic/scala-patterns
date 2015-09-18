package tupol.patterns

/**
 * This package contains the `compofun` pattern.
 *
 * The key elements of this pattern are Fun and CompoFun.
 *
 * Fun is a wrapper class for Function1 which can also create named functions.
 * CompoFun is a Fun container containing a sequence of functions that are
 * applied to the same input.
 *
 * Other possible names: multifun, demux, xfun
 *
 * @author oliver
 */
package object compofun {

  import scala.language.implicitConversions

  implicit def fun2fun[A, B](function: A => B) = function match {
    case sf: Fun[A, B]       => sf
    case f1: Function1[A, B] => Fun(f1)
  }
  implicit def funs2funs[A, B](function: Seq[A => B]) =
    function.map(fun2fun(_))

  implicit def funMap2funMap[A, B](function: Map[String, A => B]) =
    function.map(nf => (nf._1, fun2fun(nf._2)))

}