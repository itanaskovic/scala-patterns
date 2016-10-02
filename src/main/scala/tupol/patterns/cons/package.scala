package tupol.patterns

/**
  * A simple cons implementation attempt using an idea from SICP and Lambda Calculus
  *
  * TODO: Study more
  */
package object cons {

  type CONS[T] = ((T, T) => T) => T

  type GET[T] = (T, T) => T

  private def _cons[T](a :T, b: T)(m: GET[T]): T = m(a, b)

  def cons[T](a :T, b: T): CONS[T] = _cons(a, b)

  def head[T](cx: CONS[T]): T = cx((a: T, b: T) => a )

  def tail[T](cx: CONS[T]): T = cx((a: T, b: T) => b )

}
