package tupol.patterns.compofun

/**
 * Wrapper for Function1[A, B] ( A => B )
 */
trait Fun[A, B] extends Function1[A, B]

object Fun {
  /**
   * Create a nameless Fun instance
   */
  def apply[A, B](function: A => B): Fun[A, B] = function match {
    case sf: Fun[A, B] => sf
    case f1: Function1[A, B] => new Fun[A, B] {
      def apply(input: A): B = function(input)
    }
  }
  /**
   * Create a named Fun instance
   */
  def apply[A, B](fname: String, function: A => B): Fun[A, B] = new NamedFun[A, B] {
    val name = fname
    def apply(input: A): B = function(input)
  }
}

/**
 * We define an `NamedFun` to be a function of A => B that also has a name
 */
private[compofun] trait NamedFun[A, B] extends Fun[A, B] {
  def name: String
  override def toString() = s"<$name>"
}