package patterns

/**
 * This is a structure (sequence) of functions that that can be repeatedly
 * applied on various inputs of the same type, producing a sequence of
 * corresponding results.
 *
 * The main use case is one input to multiple outputs. For example, given a pair
 * of numbers, one can produce the list of results by applying +, _, * and /
 * operators, in a corresponding sequence.
 *
 * @tparam I the type of the input value
 * @tparam T the type of the output sequence
 * @author oliver
 */
trait MultiFun[A, B] {
  /**
   * The sequence of functions that create this function structure.
   */
  def functions: Seq[A => B]
  /**
   * Default names (Fun$%02d) corresponding to each function in the function sequence.
   */
  def names = defaultNames

  private def defaultNames = fillNames(functions, List())

  /**
   * Create a list of function names based on a given list of names.
   * If the list is shorter than the number of functions we fill in the rest with default names.
   * If the names list is longer we just take the names corresponding to the functions.
   */
  protected def fillNames(funcs: Seq[_], names: Seq[String]): Seq[String] =
    if (names.size < functions.size)
      names ++ (names.size until functions.size).map(i => "Fun$%02d".format(i))
    else if (names.size == functions.size)
      names
    else
      names.take(functions.size)

  /**
   * Apply the sequence of functions to the same input and return the sequence
   * of results in the same order as the functions.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the sequence output
   * @param input the function structure will be applied to this input
   * @return the results sequence in the same order as the functions applied
   */
  def apply(input: A): Seq[B] = functions.map(f => f(input))

  /**
   * Apply the sequence of functions to the same input and return a map of names
   * and results.
   *
   * @param input the function structure will be applied to this input
   * @return the map of function/result names and results
   */
  def applyToMap(input: A): Map[String, B] =
    names.zip(functions).map { case (name, f) => (name, f(input)) }.toMap

  /**
   * Create a new MultiFun object by adding the functions of this to the functions of that
   */
  def ++[A, B](that: MultiFun[A, B]): MultiFun[A, B]

  final def map[C](g: (A => B) => (A => C)): MultiFun[A, C] =
    if (this.functions.isEmpty) FunN[A, C]
    else FunN(this.functions.map(f => g(f)))

  final def flatMap[C](g: (A => B) => MultiFun[A, C]): MultiFun[A, C] =
    if (this.functions.isEmpty) FunN[A, C]
    else this.functions.foldLeft(FunN[A, C])((mf, f) => mf ++ g(f))

  override def toString: String = s"Fun${functions.size}: (${names.mkString(", ")})"
}

/**
 * This is a structure (sequence) of functions that that can be repeatedly
 * applied on various inputs of the same type, producing a sequence of
 * corresponding results.
 *
 * The main use case is one input to multiple outputs.
 *
 * @tparam I the type of the input value
 * @tparam T the type of the output sequence
 * @param functions the sequence of functions that create this function structure
 * @author oliver
 */
case class FunN[A, B](functions: Seq[A => B], nnames: Seq[String]) extends MultiFun[A, B] {

  override val names = fillNames(functions, nnames)

  override def ++[A, B](that: MultiFun[A, B]): MultiFun[A, B] =
    FunN((this.functions ++ that.functions).asInstanceOf[Seq[A => B]])

}

/**
 * Companion for `FunN`
 */
object FunN {

  def apply[A, B](): MultiFun[A, B] = new FunN[A, B](List[A => B](), Nil)

  /**
   * Create a `FunN` object with functions and default names.
   *
   * @tparam A the type of the input value
   * @tparam B the type of the List output
   * @param functions the sequence of functions that create this function structure
   * @return the newly created `FunN` object
   */
  def apply[A, B](functions: Seq[A => B]): MultiFun[A, B] = new FunN[A, B](functions, Nil)

  /**
   * Create a `FunN` object with functions and function/result names.
   *
   * @tparam A the type of the input value
   * @tparam B the type of the output values
   * @param functionsMap the map of function/result names and functions that
   * create this function structure
   * @return the newly created `FunN` object
   */
  def apply[A, B](functionsMap: Map[String, A => B]): MultiFun[A, B] =
    apply(functionsMap.values.toSeq, functionsMap.keys.toSeq)
}

