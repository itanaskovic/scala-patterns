package patterns

/**
 * This is a structure (sequence) of functions that that can be repeatedly
 * applied on various inputs of the same type, producing a sequence of
 * corresponding results.
 *
 * The main use case is one input to multiple outputs.
 *
 * @tparam I the type of the input value
 * @tparam T the type of the output sequence
 * @author oliver
 */
trait MultiFun[I, T] {
  /**
   * The sequence of functions that create this function structure.
   */
  protected def functions: Seq[I => T]
  /**
   * Apply the sequence of functions to the same input and return the sequence
   * of results in the same order as the functions.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the sequence output
   * @return the results sequence in the same order as the functions applied
   */
  def apply(input: I): Seq[T] = functions.map(f => f(input))
}

/**
 * This is a structure (sequence) of functions that that can be repeatedly
 * applied on various inputs of the same type, producing a sequence of
 * corresponding results.
 *
 * The main use case is one input to multiple outputs.
 *
 * @tparam I the type of the input value
 * @tparam T the type of the sequence output
 * @param functions the sequence of functions that create this function structure
 * @author oliver
 */
class MultiSeqFun[I, T](val functions: Seq[I => T]) extends MultiFun[I, T]

/**
 * `MultiSeqFun` companion object
 */
object MultiSeqFun {

  /**
   * Factory method for `MultiSeqFun`
   */
  def apply[I, T](functions: Seq[I => T]) = new MultiSeqFun(functions)
}

/**
 * This is a structure (map) of function/result names and functions that
 * can be repeatedly applied on various inputs of the same type, producing a
 * sequence or a map of corresponding results.
 *
 * The main use case is one input to multiple outputs.
 *
 * @tparam I the type of the input value
 * @tparam T the type of the output sequence
 * @param functions the sequence of functions that create this function structure
 * @param names the sequence of function/result names
 * @author oliver
 */
class MultiMapFun[I, T](val functions: Seq[I => T],
                        val names: Seq[String]) extends MultiFun[I, T] {
  if (names.size != functions.size)
    throw new IllegalArgumentException(
      s"The number of names (${names.size}) is different than the number of functions (${functions.size}).")

  /**
   * Constructor that takes a map of function/result names and functions as an argument.
   * @param functionsMap the map of function/result names and functions that create this function structure
   */
  def this(functionsMap: Map[String, I => T]) = {
    this(functionsMap.values.toSeq, functionsMap.keys.toSeq)
  }

  /**
   * Apply the sequence of functions to the same input and return the sequence
   * of results in the same order as the functions.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output map value
   * @return the map of function/result names and results
   */
  def applyToMap(input: I) = names.zip(super.apply(input)).toMap
}

/**
 * `MultiMapFun` companion object
 */
object MultiMapFun {

  /**
   * Create a `MultiMapFun` object with functions and default names.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the List output
   * @param functions the sequence of functions that create this function structure
   * @return the newly created `MultiMapFun` object
   */
  def apply[I, T](functions: Seq[I => T]): MultiMapFun[I, T] =
    MultiMapFun(functions, Seq[String]())

  /**
   * Create a `MultiMapFun` object with functions and function/result names.
   *
   * If there are less names than functions, the names are completed with
   * defaults, like "fun_XY"
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output values
   * @param functions the sequence of functions that create this function structure
   * @param names the sequence names associated with each function; they can be used as results names as well
   * @return the newly created `MultiMapFun` object
   */
  def apply[I, T](functions: Seq[I => T], names: Seq[String]): MultiMapFun[I, T] = {
    if (names.size < functions.size) {
      val nnames = names ++ (names.size until functions.size).map(i => "fun_%2d".format(i))
      new MultiMapFun(functions, nnames)
    } else
      new MultiMapFun(functions, names.take(functions.size))
  }

  /**
   * Create a `MultiMapFun` object with functions and names defined in the functionsMap.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the output values
   * @param functionsMap the map of function/result names and functions that create this function structure
   * @return the newly created `MultiMapFun` object
   */
  def apply[I, T](functionsMap: Map[String, I => T]): MultiMapFun[I, T] =
    new MultiMapFun(functionsMap)
}