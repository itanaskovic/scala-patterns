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
sealed trait MultiFun[I, T] {
  /**
   * The sequence of functions that create this function structure.
   */
  def functions: Seq[I => T]
  /**
   * Apply the sequence of functions to the same input and return the sequence
   * of results in the same order as the functions.
   *
   * @tparam I the type of the input value
   * @tparam T the type of the sequence output
   * @param input the function structure will be applied to this input
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
case class MultiFunSeq[I, T](functions: Seq[I => T]) extends MultiFun[I, T]

object MultiFun {
  def apply[I, T](multiFuns: Seq[MultiFun[I, T]]): MultiFunSeq[I, T] =
    MultiFunSeq(multiFuns.foldLeft(List[I => T]())((l, f) => l ++ f.functions.toList))
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
 * @param functionsMap the map of function/result names and functions that
 * create this function structure
 * @author oliver
 */
case class MultiFunMap[I, T](functionsMap: Map[String, I => T]) extends MultiFun[I, T] {

  val functions = functionsMap.values.toSeq

  /**
   * Apply the sequence of functions to the same input and return a map of names
   * and results.
   *
   * @param input the function structure will be applied to this input
   * @return the map of function/result names and results
   */
  def applyToMap(input: I) = functionsMap.map { case (name, f) => (name, f(input)) }
}
