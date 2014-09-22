package scevo.evo

/* Search state. Not to be confused with the state of the search *algorithm/process*, 
 */

class State[ES <: EvaluatedSolution[_ <: Evaluation]](val solutions: Seq[ES], val iteration: Int) {
  require(solutions.size > 0, "The set of working solutions in a state cannot be empty")
}

object State {

  def apply[ES <: EvaluatedSolution[_ <: Evaluation]](solutions: Seq[ES], iteration: Int = 0) =
    new State[ES](solutions, iteration)

  def apply[ES <: EvaluatedSolution[_ <: Evaluation]](popSize: Int, genSolution: => ES): State[ES] = {
    require(popSize > 0, "Population cannot be empty")
    new State(for (i <- 0 until popSize) yield genSolution, 0)
  }

}
