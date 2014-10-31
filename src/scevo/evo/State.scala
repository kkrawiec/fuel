package scevo.evo

/* Search state. Not to be confused with the state of the search *algorithm/process*, 
 */

trait State {
  def iteration: Int
}

class PopulationState[ES <: EvaluatedSolution[_ <: Evaluation]](val solutions: Seq[ES], override val iteration: Int)
  extends State {
  require(solutions.size > 0, "The set of working solutions in a state cannot be empty")
}

object PopulationState {

  def apply[ES <: EvaluatedSolution[_ <: Evaluation]](solutions: Seq[ES], iteration: Int = 0) =
    new PopulationState[ES](solutions, iteration)

  def apply[ES <: EvaluatedSolution[_ <: Evaluation]](popSize: Int, genSolution: => ES): PopulationState[ES] = {
    require(popSize > 0, "Population cannot be empty")
    new PopulationState(for (i <- 0 until popSize) yield genSolution, 0)
  }

}
