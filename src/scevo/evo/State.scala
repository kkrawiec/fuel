package scevo.evo

/* Search state. Not to be confused with the state of the search *algorithm/process*, 
 */

trait State extends Serializable {
  def iteration: Int
}

trait InitialState[S <: State] {
  def initialState: S
}

class PopulationState[ES <: EvaluatedSolution[_]](val solutions: Seq[ES], override val iteration: Int)
  extends State {
  require(solutions.size > 0, "The set of working solutions in a state cannot be empty")
}

object PopulationState {

  def apply[ES <: EvaluatedSolution[_]](solutions: Seq[ES], iteration: Int = 0) =
    new PopulationState[ES](solutions, iteration)

  def apply[ES <: EvaluatedSolution[_]](popSize: Int, genSolution: () => ES): PopulationState[ES] = {
    require(popSize > 0, "Population cannot be empty")
    PopulationState(for (i <- 0 until popSize) yield genSolution(), 0)
  }
}
