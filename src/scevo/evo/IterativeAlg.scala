package scevo.evo

import scevo.tools.Logging

trait Algorithm[S <: State] {
  def run(initialState: S): S
  def currentState: S
}

trait IterativeAlgorithm[ES <: EvaluatedSolution[_ <: Evaluation]] extends Algorithm[PopulationState[ES]] {
//  this : StoppingConditions[IterativeAlgorithm[ES]] =>
  override def currentState: PopulationState[ES]
  // Determining the best in population can be costly for large populations, hence this field
  def bestSoFar: ES
//  def apply(postGenerationCallback: (IterativeAlgorithm[ES] => Unit) = ((_: IterativeAlgorithm[ES]) => ())): State[ES]
}

/* 
 * Iterative search algorithm, with every iteration implemented as SearchStep
 */
trait Evolution[S <: Solution, ES <: EvaluatedSolution[E], E <: Evaluation]
  extends IterativeAlgorithm[ES] with Logging {
  this : SearchStepStochastic[S,ES,E] 
  with StoppingConditions[IterativeAlgorithm[ES]]
  with PostIterationAction[ES] =>

  private var current: PopulationState[ES] = _
  override def currentState = current
  private var best : ES = _
  override def bestSoFar = best

  /* Performs evolutionary run. 
   * Returns the final state of evolutionary process, the best of run solution, and the ideal solution (if found). 
   * PROBABLY can be called multiple times on the same Evolution; that should continue search. 
   *   */
  override def run(initialState: PopulationState[ES]): PopulationState[ES] = {

    current = initialState
    best = BestSelector(current.solutions )

    println("Search process started")
    do {
      var nextStep = apply(Seq(current))
      current = if (nextStep.isEmpty) {
        log(s"Generation ${current.iteration}: None of candidate solutions passed the evaluation stage. Restarting. ")
        PopulationState(initialState.solutions, current.iteration + 1)
      } else {
        val state = nextStep.get
        val bestOfGen = BestSelector(state.solutions)
        if (bestOfGen.eval.betterThan(best.eval)) best = bestOfGen
        state
      }
      postIteration
    } while (stoppingConditions.forall(sc => !sc(this)))

    println("Search process completed")
    current
  }
}

/*
trait EA[S <: Solution, ES <: EvaluatedSolution[E], E <: Evaluation]
extends Evolution[S,ES,E] with SearchStepStochastic[S,ES,E]
with StoppingConditions[Evolution[S,ES,E]]
with PostIterationAction[ES] with Randomness
* 
*/

