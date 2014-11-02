package scevo.evo

import scevo.tools.Logging
import scevo.tools.Options
import scevo.tools.Randomness
import scevo.tools.ResultDatabase

trait Algorithm[S <: State] {
  def run(rdb: ResultDatabase): S
}

trait IterativeAlgorithm[S <: State] extends Algorithm[S] {
  this: InitialState[S] =>
  def currentState: S
}

trait PopulationAlgorithm[ES <: EvaluatedSolution[_]] extends IterativeAlgorithm[PopulationState[ES]] {
  this: InitialState[PopulationState[ES]] =>
  override def currentState: PopulationState[ES]
  // Determining the best in population can be costly for large populations, hence this field
  def bestSoFar: ES
}

trait PostIterationAction[ES <: EvaluatedSolution[_ <: Evaluation]] {
  this: PopulationAlgorithm[ES] =>
  def postIteration: Unit =
    println(f"Generation: ${currentState.iteration}  BestSoFar: ${bestSoFar.eval}")
}

/* 
 * Iterative search algorithm, with every iteration implemented as SearchStep
 */
trait Evolution[S <: Solution, ES <: EvaluatedSolution[_ <: Evaluation]]
  extends PopulationAlgorithm[ES] with Logging {
  this: SearchStepStochastic[S, ES] with StoppingConditions[PopulationAlgorithm[ES]] with PostIterationAction[ES] with InitialState[PopulationState[ES]] =>

  private var current: PopulationState[ES] = _
  override def currentState = current
  private var best: ES = _
  override def bestSoFar = best

  /* Performs evolutionary run. 
   * Returns the final state of evolutionary process, the best of run solution, and the ideal solution (if found). 
   * PROBABLY can be called multiple times on the same Evolution; that should continue search. 
   *   */
  override def run(rdb: ResultDatabase): PopulationState[ES] = {

    current = initialState
    best = BestSelector(current.solutions)

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
    rdb.setResult("lastGeneration", current.iteration)
    rdb.setResult("bestOfRun.fitness", bestSoFar.eval)
    rdb.setResult("bestOfRun.genotype", bestSoFar.toString)
    current
  }
}

trait EA[S <: Solution, ES <: EvaluatedSolution[_ <: Evaluation]]
  extends Evolution[S, ES]
  with Options
  with InitialState[PopulationState[ES]]
  with SearchStepStochastic[S, ES]
  with Selection[ES]
  with Evaluator[S, ES]
  with StochasticSearchOperators[ES, S]
  with StoppingConditions[PopulationAlgorithm[ES]]
  with PostIterationAction[ES] with Randomness

