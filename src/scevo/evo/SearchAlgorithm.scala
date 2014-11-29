package scevo.evo

import scevo.tools.Options
import scevo.tools.Randomness
import scevo.tools.ResultDatabase
import scevo.tools.Logger

trait Algorithm[S <: State] {
  def run(rdb: ResultDatabase): S
}

trait IterativeAlgorithm[S <: State] extends Algorithm[S] {
  this: InitialState[S] =>
  def currentState: S
}

trait PopulationAlgorithm[S <: Solution, E <: Evaluation] extends IterativeAlgorithm[PopulationState[S, E]] {
  this: InitialState[PopulationState[S, E]] =>
  override def currentState: PopulationState[S, E]
}

trait PostIterationAction[S <: Solution, E <: Evaluation] {
  def postIteration: Unit
}

trait PostBestSoFar[S <: Solution, E <: Evaluation] extends PostIterationAction[S, E] {
  this: PopulationAlgorithm[S, E] =>
  protected var best: Option[EvaluatedSolution[S, E]] = None
  def bestSoFar: Option[EvaluatedSolution[S, E]] = best
  override def postIteration: Unit = {
    val bestOfGen = BestSelector(currentState.solutions)
    if (bestSoFar.isEmpty || bestOfGen.eval.betterThan(best.get.eval)) best = Some(bestOfGen)
    println(f"Generation: ${currentState.iteration}  BestSoFar: ${bestSoFar.get.eval} ${bestSoFar.get}")
  }
}

trait Epilogue[S <: Solution, E <: Evaluation] {
  def epilogue(rdb: ResultDatabase): Unit
}

trait EpilogueBestOfRun[S <: Solution, E <: Evaluation] extends Epilogue[S, E] {
  this: PostBestSoFar[S, E] =>
  def epilogue(rdb: ResultDatabase): Unit = {
    rdb.setResult("bestOfRun.fitness", bestSoFar.get.eval)
    rdb.setResult("bestOfRun.genotype", bestSoFar.toString)
    rdb.write("bestOfRun", bestSoFar)
  }
}

/* 
 * Iterative search algorithm, with every iteration implemented as SearchStep
 */
trait Evolution[S <: Solution, E <: Evaluation]
  extends PopulationAlgorithm[S, E] with Logger {
  this: SearchStepStochastic[S, E] with StoppingConditions[PopulationAlgorithm[S, E]] with PostIterationAction[S, E] with InitialState[PopulationState[S, E]] with Epilogue[S, E] with Options =>

  private var current: PopulationState[S, E] = _
  override def currentState = current
  val snapFreq = paramInt("snapshot-frequency", 0)

  /* Performs evolutionary run. 
   * Returns the final state of evolutionary process, the best of run solution, and the ideal solution (if found). 
   * PROBABLY can be called multiple times on the same Evolution; that should continue search. 
   *   */
  override def run(rdb: ResultDatabase): PopulationState[S, E] = {
    current = initialState
    println("Search process started")
    do {
      current = apply(Seq(current)).getOrElse({
        log("error", s"Generation ${current.iteration}: None of candidate solutions passed the evaluation stage. Restarting. ")
        PopulationState(initialState.solutions, current.iteration + 1)
      })
      postIteration
      if (snapFreq > 0 && current.iteration % snapFreq == 0)
        rdb.saveSnapshot(f"${current.iteration}%04d")
    } while (stoppingConditions.forall(sc => !sc(this)))
    println("Search process completed")
    rdb.setResult("lastGeneration", current.iteration)
    epilogue(rdb)
    current
  }
}

trait EA[S <: Solution, E <: Evaluation]
  extends Evolution[S, E]
  with Options with Randomness
  with InitialState[PopulationState[S, E]]
  with SearchStepStochastic[S, E]
  with Selection[S, E]
  with Evaluator[S, E]
  with StochasticSearchOperators[S, E]
  with StoppingConditions[PopulationAlgorithm[S, E]]
  with PostBestSoFar[S, E]
  with EpilogueBestOfRun[S, E]
