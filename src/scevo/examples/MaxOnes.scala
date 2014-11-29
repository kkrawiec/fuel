package scevo.examples

import scevo.evo.EA
import scevo.evo.EpilogueBestOfRun
import scevo.evo.EvaluatedSolution
import scevo.evo.Evaluator
import scevo.evo.Evolution
import scevo.evo.Experiment
import scevo.evo.InitialState
import scevo.evo.PopulationAlgorithm
import scevo.evo.PopulationState
import scevo.evo.PostBestSoFar
import scevo.evo.ScalarEvaluation
import scevo.evo.ScalarEvaluationMax
import scevo.evo.SearchStepStochastic
import scevo.evo.Selector
import scevo.evo.Solution
import scevo.evo.StochasticSearchOperators
import scevo.evo.StoppingCondition
import scevo.evo.StoppingStd
import scevo.evo.TournamentSelection
import scevo.tools.Options
import scevo.tools.OptionsFromArgs
import scevo.tools.Randomness
import scevo.tools.Rng
import scevo.tools.TRandom
import scevo.evo.ESol
import scevo.evo.SeparableEvalutator
import scevo.evo.InitialPopulationState

object GA {

  class B(val v: Seq[Boolean]) extends Solution {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  type E = ScalarEvaluationMax

  trait GASearchOperators extends StochasticSearchOperators[B, E] {
    this: Options with Randomness =>
    override def operators: Seq[Selector[B, E] => Seq[B]] =
      List(
        (source => List({
          val s = source.next.s
          val bitToMutate = rng.nextInt(s.v.size)
          new B(s.v.updated(bitToMutate, !s.v(bitToMutate)))
        })),
        (source => {
          val me = source.next.s
          val cuttingPoint = rng.nextInt(me.v.size)
          val (myHead, myTail) = me.v.splitAt(cuttingPoint)
          val (hisHead, hisTail) = source.next.s.v.splitAt(cuttingPoint)
          List(new B(myHead ++ hisTail), new B(hisHead ++ myTail))
        }))
  }
  trait Eval extends SeparableEvalutator[B, E] {
    def evaluate(p: B) = ScalarEvaluationMax(p.v.count(b => b))
  }
  trait Init extends InitialPopulationState[B, E] {
    this: Options with Randomness with Eval =>
    val numVars = paramInt("numVars", _ > 0)
    override def randomSolution = new B(List.fill(numVars)(rng.nextBoolean))
  }

  /* Use cases:
 */
  class MyConfig(args: Array[String])
    extends OptionsFromArgs(args) with Init with GASearchOperators with Eval with Rng

  class ExperimentMaxOnes3(args: Array[String])
    extends OptionsFromArgs(args) with Rng
    with EA[B, E]
    with Init
    with Eval
    with GASearchOperators
    with TournamentSelection[B,E]
    with StoppingStd[PopulationAlgorithm[B,E]]
    with Experiment[PopulationState[B,E]] {

    override def stoppingConditions = super.stoppingConditions :+
      new StoppingCondition[PopulationAlgorithm[B,E]] {
        def apply(a: PopulationAlgorithm[B,E]) = bestSoFar.get.eval.v == numVars
      }
  }

  class ExperimentMaxOnes2(args: Array[String])
    extends OptionsFromArgs(args) with Rng
    with EA[B,E]
    with Init
    with GASearchOperators
    with Eval
    with TournamentSelection[B,E]
    with StoppingStd[PopulationAlgorithm[B,E]]
    with Experiment[PopulationState[B,E]]

  class ExperimentMaxOnes(args: Array[String])
    extends OptionsFromArgs(args) with Rng
    with Init
    with Evolution[B,E]
    with GASearchOperators
    with SearchStepStochastic[B,E]
    with Eval
    with PostBestSoFar[B,E]
    with EpilogueBestOfRun[B,E]
    with TournamentSelection[B,E]
    with StoppingStd[PopulationAlgorithm[B,E]]
    // Why is this not working:?
    //  with StoppingStd[Evolution[BitVector, BitVectorEvaluated, ScalarEvaluation]]
    with Experiment[PopulationState[B,E]]

  /* Genetic Algorithm
 */
  object ExperimentMaxOnesGA {
    def main(args: Array[String]): Unit = new ExperimentMaxOnes3(args) {
    }.launch
  }

}
/* Stochastic local search: 
object ExperimentMaxOnesSLS {
  def main(args: Array[String]): Unit = new ExperimentMaxOnes(args) {
    require(populationSize == 1)
    //  override def selection = new OnePlusOneSelection[BitVectorEvaluated]
    override def selection = new GreedyBestSelection[BitVectorEvaluated, ScalarEvaluation]
  }.launch
}

final class TestExperiment {
  // val params = "--seed 8 --populationSize 100 --maxTime 300000 --maxGenerations 1000 --operatorProbs 0.5,0.5 --numVars 200"
  val params = "--numVars 200  --tournamentSize 7  --operatorProbs 0.5,0.5"
  @Test
  def testExperimentMaxOnesGA =
    ExperimentMaxOnesGA.main(params.split("\\s+"))

  @Test
  def testExperimentMaxOnesSLS =
    ExperimentMaxOnesSLS.main((params + " --populationSize 1").split("\\s+"))
}
 */