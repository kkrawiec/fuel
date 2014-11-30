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

/* Genetic Algorithm */

object GA {

  class B(val v: Vector[Boolean]) extends Solution {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  type E = ScalarEvaluationMax

  trait GA extends InitialPopulationState[B, E]
    with SeparableEvalutator[B, E] with StochasticSearchOperators[B, E] {
    this: Options with Randomness =>
    val numVars = paramInt("numVars", _ > 0)

    override def randomSolution = new B(Vector.fill(numVars)(rng.nextBoolean))

    override def evaluate(p: B) = ScalarEvaluationMax(p.v.count(b => b))

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

  /* Use cases: */

  class ExperimentMaxOnes(args: Array[String])
    extends OptionsFromArgs(args) with Rng
    with EA[B, E]
    with GA
    with TournamentSelection[B, E]
    with StoppingStd[PopulationAlgorithm[B, E]]
    with Experiment[PopulationState[B, E]] {

    override def stoppingConditions = super.stoppingConditions :+
      new StoppingCondition[PopulationAlgorithm[B, E]] {
        def apply(a: PopulationAlgorithm[B, E]) = bestSoFar.get.eval.v == numVars
      }
  }
}

  object MaxOnes {
    def main(args: Array[String]): Unit = new GA.ExperimentMaxOnes(args) {}.launch
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