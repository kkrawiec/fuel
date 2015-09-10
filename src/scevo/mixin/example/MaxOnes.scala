package scevo.mixin.example

import scevo.mixin.PopulationState
import scevo.evo.ScalarEvaluationMax
import scevo.mixin.Selector
import scevo.mixin.TournamentSelection
import scevo.mixin.EA
import scevo.mixin.Experiment
import scevo.mixin.InitialPopulationState
import scevo.mixin.SeparableEvaluator
import scevo.mixin.StochasticSearchOperators
import scevo.mixin.StoppingDefault
import scevo.tools.Options
import scevo.tools.OptionsFromArgs
import scevo.tools.Randomness
import scevo.tools.Rng

/* Genetic Algorithm */

object GA {

  // Candidate solution (bitstring)
  class B(val v: Vector[Boolean]) {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }

  // Shorthands: 
  type E = ScalarEvaluationMax

  // Fitness function = the number of ones:
  trait GA extends InitialPopulationState[B, E]
    with SeparableEvaluator[B, E] with StochasticSearchOperators[B, E] {
    this: Options with Randomness =>
    val numVars = paramInt("numVars", _ > 0)

    override def randomSolution = new B(Vector.fill(numVars)(rng.nextBoolean))

    override def evaluate(p: B) = ScalarEvaluationMax(p.v.count(b => b))

    // Search operators:  
    override def operators: Seq[Selector[B, E] => Seq[B]] =
      List(
        // One-bit mutation:
        (source => List({
          val s = source.next.s
          val bitToMutate = rng.nextInt(s.v.size)
          new B(s.v.updated(bitToMutate, !s.v(bitToMutate)))
        })),
        // One-point crossover: 
        (source => {
          val me = source.next.s
          val cuttingPoint = rng.nextInt(me.v.size)
          val (myHead, myTail) = me.v.splitAt(cuttingPoint)
          val (hisHead, hisTail) = source.next.s.v.splitAt(cuttingPoint)
          List(new B(myHead ++ hisTail), new B(hisHead ++ myTail))
        }))
  }

  // Compose the components into search algorithm:
  class ExperimentMaxOnes(args: Array[String])
    extends OptionsFromArgs(args) with Rng
    with EA[B, E]
    with GA
    with TournamentSelection[B, E]
    with StoppingDefault[PopulationState[B, E]]
    with Experiment[PopulationState[B, E]] {

    override def stop(s: PopulationState[B, E]) = super.stop(s) || bestSoFar.fold(false)(_.eval.v == numVars)
  }
}

// Run the algorithm:
object MaxOnes {
  def main(args: Array[String]): Unit = new GA.ExperimentMaxOnes(args) {}.launch
}
