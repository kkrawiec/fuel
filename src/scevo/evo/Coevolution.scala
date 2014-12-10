package scevo.evo

import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.OptionsFromArgs
import scevo.tools.Randomness
import scevo.tools.Rng

trait TwoPopState[PS1 <: PopulationState[_, _], PS2 <: PopulationState[_, _]]
  extends State {
  def p1: PS1
  def p2: PS2
}

trait Pipeline[S <: Solution, E <: Evaluation]
  extends Options with Randomness
  with SearchStepStochastic[S, E]
  with Selection[S, E]
  with Evaluator[S, E]
  with StochasticSearchOperators[S, E]

trait SearchStepCoev[S1 <: Solution, E1 <: Evaluation, S2 <: Solution, E2 <: Evaluation]
  extends Step[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]] {
  def pipe1: Pipeline[S1, E1]
  def pipe2: Pipeline[S2, E2]
  type TPS = TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]
  override def step(state: TPS) = {
    val s = super.step(state)
    new TPS {
      override def iteration = s.iteration
      override def p1 = pipe1.step(s.p1)
      override def p2 = pipe2.step(s.p2)
    }
  }
}

trait CoEA[S1 <: Solution, E1 <: Evaluation, S2 <: Solution, E2 <: Evaluation]
  extends Options with Randomness with Collector
  with InitialState[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  with IterativeAlgorithm[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  with SearchStepCoev[S1, E1, S2, E2]
  with StoppingDefault[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  //  with PostBestSoFar[S, E]
  with Epilogue[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
with Experiment[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
//  this: Pipeline[S1, E1] with Pipeline[S2, E2] =>

object NumberGame {

  class B(val v: Vector[Boolean]) extends Solution {

    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }
  type E = ScalarEvaluationMax

  trait Pipe extends Pipeline[B, E]
    with InitialPopulationState[B, E]
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

  class NG(args: Array[String]) extends OptionsFromArgs(args) with Rng
    with CoEA[B, E, B, E] {

    override def initialState = new TwoPopState[PopulationState[B, E], PopulationState[B, E]] {
      override val iteration = 0
      override def p1 = pipe1.initialState
      override def p2 = pipe2.initialState
    }

    // TODO: config file prefix
    override def pipe1 = new OptionsFromArgs(args) with Rng with Pipe with TournamentSelection[B, E]
    override def pipe2 = new OptionsFromArgs(args) with Rng with Pipe with TournamentSelection[B, E]
  }
  object NG {
    def main(args: Array[String]) : Unit = new NG(args).launch
  }
}
