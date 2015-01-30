package scevo.evo

import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.Randomness

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
      override def iteration = s.iteration + 1
      override def p1 = pipe1.step(s.p1)
      override def p2 = pipe2.step(s.p2)
    }
  }
}

trait CoEA[S1 <: Solution, E1 <: Evaluation, S2 <: Solution, E2 <: Evaluation]
  extends Options with Randomness with Collector
  with IterativeAlgorithm[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  with InitialState[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  with SearchStepCoev[S1, E1, S2, E2]
  with StoppingDefault[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  //  with PostBestSoFar[S, E]
  with Epilogue[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
  with Experiment[TwoPopState[PopulationState[S1, E1], PopulationState[S2, E2]]]
//  this: Pipeline[S1, E1] with Pipeline[S2, E2] =>

