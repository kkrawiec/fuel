package scevo.examples

import scevo.evo.CoEA
import scevo.evo.Pipeline
import scevo.evo.PopulationState
import scevo.evo.ScalarEvaluationMax
import scevo.evo.TournamentSelection
import scevo.evo.TwoPopState
import scevo.tools.OptionsFromArgs
import scevo.tools.Rng

object NumberGame {

  type E = ScalarEvaluationMax

  trait Pipe extends Pipeline[GA.B, E] with GA.GA

  class NG(args: Array[String]) extends OptionsFromArgs(args) with Rng
    with CoEA[GA.B, E, GA.B, E] {

    override def initialState = new TwoPopState[PopulationState[GA.B, E], PopulationState[GA.B, E]] {
      override val iteration = 0
      override def p1 = pipe1.initialState
      override def p2 = pipe2.initialState
    }

    // TODO: config file prefix
    override def pipe1 = new OptionsFromArgs(args) with Rng with Pipe with TournamentSelection[GA.B, E]
    override def pipe2 = new OptionsFromArgs(args) with Rng with Pipe with TournamentSelection[GA.B, E]
  }

}
object NG {
  def main(args: Array[String]) { new NumberGame.NG(args).launch }
}
