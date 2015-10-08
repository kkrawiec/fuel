package scevo.func.example

import scevo.func.Experiment
import scevo.tools.Rng
import scevo.tools.OptColl
import scevo.func.SimpleEA
import scevo.domain.PermutationMoves
import scevo.func.SearchOperator1
import scevo.domain.VectorMoves
import scevo.tools.TRandom
import scevo.tools.OptCollRng

/**  Continuous optimization: Rosenbrock function. 
 *   
  * Minimized fitness function.
  */


class DoubleVectorMoves(numVars: Int, sigma: Double)(implicit rng: TRandom)
    extends VectorMoves[Double](numVars)(rng) {

  override def newSolution = IndexedSeq.fill(numVars)(rng.nextDouble)

  override def oneBitMutation = SearchOperator1((p: IndexedSeq[Double]) => {
    val xiToMutate = rng.nextInt(numVars)
    p.updated(xiToMutate, p(xiToMutate) + sigma*(rng.nextDouble-0.5))
  })
}


object Rosenbrock {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--n 3 --maxGenerations 300")

    val n = opt.paramInt("n", _ > 0)
    def rosenbrock(x : Seq[Double]) = Range(0,n-1).map( i => 
      (1-x(i))*(1-x(i)) + 100 * math.pow(x(i+1)-x(i)*x(i),2) ).sum

    Experiment.run(SimpleEA(new DoubleVectorMoves(n, 0.001), rosenbrock))
  }
}


