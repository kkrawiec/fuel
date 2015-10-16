package scevo.example

import scevo.moves.BoolVectorMoves
import scevo.util.Env
import scevo.func.SimpleEA

/** Example of programmatic manipuation of components. 
 *  
 *  This program runs EA for 7 nonempty combinations of three basic search operators
 *  defined in VectorMoves, and prints out the fitness values of the best-of-run solutions.  
 *  
 */
object Programmatic {
  def main(args: Array[String]) {

    new Env('n -> 32, 'trivial -> false, 'maxGenerations -> 10) {
      val n = opt('n, (_: Int) > 0)

      val prob = new HiffProblem(n, opt('trivial, false))

      val moves = BoolVectorMoves(n)
      // Generate all subsets and drop the first one - the emtpy set
      val comb = moves.moves.toSet.subsets.toList.tail

      val best = for (c <- comb) yield {
        println(s"Combination $c")
        val ga = SimpleEA(moves = new BoolVectorMoves(n) { override def moves = c.toSeq },
          eval = prob.hiff _, prob.maxEval)(opt, coll, rng,
            ordering = Ordering[Int].reverse)
        ga()
        ga.bsf.bestSoFar.get
        s"Combination: $c  Best fitness: ${ga.bsf.bestSoFar.get._2}"
      }
      println(best.mkString("\n"))
    }
  }
}