package scevo.evo

import scevo.tools.TRandom

object BestSelector {
  def apply[ES <: EvaluatedSolution](set: Seq[ES]): ES = {
    require(set.nonEmpty)
    var best = set(0)
    set.tail.foreach(e => if (e.betterThan(best).getOrElse(false)) best = e)
    best
  }
}

trait Selection[+ES <: EvaluatedSolution] {
  def apply[T >: ES <: EvaluatedSolution](pool: Seq[T], numToGenerate: Int, previous: Seq[T]): Seq[T]
}

class TournamentSelection[ES <: EvaluatedSolution](tournSize: Int, rng: TRandom)
  extends Selection[ES] {

  require(tournSize >= 2, "Tournament size has to be at least 2")

  // ignores previous
  override def apply[T >: ES <: EvaluatedSolution](pool: Seq[T], numToGenerate: Int, previous: Seq[T]): Seq[T] =
    (0 until numToGenerate).map(_ => {
      val participants = for (i <- 0 until tournSize) yield pool(rng.nextInt(pool.length))
      BestSelector.apply(participants)
    })
}

class GreedyBestSelection[ES <: EvaluatedSolution] extends Selection[ES] {

  override def apply[T >: ES <: EvaluatedSolution](pool: Seq[T], numToGenerate: Int, previous: Seq[T]): Seq[T] = {
    require(numToGenerate == 1)
    Seq(BestSelector.apply(pool))
  } ensuring (_.size == 1)
}

/* Caveat: this is not an exact implementation of 1+1, because this framework cannot implement backtracking yet 
 * 
 */
class OnePlusOneSelection[ES <: EvaluatedSolution] extends Selection[ES] {

  override def apply[T >: ES <: EvaluatedSolution](pool: Seq[T], numToGenerate: Int, previous: Seq[T]): Seq[T] = {
    require(numToGenerate == 1)
    println(pool.head)
    if (previous.nonEmpty) println(previous.head)
    Seq(BestSelector.apply(pool ++ previous))
  } ensuring (_.size == 1)
}
