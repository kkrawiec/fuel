package scevo.evo

import scevo.tools.TRandom
import scevo.Preamble._
import scala.collection.mutable.MutableList
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec

/* Implements (with modifications) the NSGAII selection algorithm by Deb et al. 
 * Solutions are Pareto-ranked, and are selected by running tournament selection 
 * on ranks. 
 * 
 * Modification w.r.t. Deb et al.:
 * To resolve the ties between ranks, Deb et al. use 'sparsity', a measure based on 
 * the hypervolume between the neighboring solutions in the same Pareto layer. 
 * This implementation replaces sparsity with crowding, which is calculated in a 
 * discrete way: solution's crowding is the number of solutions with this very genotype. 
 * So what matters for crowding is only how many identical solutions are there, not
 * how far they are spaced in the Pareto layer. 
 */

class NSGASelection[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation](
  val numToGenerate: Int, val tournSize: Int, rng: TRandom)
  extends Selection[ES, F] {

  var archive = Seq[ES]()

  // Note: calling selector() changes the state of archive
  override def selector(history: Seq[State[ES]]) = new Selector[ES, F] {
    require(numToGenerate <= archive.size + history.head.solutions.size)

    val ranking = paretoRanking((archive ++ history.head.solutions))
    var capacity = numToGenerate
    val fullLayers = ranking.takeWhile(r => if (capacity < numToGenerate) false else { capacity -= r.size; true })

    val selected = fullLayers.flatten ++ ( // flatten preserves ordering
      if (capacity <= 0)
        None
      else // the least crowded (i.e, most sparse) individuals from the partial rank
        ranking(fullLayers.size).toSeq.sortBy(_.eval.crowding).splitAt(capacity)._1)

    archive = selected.map(_.eval.s)

    override def next = BestSelector(selected(rng, tournSize).map(_.eval.s))
    override val numSelected = numToGenerate
  }

  private class NSGAEval(val rank: Int, val s: ES, val sols: Seq[ES]) extends Evaluation {
    lazy val crowding = sols.count(_ equals s)
    def comparePartial(that: Evaluation): Option[Int] = {
      val other = that.asInstanceOf[NSGAEval]
      val rankCmp = rank.compare(other.rank)
      Some(if (rankCmp != 0) -rankCmp
      else -crowding.compare(other.crowding))
    }
  }
  // Works as a wrapper around the original ES
  private class NSGASol(val eval: NSGAEval) extends EvaluatedSolution[NSGAEval]

  // TODO: treat indiscernibility and incomparability in the same way
  private def paretoRanking(solutions: Seq[ES]): Seq[Seq[NSGASol]] = {
    @tailrec def reversePareto(dominating: Map[Int, Set[Int]], layers: List[Seq[Int]] = List()): List[Seq[Int]] = {
      val (lastLayer, rest) = dominating.partition(e => e._2.isEmpty)
      val ll = lastLayer.keys.toSeq
      if (rest.isEmpty)
        ll :: layers
      else
        reversePareto(rest.map(s => (s._1, s._2.diff(ll.toSet))), ll :: layers)
    }
    val sols = 0 until solutions.length
    val dominating = sols.map(i =>
      (i, sols.filter(o =>
        solutions(i).eval.comparePartial(solutions(o).eval).getOrElse(0) > 0).toSet)).toMap
    val layers = reversePareto(dominating).toSeq.reverse
    (0 until layers.size).map(i =>
      layers(i).map(j => new NSGASol(new NSGAEval(i, solutions(j), solutions))))
  }
}  
