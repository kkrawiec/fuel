package scevo.evo

import scevo.tools.TRandom
import scevo.Preamble._
import scala.collection.mutable.MutableList

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

    archive = selected.map(_.s)

    override def next = BestSelector(selected(rng, tournSize).map(_.s))
    override val numSelected = numToGenerate
  }

  private class NSGAEval(val rank: Int, val crowding: Int) extends Evaluation {
    def comparePartial(that: Evaluation): Option[Int] = {
      val other = that.asInstanceOf[NSGAEval]
      val rankCmp = rank.compare(other.rank)
      Some(if (rankCmp != 0) -rankCmp
      else -crowding.compare(other.crowding))
    }
  }
  // Works as a wrapper around the original ES
  private class NSGASol(val s: ES, val eval: NSGAEval) extends EvaluatedSolution[NSGAEval]

  // TODO: treat indiscernibility and incomparability in the same way
  private def paretoRanking(solutions: Seq[ES]): Seq[Set[NSGASol]] = {
    val n = solutions.length
    var cmp = Array.ofDim[Option[Int]](n, n) // None means incomparable
    for (i <- 0 until n) {
      for (j <- 0 until i) {
        val v = solutions(i).eval.comparePartial(solutions(j).eval)
        cmp(i)(j) = v
        cmp(j)(i) = if (v.isEmpty) None else Some(-v.get)
      }
      cmp(i)(i) = Some(0)
    }
    var layers = MutableList[Set[ES]]()
    var remaining = (0 until n).toSet
    while (!remaining.isEmpty) {
      //      println( "aa: " + remaining.size )
      val dominated = remaining.filter(i =>
        remaining.find(j => cmp(i)(j).getOrElse(1) < 0).isDefined)
      val rank = remaining -- dominated
      layers = (layers.:+(rank.map(e => solutions(e))))
      remaining = remaining -- rank
    }
    //    println( "Total: " + layers.map( _.size ).sum ) 
    val crowding = (0 until n).map(i =>
      (solutions(i), cmp(i).count(e => e.contains(0)))).toMap
    (0 until layers.size).map(i =>
      layers(i).map(s => new NSGASol(s, new NSGAEval(i, crowding(s)))))
  }
}