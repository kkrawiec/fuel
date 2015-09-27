package scevo.func

import scevo.evo.BestSelector
import scevo.tools.Options
import scevo.tools.TRandom
import scala.annotation.tailrec
import scevo.tools.Random
import org.junit.Test
import scevo.Preamble.RndApply
import scevo.evo.MOrdering
import scevo.evo.WorstSelector

class NSGA2Selection(val tournSize: Int, val removeEvalDuplicates: Boolean, val promoteFrontExtremes: Boolean) {

  def this(opt: Options) = this(
    opt.paramInt("tournamentSize", 7, _ > 1),
    opt.paramString("removeEvalDuplicates").getOrElse("false") == "true",
    opt.paramString("promoteFrontExtremes").getOrElse("false") == "true")

  // Works as a wrapper around the original ES
  case class Wrapper[S, E](val s: (S, E), val rank: Int, val crowding: Int)

  def globalOrdering[S, E] = new Ordering[Wrapper[S, E]] {
    override def compare(a: Wrapper[S, E], b: Wrapper[S, E]) = {
      val c = a.rank compare b.rank
      if (c != 0) c else a.crowding compare b.crowding
    }
  }
  def intraLayerOrdering[S, E] = new Ordering[Wrapper[S, E]] {
    override def compare(a: Wrapper[S, E], b: Wrapper[S, E]) = a.crowding compare b.crowding
  }

  // Phase 1: Build the ranking, calculate crowding, and preserve only top ranks that host the required number of solutions
  // Should be called *once per generation*
  def rank[S, E](numToSelect : Int, po: MOrdering[E]) = {
    // assumes nonempty pop
    pop: Seq[(S, Seq[E])] =>
      {
        //require(numToGenerate <= archive.size + solutions.size)
        // eliminate evaluation duplicates
        val toRank = if (removeEvalDuplicates) pop.groupBy(_._2).map(kv => kv._2(0)).toSeq else pop
        val ranking = paretoRanking(toRank, po)
        var capacity = math.min(toRank.size, numToSelect)
        val fullLayers = ranking.takeWhile(
          r => if (capacity - r.size < 0) false else { capacity -= r.size; true })
        /*
        val top = ranking(0).map(_.s._2).toSet
        println(f"NSGA ranks: ${ranking.size} Top(${top.size}): ${top}")
        println(f"NSGA rsizes: ${ranking map (_.size)} }")
        */
        if (capacity <= 0) fullLayers.flatten // flatten preserves ordering 
        else fullLayers.flatten ++ ranking(fullLayers.size).sorted(intraLayerOrdering[S, Seq[E]]).splitAt(capacity)._1
      }

  }

  // Phase 2: The actual selection, based on the wrapped solutions
  // May be called arbitrarily many times. 
  def apply[S, E ](implicit rand: TRandom) = {
    sel: Seq[Wrapper[S, E]] => BestSelector(sel(rand, tournSize), globalOrdering[S, E]).s
  }

  // Builds the ranking top-down. 
  private def paretoRanking[S, E](solutions: Seq[(S, Seq[E])], po : MOrdering[E]): Seq[Seq[Wrapper[S, Seq[E]]]] = {
    @tailrec def pareto(dominating: Map[Int, Set[Int]], layers: List[Seq[Int]] = List()): List[Seq[Int]] = {
      val (lastLayer, rest) = dominating.partition(_._2.isEmpty)
      val ll = lastLayer.keys.toSeq
      if (rest.isEmpty)
        layers :+ ll
      else
        pareto(rest.map(s => (s._1, s._2.diff(ll.toSet))), layers :+ ll)
    }
    val sols = 0 until solutions.length
    val comparisons = sols.map(i => // i -> outcomesOfComparisonsWith(i)
      (i -> sols.map(o => po.tryCompare(solutions(i)._2,solutions(o)._2))))
    val dominating = comparisons.map({ // i -> dominatedBy(i)
      case (i, cmp) => (i, sols.map(i => (i, cmp(i)))
        .collect({ case (i, Some(1)) => i }).toSet)
    }).toMap
    val identical = comparisons.map({ case (i, cmp) => cmp.count(_.getOrElse(1) == 0) })
    val layers = pareto(dominating).toSeq
    (0 until layers.size).map(i => {
      val lay = layers(i)
      /*
      if (promoteFrontExtremes) {
        val evals = lay.map(j => solutions(j)._2).transpose
        val mins = (0 until evals.size).map(k => BestSelector(evals(k), po.ordering(k)))
        val maxs = (0 until evals.size).map(k => WorstSelector(evals(k), po.ordering(k)))
        lay.map(j => new Wrapper[S, Seq[E]](solutions(j), i, {
          val ev = solutions(j)._2.map(_.v)
          val detExtreme = (0 until ev.size).map(k => (ev(k) - mins(k)) * (ev(k) - maxs(k)))
          if (detExtreme.contains(0)) 0
          else identical(j)
        }))
      } else
      * 
      */
        lay.map(j => new Wrapper[S, Seq[E]](solutions(j), i, identical(j)))
    })
  }

}

/*
final class TestNSGA2 {
  MultiobjectiveEvaluation(List(ScalarEvaluationMax(0),ScalarEvaluationMin(0)))
  def e(o: Seq[Int]) = MultiobjectiveEvaluation(o.map(v => ScalarEvaluationMax(v)))
  @Test def test: Unit = {
    val state = List(
      ('a, e(Seq(2, 3, 3))),
      ('b, e(Seq(3, 3, 1))),
      ('c, e(Seq(2, 2, 1))),
      ('d, e(Seq(1, 2, 2))), // crowding
      ('e, e(Seq(1, 2, 2))),
      ('f, e(Seq(1, 2, 2))),
      ('g, e(Seq(1, 1, 1))))
    val nsga = new NSGA2Selection(10, false, false)
    val ranking = nsga.rank(3)(state) 
    println("Ranking: " + ranking.mkString("\n"))
    println("Selections:")
    val sel = nsga[Symbol, MultiobjectiveEvaluation](new Random)
    for (i <- 0 until 20)
      println(sel(ranking))
  }
}
* 
*/

