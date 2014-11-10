package scevo.evo

import scala.annotation.tailrec
import org.junit.Test
import scevo.Preamble.RndApply
import scevo.tools.Random
import scevo.tools.TRandom
import scevo.tools.Randomness
import scevo.tools.Options

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
 * 
 * BTW note that with discrete objectives, the odds for having the same multiobjective 
 * evaluation are quite high (also between the new solutions and the archive). 
 * 
 * Note: By being statefull (archive), this selection method assumes that the new solutions 
 * and the ones in the archive can be sensibly compared. 
 */

object NSGA {

  class Eval(val rank: Int, val crowding: Int) extends Evaluation {
    def comparePartial(that: Evaluation): Option[Int] = {
      val other = that.asInstanceOf[Eval]
      val rankCmp = rank.compare(other.rank)
      Some(if (rankCmp != 0) -rankCmp
      else -crowding.compare(other.crowding))
    }
  }
  // Works as a wrapper around the original ES
  class Sol[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation](val s: ES, val eval: Eval) extends EvaluatedSolution[Eval]

  /* No-archive, memoryless variant of NSGA. Selection works on the current population only 
 */
  protected trait NoArchive[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation]
    extends Selection[ES] {

    def rng: TRandom
    def numToGenerate: Int
    def tournSize: Int

    def archive = Seq[ES]() // fixed, won't change
    override def selector(history: Seq[PopulationState[ES]]) = new NSGASelector(history)

    class NSGASelector(history: Seq[PopulationState[ES]]) extends Selector[ES] {
      require(numToGenerate <= archive.size + history.head.solutions.size)
      val l = archive ++ history.head.solutions
      val ranking = paretoRanking(archive ++ history.head.solutions)
      var capacity = numToGenerate
      val fullLayers = ranking.takeWhile(
        r => if (capacity - r.size < 0) false else { capacity -= r.size; true })
      val selected = if (capacity <= 0) fullLayers.flatten // flatten preserves ordering
      else fullLayers.flatten ++ ranking(fullLayers.size).sortBy(_.eval.crowding).splitAt(capacity)._1
      override def next = BestSelector(selected(rng, tournSize)).s
      override val numSelected = numToGenerate
    }

    private def paretoRanking(solutions: Seq[ES]): Seq[Seq[Sol[ES, F]]] = {
      @tailrec def pareto(dominating: Map[Int, Set[Int]], layers: List[Seq[Int]] = List()): List[Seq[Int]] = {
        val (lastLayer, rest) = dominating.partition(e => e._2.isEmpty)
        val ll = lastLayer.keys.toSeq
        if (rest.isEmpty)
          ll :: layers
        else
          pareto(rest.map(s => (s._1, s._2.diff(ll.toSet))), ll :: layers)
      }
      val sols = 0 until solutions.length
      val comparisons = sols.map(i => // i -> outcomesOfComparisonsWith(i)
        (i -> sols.map(o => solutions(i).eval.comparePartial(solutions(o).eval))))
      val dominating = comparisons.map({ // i -> dominatedBy(i)
        case (i, cmp) => (i, sols.map(i => (i, cmp(i)))
          .collect({ case (i, Some(1)) => i }).toSet)
      }).toMap
      val identical = comparisons.map({ case (i, cmp) => cmp.count(_.getOrElse(1) == 0) })
      val layers = pareto(dominating).toSeq
      (0 until layers.size).map(i =>
        layers(i).map(j => new Sol[ES, F](solutions(j), new Eval(i, identical(j)))))
    }
  }

  /* The conventional NSGA: the archive stores the selected solutions and is merged
 * with the next population prior to selection. 
 */
  protected trait WithArchive[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation]
    extends NoArchive[ES, F] {
    var arch = Seq[ES]()
    override def archive = arch
    // Note: calling selector() changes the state of archive
    override def selector(history: Seq[PopulationState[ES]]) = {
      val sel = super.selector(history)
      arch = sel.selected.map(_.s)
      sel
    }
  }

  /* Convenience objects and traits 
 * 
 */
  trait ParamProvider {
    this: Options =>
    val numToGenerate = paramInt("populationSize")
    val tournSize = paramInt("tournamentSize", 7, _ > 1)
  }

  object NoArchive {
    def apply[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation](
      numToGen: Int, tournamentSize: Int, rn: TRandom) =
      new {
        val (rng, numToGenerate, tournSize) = (rn, numToGen, tournamentSize)
      } with NoArchive[ES, F]
  }

  trait NoArchiveMixin[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation]
    extends NoArchive[ES, F] with ParamProvider {
    this: Options with Randomness =>
  }

  object WithArchive {
    def apply[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation](
      numToGen: Int, tournamentSize: Int, rn: TRandom) =
      new {
        val (rng, numToGenerate, tournSize) = (rn, numToGen, tournamentSize)
      } with WithArchive[ES, F]
  }
  trait WithArchiveMixin[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation]
    extends WithArchive[ES, F] with ParamProvider {
    this: Options with Randomness =>
  }
}

final class TestNSGA {
  // maximized
  class S(o: Seq[Int]) extends EvaluatedSolution[MultiobjectiveEvaluation] {
    override def eval = MultiobjectiveEvaluation(o.map(v => ScalarEvaluationMax(v)))
    override def toString = o.toString
  }
  @Test
  def test: Unit = {
    val nsga = NSGA.WithArchive[S, MultiobjectiveEvaluation](5, 3, new Random)
    val state = new PopulationState(List(
      new S(Seq(2, 3, 3)),
      new S(Seq(3, 3, 1)),
      new S(Seq(2, 2, 1)),
      new S(Seq(1, 2, 2)), // crowding
      new S(Seq(1, 2, 2)),
      new S(Seq(1, 2, 2)),
      new S(Seq(1, 1, 1))), 0)
    val sel = nsga.selector(Seq(state))
    for (i <- 0 until 20)
      println(sel.next)
  }
  @Test
  def test2: Unit = {
    val a = new NSGA.Eval(1, 3) 
    val b = new NSGA.Eval(2, 3) 
    val c = new NSGA.Eval(2, 1) 
    println( a.comparePartial(b) )
    println( a.comparePartial(c) )
    println( b.comparePartial(c) )
    println( c.comparePartial(b) )
    println( c.comparePartial(c) )
  }
 }  

