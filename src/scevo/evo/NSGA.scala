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
  // Works as a wrapper around the original ES
  class Wrapper[ES <: EvaluatedSolution[_,F], F <: MultiobjectiveEvaluation](val s: ES, val rank: Int, val sparsity: Int)

  trait DefaultOrdering[S <: Solution, F <: MultiobjectiveEvaluation] {
    type ES = EvaluatedSolution[S,F]
    def globalOrdering = new Ordering[Wrapper[ES, F]] {
      override def compare(a: Wrapper[ES, F], b: Wrapper[ES, F]) = {
        val c = a.rank compare b.rank
        if (c != 0) c else a.sparsity compare b.sparsity
      }
    }
    def intraLayerOrdering = new Ordering[Wrapper[ES, F]] {
      override def compare(a: Wrapper[ES, F], b: Wrapper[ES, F]) = a.sparsity compare b.sparsity
    }
  }

  /* No-archive, memoryless variant of NSGA. Selection works on the current population only 
 */
  protected trait NoArchive[S <: Solution, F <: MultiobjectiveEvaluation]
    extends Selection[S,F] {
    this: DefaultOrdering[S, F] =>

    def rng: TRandom
    def numToGenerate: Int
    def tournSize: Int

    def archive = Seq[ES]() // fixed, won't change
    override def selectorSol(solutions: Seq[EvaluatedSolution[S,F]]) = new NSGASelector(solutions)

    class NSGASelector(solutions: Seq[EvaluatedSolution[S,F]]) extends Selector[S,F] {
      require(numToGenerate <= archive.size + solutions.size)
      val l = archive ++ solutions
      val ranking = paretoRanking(archive ++ solutions)
      var capacity = numToGenerate
      val fullLayers = ranking.takeWhile(
        r => if (capacity - r.size < 0) false else { capacity -= r.size; true })
      val selected = if (capacity <= 0) fullLayers.flatten // flatten preserves ordering 
      else fullLayers.flatten ++ ranking(fullLayers.size).sorted(intraLayerOrdering).splitAt(capacity)._1
      override def next = BestSelector(selected(rng, tournSize), globalOrdering).s
      override val numSelected = numToGenerate
    }

    // Builds the ranking bottom up. Only pointers to dominated solutions are necessary (the counters 
    // of dominating solutions, as used in Deb's paper, are not needed).  
    // On the other hand, bottom-up requires building the *entire* ranking (while regular NSGA ranks
    // roughly half of the solutions).
    private def paretoRanking(solutions: Seq[ES]): Seq[Seq[Wrapper[ES, F]]] = {
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
        layers(i).map(j => new Wrapper[ES, F](solutions(j), i, identical(j))))
    }
  }

  /* The conventional NSGA: the archive stores the selected solutions and is merged
 * with the next population prior to selection. 
 */
  protected trait WithArchive[S <: Solution, F <: MultiobjectiveEvaluation]
    extends NoArchive[S, F] {
    this: DefaultOrdering[S, F] =>
    var arch = Seq[ES]()
    override def archive = arch
    // Note: calling selector() changes the state of archive
    override def selectorSol(solutions: Seq[EvaluatedSolution[S,F]]) = {
      val sel = super.selectorSol(solutions)
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
    def apply[S <: Solution, F <: MultiobjectiveEvaluation](
      numToGen: Int, tournamentSize: Int, rn: TRandom) =
      new {
        val (rng, numToGenerate, tournSize) = (rn, numToGen, tournamentSize)
      } with NoArchive[S, F] with DefaultOrdering[S, F]
  }

  trait NoArchiveMixin[S <: Solution, F <: MultiobjectiveEvaluation]
    extends NoArchive[S, F] with ParamProvider {
    this: Options with Randomness with DefaultOrdering[S, F] =>
  }

  object WithArchive {
    def apply[S <: Solution, F <: MultiobjectiveEvaluation](
      numToGen: Int, tournamentSize: Int, rn: TRandom) =
      new {
        val (rng, numToGenerate, tournSize) = (rn, numToGen, tournamentSize)
      } with WithArchive[S, F] with DefaultOrdering[S, F]
  }
  trait WithArchiveMixin[S <: Solution, F <: MultiobjectiveEvaluation]
    extends WithArchive[S, F] with ParamProvider {
    this: Options with Randomness with DefaultOrdering[S, F] =>
  }
}

final class TestNSGA {
  class S(val o: Seq[Int]) extends Solution
  class ES(o: Seq[Int]) extends ESol( new S(o),  
     MultiobjectiveEvaluation(o.map(v => ScalarEvaluationMax(v)))) {
    override def toString = o.toString
  }
  @Test
  def test: Unit = {
    val nsga = NSGA.WithArchive[S, MultiobjectiveEvaluation](5, 3, new Random)
    val state = new PopulationState(List(
      new ES(Seq(2, 3, 3)),
      new ES(Seq(3, 3, 1)),
      new ES(Seq(2, 2, 1)),
      new ES(Seq(1, 2, 2)), // crowding
      new ES(Seq(1, 2, 2)),
      new ES(Seq(1, 2, 2)),
      new ES(Seq(1, 1, 1))), 0)
    val sel = nsga.selector(Seq(state))
    for (i <- 0 until 20)
      println(sel.next)
  }
  /*
  @Test
  def test2: Unit = {
    val a = new NSGA.Eval(1, 3)
    val b = new NSGA.Eval(2, 3)
    val c = new NSGA.Eval(2, 1)
    println(a.comparePartial(b))
    println(a.comparePartial(c))
    println(b.comparePartial(c))
    println(c.comparePartial(b))
    println(c.comparePartial(c))
  }
  * 
  */
}  