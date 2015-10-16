package scevo.func

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.util.Options
import scevo.util.TRandom
import scala.annotation.tailrec
import scevo.core.StatePop
import scala.collection.SeqLike
import scala.collection.generic.SeqForwarder
import scala.collection.IndexedSeqLike
import scala.collection.mutable.Builder

/**
  * Selection can be applied to any set of solutions, not only populations, hence
  *  the signature.
  */
trait Selection[S, E] extends (Seq[(S, E)] => (S, E))

class GreedySelection[S, E](implicit o: Ordering[E]) extends Selection[S, E] {
  def apply(pop: Seq[(S, E)]) = pop.minBy(_._2)
}

abstract class StochasticSelection[S, E](val rand: TRandom) extends Selection[S, E]

class RandomSelection[S, E](implicit rand: TRandom) extends StochasticSelection[S, E](rand) {
  override def apply(pop: Seq[(S, E)]) = pop(rand)
}

class TournamentSelection[S, E](ordering: Ordering[E], val tournamentSize: Int)(implicit rand: TRandom)
    extends StochasticSelection[S, E](rand) {

  def this(o: Ordering[E])(implicit opt: Options, rand: TRandom) =
    this(o, opt("tournamentSize", 7, (_: Int) >= 2))(rand)

  def apply(pop: Seq[(S, E)]) = pop(rand, tournamentSize).minBy(_._2)(ordering)
}
object TournamentSelection {
  def apply[S, E](o: Ordering[E])(implicit opt: Options, rand: TRandom) =
    new TournamentSelection[S, E](o)(opt, rand)
  def apply[S, E](opt: Options)(rand: TRandom)(o: Ordering[E]) =
    new TournamentSelection[S, E](o)(opt, rand)
}

class FitnessPropSelSlow[S](implicit rand: TRandom) extends Selection[S, Double] {
  // Inefficient version: recalculates distribution in every selection act. 
  def apply(pop: Seq[(S, Double)]) = {
    val distribution = Distribution.fromAnything(pop.map(_._2))
    pop(distribution(rand))
  }
}

/**
  * Efficient version: applicable only to NormalizedPop
  *
  */
class FitnessPropSel[S](implicit rand: TRandom) extends Selection[S, Double] {

  /*
  class N(val values: Vector[Int])
    extends IndexedSeq[Int] with IndexedSeqLike[Int, N] {
  override def newBuilder: Builder[Int, N] = N.newBuilder(values)
  protected override def underlying = values
}
  class NormalizedSeq(override val seq: Seq[(S,Double)]) 
  extends SeqLike[(S,Double), NormalizedSeq] {
    override def iterator = seq.iterator
    override def apply(i: Int) = seq(i)
    override def length = seq.length
    override def newBuilder = new Builder
  }
    

  case class NormalizedPop(val pop :StatePop[(S,Double)]) extends StatePop[(S, Double)]{
    val distribution = Distribution.fromAnything(pop.solutions.map(_._2))
    override val solutions = pop.solutions
  }
  * 
  */
  def apply(pop: Seq[(S, Double)]) = {
    val distribution = Distribution.fromAnything(pop.map(_._2))
    pop(distribution(rand))
  }
  // Efficient version: Distribution calculated only once. 
  /*
  def apply[S] =
    (pop: Seq[(S, Double)]) =>
      {
        val distribution = Distribution.fromAnything(pop.map(_._2))
        (rand: TRandom) => pop(distribution(rand))
      }
      * 
      */
}

// Note: Here E stands for one objective, not entire evaluation. 
class LexicaseSelection[S, E](o: Ordering[E])(implicit rand: TRandom)
    extends StochasticSelection[S, Seq[E]](rand) {
  def apply(pop: Seq[(S, Seq[E])]) = {
    @tailrec def sel(sols: Seq[(S, Seq[E])], cases: List[Int]): (S, Seq[E]) =
      if (sols.size == 1) sols(0)
      else if (cases.size == 1) sols(rand)
      else {
        val theCase = cases(rand)
        val ord = new Ordering[(S, Seq[E])] {
          override def compare(a: (S, Seq[E]), b: (S, Seq[E])) = o.compare(a._2(theCase), b._2(theCase))
        }
        val best = sols.min(ord)
        //println("Sols:" + sols.size + " Cases: " + cases.size)
        sel(sols.filter(s => ord.compare(s, best) <= 0), cases.diff(List(theCase)))
      }
    // assumes nonempty pop
    sel(pop, 0.until(pop(0)._2.size).toList)
  }
}
