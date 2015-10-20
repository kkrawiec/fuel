package scevo.tests

import org.junit.Test
import scevo.core.Greatest
import scevo.core.Dominance
import scevo.moves.BoolVectNeigh

/** Tests of selected components. 
 *  
 */
object TestGreatest extends App {
  val a = Seq(1, 1)
  val b = Seq(2, 2)
  val c = Seq(1, 3)
  val d = Seq(3, 1)
  val e = Seq(2, 2)

  implicit val o = Dominance[Int]

  for (p <- Seq(a, b, c, d, e).permutations)
    assert(Greatest(p) == Some(a))
  for (p <- Seq(b, c, d).permutations)
    assert(Greatest(p) == None)
  for (p <- Seq(b, c, d, e).permutations)
    assert(Greatest(p) == None)
  for (p <- Seq(a, b, c, d, e))
    assert(Greatest(Seq(p)) == Some(p))
}

object TestNeigh extends App {
  val n = new BoolVectNeigh
  assert((n(IndexedSeq(false, false, false)).toList == 
    List(Vector(false, false, true), Vector(false, true, false), Vector(true, false, false))))
  /*
  for( n <- 1 until 5)
    for( p <- IndexedSeq.fill(n)(false) )
  println(n(IndexedSeq(false, false, false)))
  * 
  */
}

object TestBestSelector {
//  def main(args: Array[String]) =
//    println(BestSelector(List(3, 1, 3, 6))(Ordering[Int]))
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

