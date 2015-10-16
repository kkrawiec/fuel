package scevo.tests

import org.junit.Test
import org.scalatest._
import org.scalatest.Matchers
import scevo.core.BestSelector

object TestBestSelector {
//  def main(args: Array[String]) =
//    println(BestSelector(List(3, 1, 3, 6))(Ordering[Int]))
}

/*
class ExampleSpec extends FlatSpec with Matchers {

  "A BestSelector" should "select the maximal element for maximized evaluation" in {
    BestSelector(  should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
}
* 
*/

class TestEvaluation extends FlatSpec with Matchers {

  /*
  "An Evaluation " should " compare correctly to other evaluations." in {
    val x = ScalarEvaluationMax(3.0)
    val y = ScalarEvaluationMax(3.0)
    val z = ScalarEvaluationMax(2.0)

    x shouldEqual y
    y shouldEqual x
    assert(x.betterThan(z))
    assert(!z.betterThan(x))
    assert(!x.betterThan(y))

    val zm = ScalarEvaluationMax(2.0)

    val m0 = MultiobjectiveEvaluation(Seq(x, x))
    val m1 = MultiobjectiveEvaluation(Seq(x, z))
    val m2 = MultiobjectiveEvaluation(Seq(z, x))
    val m3 = MultiobjectiveEvaluation(Seq(z, z))
    m1 comparePartial m1 shouldBe Some(0)
    m1 comparePartial m2 shouldBe None
    m1 comparePartial m3 shouldBe Some(-1)
    m3 comparePartial m1 shouldBe Some(1)

    m1 betterThan m1 shouldBe false
    m1 betterThan m2 shouldBe false
    m1 betterThan m3 shouldBe true
    m3 betterThan m1 shouldBe false
    m0 betterThan m3 shouldBe true
  }

  it should " not pass assertion for NaN" in {
    a[IllegalArgumentException] should be thrownBy {
      ScalarEvaluationMax(Double.NaN)
    }
  }
  * 
  *
  */
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

