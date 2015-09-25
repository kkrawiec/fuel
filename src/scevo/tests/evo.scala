package scevo.tests

import org.junit.Test
import org.scalatest._
import org.scalatest.Matchers

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

