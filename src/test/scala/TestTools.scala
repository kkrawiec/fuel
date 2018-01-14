package test.scala

import fuel.util.{Parsers, Utils}
import org.junit.Test
import org.junit.Assert._

final class TestTools {
  @Test
  def testParsePropertiesFileEmpy(): Unit = {
    val text =
      """
        |
        |
      """.stripMargin
    val opt = Parsers.parsePropertiesFile(text)
    assertEquals(Map(), opt.allOptions)
  }

  @Test
  def testParsePropertiesFile(): Unit = {
    val text =
      """maxGenerations = 30
        |status = completed
      """.stripMargin
    val opt = Parsers.parsePropertiesFile(text)
    assertEquals(Map("maxGenerations"->"30", "status"->"completed"), opt.allOptions)
  }

  @Test
  def testMedian(): Unit = {
    assertEquals(2.0, Utils.median(Seq(2.0)), 0.0)
    assertEquals(-1.0, Utils.median(Seq(2.0, -4.0)), 0.0)
    assertEquals(3.0, Utils.median(Seq(2.0, 5.0, 4.0, 1.0)), 0.0)
    assertEquals(4.0, Utils.median(Seq(2.0, 5.0, 4.0, 1.0, 10.0)), 0.0)
  }
}
