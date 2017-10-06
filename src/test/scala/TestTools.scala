package test.scala

import fuel.util.Parsers

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
}
