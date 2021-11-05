package scalashop

import org.junit.Assert.*
import org.junit.Test
import scalashop.*

import java.util.concurrent.*
import scala.collection.*

case class TestCase(
                     val testPointx: Int,
                     val testPointy: Int,
                     val originalColour: RGBA,
                     val radius: Int,
                     val expectRed: Int,
                     val expectBlue: Int,
                     val expectGreen: Int,
                     val expectAlpha: Int
                   )

class Test2 {

  def runTest(
               testPointx: Int,
               testPointy: Int,
               originalColour: Int,
               radius: Int,
               expectRed: Int,
               expectBlue: Int,
               expectGreen: Int,
               expectAlpha: Int): Unit = {
    val width = 5
    val height = 5
    val src = Img(width, height)
    val img = for {
      x <- 0 to width - 1
      y <- 0 to height - 1
      c = if (x == testPointx && y == testPointy) originalColour else 0
      color = rgba(c, c, c, c)
    } yield (x, y, color)

    img.foreach(p => {
      src.update(p._1, p._2, p._3)
    })

    val rgba1 = boxBlurKernel(src, testPointx, testPointy, radius)
    assertEquals(expectRed, red(rgba1))
    assertEquals(expectBlue, blue(rgba1))
    assertEquals(expectGreen, green(rgba1))
    assertEquals(expectAlpha, alpha(rgba1))
  }

  @Test
  def `boxBlurKernel_2`(): Unit = {
    runTest(2,2,9,1,1,1,1,1)
  }

  @Test
  def `boxBlurKernel_3`(): Unit = {
    runTest(2,2,25,2,1,1,1,1)
  }

  @Test
  def `boxBlurKernel_1`(): Unit = {
    runTest(0,0,4,1,1,1,1,1)
  }

  @Test
  def `boxBlurKernel_4`(): Unit = {
    runTest(0,4,4,1,1,1,1,1)
  }

  @Test
  def `boxBlurKernel_5`(): Unit = {
    runTest(4,4,4,1,1,1,1,1)
  }

  @Test
  def `boxBlurKernel_6`(): Unit = {
    runTest(4,0,4,1,1,1,1,1)
  }


}
