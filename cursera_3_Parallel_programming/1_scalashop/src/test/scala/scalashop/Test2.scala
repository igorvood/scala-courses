package scalashop

import org.junit.Test
import scalashop.*

import java.util.concurrent.*
import scala.collection.*

class Test2 {

  @Test def `qqqqqqqqqq`(): Unit = {
    assert(1 == 1)
  }

  @Test
  def `boxBlurKernel_1`(): Unit = {
    val radius = 3
    val width = 5
    val height = 5
    val src = Img(width, height)
    val unit = for {
      x <- 0 to width - 1
      y <- 0 to height - 1
    } yield (x, y)
    unit.foreach(p => {
      val value1 = rgba(p._1 + p._2, p._1 + p._2, p._1 + p._2, p._1 + p._2)
      src.update(p._1, p._2, value1)
    })

    //    src.


    val value = src(2, 2)
    val rgba1 = boxBlurKernel(src, 2, 2, 1)
    println(rgba)
  }
}
