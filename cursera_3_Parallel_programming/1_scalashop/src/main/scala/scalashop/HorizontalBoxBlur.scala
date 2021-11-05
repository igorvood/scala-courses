package scalashop

import org.scalameter.*

object HorizontalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer (Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)

    val value = src(2, 2)
    val rgba = boxBlurKernel(src, 2, 2, 1)


    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface :

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   * starting with `from` and ending with `end` (non-inclusive).
   *
   * Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    val blurValue = for {
      xp <- (0 until src.width)
      yp <- (from until end)
      if yp >= 0 && yp < src.height
    } yield {
      dst.update(xp, yp, boxBlurKernel(src, xp, yp, radius))
    }

  }


  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   * Parallelization is done by stripping the source image `src` into
   * `numTasks` separate strips, where each strip is composed of some number of
   * rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val separateStrips: Seq[Int] = 0 to src.height by (src.height / numTasks max 1)

    val range: Seq[(Int, Int)] = separateStrips.zip(separateStrips.tail)
    range
      .map { case (from, end) =>
        task[Unit] {
          blur(src, dst, from, end, radius)
        }
      }
      .foreach(_.join())
  }

