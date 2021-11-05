package scalashop

import org.scalameter.*

import java.util.concurrent.*
import scala.util.DynamicVariable

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))

  def apply(x: Int, y: Int): RGBA = data(y * width + x)

  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

  //  val currentPoint = src.apply(x, y)
  //  val redP = red(currentPoint)
  //  val blueP = blue(currentPoint)
  //  val greenP = green(currentPoint)
  //  val alphaP = alpha(currentPoint)
/*
  val dd = for {
    yp <- (y - radius to y + radius) if (clamp(yp, 0, src.height - 1) == yp)
    xp <- (x - radius to x + radius) if (clamp(xp, 0, src.width - 1) == xp)
  } yield (xp, xp, yp * src.width + xp)

  dd.size
*/
  /// delete
  val points = for {
    yp <- (y - radius to y + radius) if (clamp(yp, 0, src.height-1) == yp)
    xp <- (x - radius to x + radius) if (clamp(xp, 0, src.width-1) == xp)
    point = src(xp, yp)
    rp = red(point)
    bp = blue(point)
    gp = green(point)
    ap = alpha(point)
  } yield (rp, bp, gp, ap)
  val size = points.size
  val ((ar, ab), (ag, aa)) = parallel(
    parallel(points.map(_._1).sum / size, points.map(_._2).sum / size),
    parallel(points.map(_._3).sum / size, points.map(_._4).sum / size)
  )
  rgba(ar, ag, ab, aa)
}

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler :
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task {
    taskA
  }
  val tb = task {
    taskB
  }
  val tc = task {
    taskC
  }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
