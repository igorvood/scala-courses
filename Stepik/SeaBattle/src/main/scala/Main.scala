package ru.vood.scala.sea.battle

import data.Naval.{Fleet, Ship, readNext}

object Main extends App {

  val sheepCnt = readNext.toInt
  private val data = (for {
    sheepNum <- 1 to sheepCnt
    sheepInfo <- {
      val strings: Seq[String] = readNext.split(" ").toList
      List((strings.head, strings(1).toInt))
    }
    sheepName = sheepInfo._1
    sheepSize = sheepInfo._2
    square <- 1 to sheepInfo._2.toInt
    point = readNext
    p = {
      val strings = point.split(" ")
      (strings(0), strings(1))
    }
  } yield (sheepName, p)).toList

  val value: Fleet =
    data
      .groupBy(_._1)
      .map(q => (
        q._1,
        q._2
          .map(e => e._2)
          .map(p => (p._1.toInt, p._2.toInt))
      )
      )
  value.foreach(println)

  def checkIsVertical(ship: Ship, isVertical: Boolean = true): Boolean = {
    (isVertical, ship.map(_._1).distinct.size, ship.map(_._2).distinct.size) match {
      case (true, row, col) => row == 1 && col > 1
      case (true, row, col) => row > 1 && col == 1
    }
  }

  // определить, подходит ли корабль по своим характеристикам
  def validateShip(ship: Ship): Boolean = {
    val size = ship.map(_._1).distinct.size
    val size1 = ship.map(_._2).distinct.size
    println(size + " " + size1)
    (size == 1) || (size1 == 1)
  }


  assert(validateShip(List((0, 0))))
  assert(!validateShip(List((0, 0), (0, 1), (1, 0))))
  assert(!validateShip(List((0, 0), (0, 1), (0, 2), (0, 3), (0, 5))))
}
