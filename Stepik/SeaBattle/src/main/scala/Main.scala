package ru.vood.scala.sea.battle

import data.Lesson
import data.Lesson.{fieldBad1, fieldBad2, fieldBad3}
import data.Naval.{Field, Fleet, Ship, readNext}

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
      case (_, row, col) if col == 1 && row == 1 => true
      case (false, row, col) => row == 1 && col > 1
      case (true, row, col) => row > 1 && col == 1
    }
  }

  // определить, подходит ли корабль по своим характеристикам
  def validateShip(ship: Ship): Boolean = {
    val isVert = checkIsVertical(ship)
    val isHor = checkIsVertical(ship, isVertical = false)
    (isVert, isHor, ship, ship.size) match {
      case (_, _, _, l) if l > 4 => false
      case (true, false, s, _) => (s.map(_._1).min to s.map(_._1).max) == s.map(_._1)
      case (false, true, s, _) => (s.map(_._2).min to s.map(_._2).max) == s.map(_._2)
      case (true, true, _, _) => true
      case _ => false
    }
  }


  def validatePosition(ship: Ship, field: Field): Boolean = {
    val mappedF: Seq[(Boolean, (Int, Int))] = field.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(col => (col._1, (row._2, col._2))))
    val value1 = for {
      s <- ship
      m <- mappedF if !m._1 && m._2 == s
    } yield m
    value1.size == ship.size
  }

//  // добавить корабль во флот
//  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
//    val value1: Fleet = fleet ++ (name, ship)
//    value1
//  }


  assert(validateShip(List((0, 0))))
  assert(validateShip(List((0, 1), (0, 2))))
  assert(validateShip(List((0, 0), (1, 0))))
  assert(!validateShip(List((0, 0), (0, 1), (1, 0))))
  assert(!validateShip(List((0, 0), (0, 1), (0, 2), (0, 3), (0, 5))))
  assert(!validateShip(List((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5))))
  assert(validateShip(List((0, 0), (0, 1), (0, 2), (0, 3))))


  assert(validatePosition(List((0, 0)), Lesson.field))
  assert(!validatePosition(List((10, 0)), Lesson.field))
  assert(!validatePosition(List((0, 10)), Lesson.field))
  assert(!validatePosition(List((-1, 0)), Lesson.field))
  assert(!validatePosition(List((0, -1)), Lesson.field))
  assert(!validatePosition(List((0, 0)), fieldBad1))
  assert(!validatePosition(List((0, 0)), fieldBad2))
  assert(!validatePosition(List((0, 0)), fieldBad3))
}
