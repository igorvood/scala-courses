package ru.vood.scala.sea.battle
package data

object Naval {

  type Point = (Int, Int) // Клетка корабля - пара координат этой клетки на поле
  type Field = Vector[Vector[Boolean]] // Игровое поле - двумерный массив, хранящий для каждой ячейки булево значение - стоит ли на этой клетке корабль?
  type Ship = List[Point] // Корабль как список точек
  type Fleet = Map[String, Ship] // Множество всех кораблей на поле как ассоциативный массив. По строковому ключу (имени корабля) находится список точек корабля
  type Game = (Field, Fleet) // Игровое поле и список кораблей


  var input = List(
    "3",
    "BlackPearl 3",
    "1 6",
    "1 7",
    "1 8",
    "MillenniumFalcon 4",
    "2 5",
    "3 5",
    "4 5",
    "5 5",
    "Varyag 1",
    "9 9")


  def readNext: String = {
    input match {
      case x :: xs =>
        input = xs
        x
      case Nil => "Empty"
    }

  }

}
