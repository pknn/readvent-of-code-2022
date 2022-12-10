import core.Solution

import scala.collection.mutable

object Day9 extends Solution(9) {
  solveEasy { input =>
    val movements = input.map(Movement.apply)
    val currentTailsPosition: mutable.IndexedSeq[(Int, Int)] = mutable.IndexedSeq((0 to 1).map(_ => (0, 0)): _*)
    solve(movements, currentTailsPosition)
  }

  solveHard { input =>
    val movements = input.map(Movement.apply)
    val currentTailsPosition: mutable.IndexedSeq[(Int, Int)] = mutable.IndexedSeq((0 to 9).map(_ => (0, 0)): _*)
    solve(movements, currentTailsPosition)
  }

  def solve(movements: Seq[Movement], currentTailsPosition: mutable.IndexedSeq[(Int, Int)]) = {
    val gridMap: mutable.Map[(Int, Int), Boolean] = mutable.Map((0, 0) -> true)
    movements.foreach { movement =>
      (0 until movement.distance).toList foreach { i =>
        currentTailsPosition.zipWithIndex.foreach { tailWithIndex =>
          val (tail, index) = tailWithIndex
          currentTailsPosition(index) =
            if (index == 0) getNextHeadPosition(movement.direction, currentTailsPosition(0))
            else getNextTailPosition(tail, currentTailsPosition(index - 1))
        }
        gridMap(currentTailsPosition.last) = true
      }
    }
    gridMap.values.toSeq.length
  }

  sealed trait Direction

  case object Up extends Direction

  case object Down extends Direction

  case object Left extends Direction

  case object Right extends Direction

  object Direction {
    def apply(directionString: String): Direction = directionString match {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }
  }

  case class Movement(direction: Direction, distance: Int)

  object Movement {
    private val pattern = raw"(.) (.+)".r

    def apply(str: String): Movement = str match {
      case pattern(direction, distanceString) =>
        Movement(Direction.apply(direction), distanceString.toInt)
    }
  }

  def isAdjacent(currentTailPos: (Int, Int), currentHeadPos: (Int, Int)): Boolean = {
    isDiagonallyAdjacent(currentTailPos, currentHeadPos) ||
      isVerticallyOrHorizontallyAdjacent(currentTailPos, currentHeadPos) ||
      currentHeadPos == currentTailPos
  }

  def isDiagonallyAdjacent(currentTailPos: (Int, Int), currentHeadPos: (Int, Int)): Boolean =
    Math.abs(currentTailPos._1 - currentHeadPos._1) == 1 && Math.abs(currentTailPos._2 - currentHeadPos._2) == 1

  def isVerticallyOrHorizontallyAdjacent(currentTailPos: (Int, Int), currentHeadPos: (Int, Int)): Boolean =
    (currentTailPos._1 == currentHeadPos._1 && Math.abs(currentTailPos._2 - currentHeadPos._2) == 1) ||
      (currentTailPos._2 == currentHeadPos._2 && Math.abs(currentTailPos._1 - currentHeadPos._1) == 1)

  def getNextHeadPosition(direction: Direction, headPosition: (Int, Int)): (Int, Int) = {
    direction match {
      case Up => headPosition.copy(_2 = headPosition._2 + 1)
      case Down => headPosition.copy(_2 = headPosition._2 - 1)
      case Left => headPosition.copy(_1 = headPosition._1 - 1)
      case Right => headPosition.copy(_1 = headPosition._1 + 1)
    }
  }

  def getNextTailPosition(tailPosition: (Int, Int), headPosition: (Int, Int)): (Int, Int) = {
    if (isAdjacent(tailPosition, headPosition)) tailPosition
    else {
      val diffX = if (headPosition._1 < tailPosition._1) -1 else if (headPosition._1 == tailPosition._1) 0 else 1
      val diffY = if (headPosition._2 < tailPosition._2) -1 else if (headPosition._2 == tailPosition._2) 0 else 1

      (tailPosition._1 + diffX, tailPosition._2 + diffY)
    }
  }

  def diagram(head: (Int, Int), tail: (Int, Int)) = {
    val maxX = 6
    val maxY = 6
    (0 until maxY).reverse.foreach { y =>
      (0 until maxX).foreach { x =>
        val char = (x, y) match {
          case coord if coord == head => " H "
          case coord if coord == tail => " T "
          case _ => " . "
        }
        print(char)
      }
      println()
    }
  }
}
