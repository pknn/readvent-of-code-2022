import Day2.Shape.counter
import core.Solution

object Day2 extends Solution(2) {

  solveEasy {
    solve(Turn.apply)(_)
  }

  solveHard {
    solve(TurnRiggedByElf.apply)(_)
  }

  private def solve(apply: Seq[String] => Playable)(input: Seq[String]) =
    input.map(_.split(" ").toSeq)
         .map(apply)
         .map(_.gamePoint)
         .sum

  sealed abstract class Shape(val point: Int)

  case object Rock extends Shape(1)

  case object Paper extends Shape(2)

  case object Scissor extends Shape(3)

  object Shape {
    def apply(s: String): Shape = s match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissor
    }

    def counter(shape: Shape): Shape = shape match {
      case Rock => Paper
      case Paper => Scissor
      case Scissor => Rock
    }
  }

  sealed abstract class Status(val point: Int)

  case object Win extends Status(6)

  case object Draw extends Status(3)

  case object Lose extends Status(0)

  object Status {
    def apply(s: String): Status = s match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }
  }

  trait Playable {
    def gamePoint: Int
  }

  case class Turn(opponent: Shape, you: Shape) extends Playable {
    def gamePoint: Int = status.point + you.point

    private def status: Status = (opponent, you) match {
      case (oShape, yShape) if oShape == yShape => Draw
      case (oShape, yShape) => if (counter(oShape) == yShape) Win else Lose
    }
  }

  object Turn {
    def apply(g: Seq[String]): Turn = g match {
      case o +: y +: _ => Turn(o, y)
    }

    def apply(o: String, y: String): Turn = Turn(Shape(o), Shape(y))
  }

  case class TurnRiggedByElf(opponent: Shape, you: Status) extends Playable {
    def gamePoint: Int = shape.point + you.point

    private def shape: Shape = (opponent, you) match {
      case (oShape, Draw) => oShape
      case (oShape, Win) => counter(oShape)
      case (oShape, Lose) => counter(counter(oShape))
    }
  }

  object TurnRiggedByElf {
    def apply(g: Seq[String]): TurnRiggedByElf = g match {
      case o +: y +: _ => TurnRiggedByElf(Shape(o), Status(y))
    }
  }
}
