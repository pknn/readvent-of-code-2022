import core.Solution

object Day3 extends Solution(3) {
  val UpperP = "([A-Z])".r
  val LowerP = "([a-z])".r

  def toPriority(char: Char) = char match {
    case UpperP(value) => value.toInt - 38
    case LowerP(value) => value.toInt - 96
  }

  solveEasy {
    _.map(_.toCharArray.toSeq)
     .map(line => line.grouped(line.length / 2).toSeq)
     .map {
       case a +: b +: _ => a.intersect(b)
     }
     .map(_.head)
     .map(toPriority)
     .sum
  }

  solveHard {
    _.map(_.toCharArray.toSeq)
     .grouped(3).toSeq
     .map {
       case a +: b +: c +: _ => a.intersect(b).intersect(c)
     }
     .map(_.head)
     .map(toPriority)
     .sum
  }
}
