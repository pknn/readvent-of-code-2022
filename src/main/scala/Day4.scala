import core.Solution

object Day4 extends Solution(4) {
  def isFullyContains(a: Range, b: Range) = a.intersect(b) == b || b.intersect(a) == a

  private val toFormattedLines = (xs: Seq[String]) => xs.map(_.split(",").toSeq.map(_.split("-").toSeq))
  private val mapDutyRanges = (xsss: Seq[Seq[Seq[String]]]) => xsss.map(_.map {
    case a +: b +: _ => Range.inclusive(a.toInt, b.toInt)
  })

  private val toDutyRanges = mapDutyRanges compose toFormattedLines


  solveEasy {
    toDutyRanges(_).map {
      case groupA +: groupB +: _ => isFullyContains(groupA, groupB)
    }.count(r => r)
  }

  solveHard {
    toDutyRanges(_).map {
      case groupA +: groupB +: _ => groupA.intersect(groupB)
    }.count(_.nonEmpty)
  }
}
