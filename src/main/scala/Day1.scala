import core.Solution

object Day1 extends Solution(1) {

  solveEasy {
    toCalories(_).max
  }

  solveHard {
    toCalories(_)
              .sorted
              .takeRight(3)
              .sum
  }

  private def toGroup(x: Seq[String]): Seq[Seq[String]] = x.foldLeft(Seq(Seq.empty[String])) {
    case (acc, "") => acc.prepended(Seq.empty)
    case (head :: tail, nonEmpty) => head.appended(nonEmpty) :: tail
  }.reverse

  private def asSeqOfSeqInt(x: Seq[Seq[String]]) = x.map(_.map(_.toInt))

  private def asSum(x: Seq[Seq[Int]]) = x.map(_.sum)

  private def toCalories = asSum compose asSeqOfSeqInt compose toGroup
}
