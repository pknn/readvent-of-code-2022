import core.Solution

object Day1 extends Solution(1) {
  private val toGroup = (x: Seq[String]) => x.foldLeft(Seq(Seq.empty[String])) {
    case (acc, "") => acc.prepended(Seq.empty)
    case (head :: tail, nonEmpty) => head.appended(nonEmpty) :: tail
  }.reverse

  private val asSeqOfSeqInt = (x: Seq[Seq[String]]) => x.map(_.map(_.toInt))

  private val asSum = (x: Seq[Seq[Int]]) => x.map(_.sum)

  private val toCalories = asSum compose asSeqOfSeqInt compose toGroup

  solveEasy { input =>
    toCalories(input).max
  }

  solveHard { input =>
    toCalories(input)
              .sorted
              .takeRight(3)
              .sum
  }
}
