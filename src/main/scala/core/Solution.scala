package core

import scala.io.Source

abstract class Solution(nthDay: Int, kind: Kind) extends App {
  type InputLines = Seq[String]
  private lazy val input = getNthDayContent(nthDay, kind)

  def solveEasy(fn: InputLines => Any): Unit = solve(1, fn)

  def solveHard(fn: InputLines => Any): Unit = solve(2, fn)

  private def solve(nthChallenge: Int, fn: InputLines => Any): Unit = {
    val answer = fn(input)
    done(nthChallenge, answer)
  }

  private def getNthDayContent(nthDay: Int, kind: Kind): Seq[String] =
    Source.fromResource(composeFileName(nthDay, kind)).getLines().toSeq

  private def composeFileName(nthDay: Int, kind: Kind) =
    s"day$nthDay/${kind.fileName}"

  private def done[V](nthChallenge: Int, answer: V): Unit = {
    println(s"Answer#$nthChallenge: $answer")
  }
}