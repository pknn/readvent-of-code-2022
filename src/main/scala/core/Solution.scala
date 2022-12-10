package core


import core.Kind.{Input, Test}

import scala.io.Source

abstract class Solution(nthDay: Int) extends App {
  type InputLines = Seq[String]

  def solveEasy(fn: InputLines => Any): Unit = solve(1, fn)()

  def solveHard(fn: InputLines => Any): Unit = solve(2, fn)()

  object solveEasy {
    def withTestInput(fn: InputLines => Any): Unit = solve(1, fn)(Test)
  }

  object solveHard {
    def withTestInput(fn: InputLines => Any): Unit = solve(2, fn)(Test)
  }

  private def solve(nthChallenge: Int, fn: InputLines => Any)(kind: Kind = Input): Unit = {
    val answer = fn(getNthDayContent(nthDay, kind))
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