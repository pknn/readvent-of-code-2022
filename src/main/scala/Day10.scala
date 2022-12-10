import core.Solution

import scala.collection.mutable

object Day10 extends Solution(10) {

  sealed trait Ops

  case object Noop extends Ops

  case class AddX(x: Int) extends Ops

  private val noopPattern = raw"noop".r
  private val addXPattern = raw"addx (.+)".r

  private val toOps = (xs: Seq[String]) => xs.flatMap {
    case noopPattern() => Seq(Noop)
    case addXPattern(x: String) => Seq(Noop, AddX(x.toInt))
  }

  private val mapWithValue = (ops: Seq[Ops]) => mutable.IndexedSeq.from(Seq((Noop, 1)) ++ ops.map((_, 1)))

  private val toOpsWithValue = mapWithValue compose toOps

  private def evaluate(ops: mutable.IndexedSeq[(Ops, Int)]) = {
    ops.zipWithIndex.foreach { opWithIndex =>
      val ((op, _), index) = opWithIndex
      op match {
        case Noop if index == 0 => ()
        case Noop => ops(index) = ops(index).copy(_2 = ops(index - 1)._2)
        case AddX(x) => ops(index) = ops(index).copy(_2 = ops(index - 1)._2 + x)
      }
    }

    ops
  }

  def isPartOfSprite(x: Int, cycle: Int) = Math.abs(cycle - x) <= 1

  solveEasy { input =>
    val opsWithValue = toOpsWithValue(input)
    val evaluatedOps = evaluate(opsWithValue)

    val focusCycles = Seq(20, 60, 100, 140, 180, 220)
    focusCycles.map(cycle => evaluatedOps(cycle - 1)._2 * cycle).sum
  }

  solveHard { input =>
    val opsWithValue = toOpsWithValue(input)
    val evaluatedOps = evaluate(opsWithValue)
    val screen = evaluatedOps.indices.map { cycle =>
      println(cycle, cycle % 40, evaluatedOps(cycle)._2)
      if (isPartOfSprite(evaluatedOps(cycle)._2, cycle % 40)) "##" else ".."
    }.grouped(40)

    screen.map(_.foldLeft("")(_ + _)).foldLeft("")((lines, line) => lines + "\n" + line)
  }
}
