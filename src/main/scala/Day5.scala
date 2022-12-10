import core.Solution

import scala.collection.mutable
import scala.util.matching.Regex

object Day5 extends Solution(5) {

  solveEasy { input =>
    val (commandsString, fullStack) = input.partition(_.startsWith("move"))

    val stackString = fullStack match {
      case stack :+ _ :+ _ => stack
    }

    val groupedStackString = stackString.map(_.grouped(4).toSeq).reverse

    val stackCount = groupedStackString.map(_.length).max

    val stacks = Range.inclusive(1, stackCount).map(_ => new mutable.Stack[String]())
    groupedStackString.map(layer => layer.map(_.strip().replaceAll("[\\[\\]]", "")).zip(stacks).filter(_._1.nonEmpty))
                      .foreach(_.foreach { row =>
                        val (item, stack) = row
                        stack.push(item)
                      })
    val commands = commandsString.map(Command.apply)

    for (command <- commands) {
      val fromStack = stacks(command.from - 1)
      val toStack   = stacks(command.to - 1)

      for (_ <- Range.inclusive(1, command.quantity)) {
        val item = fromStack.pop()
        toStack.push(item)
      }
    }

    stacks.map(_.pop()).fold("")(_ + _)
  }

  solveHard { input =>
    val (commandsString, fullStack) = input.partition(_.startsWith("move"))

    val stackString = fullStack match {
      case stack :+ _ :+ _ => stack
    }

    val groupedStackString = stackString.map(_.grouped(4).toSeq).reverse

    val stackCount = groupedStackString.map(_.length).max
    val secondStacks = Range.inclusive(1, stackCount).map(_ => new mutable.Stack[String]())
    groupedStackString.map(layer => layer.map(_.strip().replaceAll("[\\[\\]]", ""))
                                         .zip(secondStacks)
                                         .filter(_._1.nonEmpty))
                      .foreach(_.foreach { row =>
                        val (item, stack) = row
                        stack.push(item)
                      })

    val commands = commandsString.map(Command.apply)

    for (command <- commands) {
      val fromStack = secondStacks(command.from - 1)
      val toStack   = secondStacks(command.to - 1)

      val list = new mutable.ListBuffer[String]()
      for (_ <- Range.inclusive(1, command.quantity)) {
        val item = fromStack.pop()
        list.append(item)
      }

      toStack.pushAll(list.reverse)
    }

    secondStacks.map(_.pop()).fold("")(_ + _)
  }

  case class Command(quantity: Int, from: Int, to: Int)

  object Command {

    implicit class RegexOps(sc: StringContext) {
      def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    private val pattern = raw"move (\d+) from (\d+) to (\d+)".r

    def apply(strCommand: String): Command = strCommand match {
      case pattern(quantity, from, to) => Command(quantity.toInt, from.toInt, to.toInt)
    }
  }
}