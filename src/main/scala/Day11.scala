import core.Solution

import scala.collection.mutable

object Day11 extends Solution(11) {
  private val mapToDescriptions =
    (input: Seq[String])=> input.filter(_.nonEmpty).grouped(6).toSeq
  private val pattern = raw"Monkey (\d+):\n  Starting items: (.+)\n  Operation: new = old (.) (.+)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d)\n    If false: throw to monkey (\d)".r
  private val mapDescriptionsToMonkeys =
    (descriptions: Seq[Seq[String]]) => descriptions.map(_.mkString("\n") match {
      case pattern(index, itemsString, operation, against, divider, targetTrue, targetFalse) =>
        Monkey(
          index.toInt,
          items = itemsString.split(",").map(_.strip()).map(worryLevel => Item(worryLevel.toLong)),
          operation = Operation(operation, against),
          testOperation = TestOperation(divider, targetTrue, targetFalse)
        )
    })

  private val toMonkeys = mapDescriptionsToMonkeys compose mapToDescriptions
  private val toTurnResult = (monkey: Monkey) => {
    val evaluatedItems = monkey.items.map(Operation.evaluate(monkey.operation, _))
    val groupedItems = evaluatedItems.partition(_.worryLevel % monkey.testOperation.by == 0)
    (
      groupedItems._1,
      groupedItems._2
    )
  }
  private val toRoundResult = (monkeys: Seq[Monkey]) => {
    val activeness = mutable.Seq.from(monkeys.indices.map(_ => 0))
    monkeys.foreach { monkey =>
      val (targetTrueItems, targetFalseItems) = toTurnResult(monkey)
      activeness(monkey.index) += monkey.items.length

      monkeys(monkey.index).throwItems()
      monkeys(monkey.testOperation.targetTrue).receiveItems(targetTrueItems)
      monkeys(monkey.testOperation.targetFalse).receiveItems(targetFalseItems)
    }

    (monkeys, activeness)
  }
  private val runTrial = (monkeys: Seq[Monkey], n: Int) => {
    var monkeyss = monkeys
    var activeness = mutable.Seq.from(monkeyss.indices.map(_ => 0))
    (1 to n).foreach { round =>
      val result = toRoundResult(monkeyss)
      monkeyss = result._1
      activeness = activeness.zip(result._2).map(r => r._1 + r._2)
      if (round == 1 || round == 20) {
        println(activeness)
      }
      if (round % 1000 == 0) {
        println(activeness)
      }
    }

    (monkeyss, activeness)
  }

  solveEasy { input =>
    val (_, activeness) = runTrial(toMonkeys(input), 20)

    activeness.sorted.reverse.take(2).product
  }

  solveHard.withTestInput { input =>
    val (_, activeness) = runTrial(toMonkeys(input), 10000)
    activeness.sorted.reverse.take(2).product
  }

  sealed trait Operation
  case object Square extends Operation
  case class Multiply(by: Long) extends Operation
  case class Add(by: Long) extends Operation

  object Operation {
    def apply(operation: String, against: String): Operation = (operation, against) match {
      case ("*", "old") => Square
      case ("*", number) => Multiply(by = number.toLong)
      case ("+", number) => Add(by = number.toLong)
    }

    def evaluate(operation: Operation, item: Item): Item = {
      val moddedItem = item.copy(worryLevel = item.worryLevel % 9_699_690L)
      val result = operation match {
        case Square => moddedItem.copy(moddedItem.worryLevel * moddedItem.worryLevel)
        case Multiply(by) =>
          moddedItem.copy(moddedItem.worryLevel * by)
        case Add(by) =>
          moddedItem.copy(moddedItem.worryLevel + by)
      }
      result
    }
  }

  case class TestOperation(by: Long, targetTrue: Int, targetFalse: Int)
  object TestOperation {
    def apply(divider: String, targetTrue: String, targetFalse: String): TestOperation =
      TestOperation(by = divider.toLong, targetTrue.toInt, targetFalse.toInt)
  }

  case class Item(worryLevel: Long)
  case class Monkey(index: Int, var items: Seq[Item], operation: Operation, testOperation: TestOperation) {
    def receiveItems(items: Seq[Item]): Unit = {
      this.items = this.items ++ items
    }

    def throwItems(): Unit = {
      this.items = Seq.empty
    }

    override def toString: String = s"Monkey $index has items: $items"
  }
}
