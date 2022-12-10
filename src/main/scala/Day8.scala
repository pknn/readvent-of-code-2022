import core.Solution

object Day8 extends Solution(8) {
  private def asTrees(input: Seq[String]): Seq[Tree] =
    input.zipWithIndex.flatMap { row =>
      val (line, y) = row
      line.split("").zipWithIndex.map { column =>
        val (char, x) = column
        Tree(char.toInt, x, y)
      }
    }

  solveEasy { input =>
    val trees    = asTrees(input)
    val edges    = Util.getEdge(trees)
    val fillings = trees.diff(edges)

    val visibleFromEdges = fillings.map { self =>
      val (left, right, up, down) = Util.getLineOfSight(self, trees)

      val maxHeights = Seq(
        left.map(_.height).max,
        right.map(_.height).max,
        up.map(_.height).max,
        down.map(_.height).max
      )

      maxHeights.exists(_ < self.height)
    }

    edges.length + visibleFromEdges.count(v => v)
  }

  solveHard { input =>
    val trees    = asTrees(input)
    val scores = trees.map { self =>
      val (left, right, up, down) = Util.getLineOfSight(self, trees)
      Seq(left.reverse, right, up.reverse, down)
        .map(_.map(_.height))
        .map(Util.getFurthestDistance(self.height, _))
        .product
    }

    scores.max
  }

  case class Tree(height: Int, x: Int, y: Int)

  object Util {
    def getEdge(trees: Seq[Tree]): Seq[Tree] = {
      val (maxX, maxY) = (trees.map(_.x).max, trees.map(_.y).max)

      trees.filter(tree => tree.x == 0 || tree.y == 0 || tree.x == maxX || tree.y == maxY)
    }

    def getLineOfSight(self: Tree, trees: Seq[Tree]): (Seq[Tree], Seq[Tree], Seq[Tree], Seq[Tree]) = {
      val (left, right) = trees.filter(_.y == self.y).filter(_ != self).partition(_.x < self.x)
      val (up, down)    = trees.filter(_.x == self.x).filter(_ != self).partition(_.y < self.y)

      (left, right, up, down)
    }

    def getFurthestDistance(selfHeight: Int, heights: Seq[Int]): Int = heights.indexWhere(_ >= selfHeight) match {
      case -1 => heights.length
      case other => other + 1
    }
  }
}
