import core.Solution

object Day6 extends Solution(6) {

  def findFirstMarker(text: String, size: Int) = {
    val windows       = text.toList.sliding(size).toSeq
    val uniqueWindows = windows.find(_.toSet.toSeq.length >= size).get

    windows.indexOf(uniqueWindows) + size
  }

  solveEasy { input =>
    findFirstMarker(input.head, 4)
  }

  solveHard { input =>
    findFirstMarker(input.head, 14)
  }
}
