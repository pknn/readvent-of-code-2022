import core.Solution

import scala.collection.mutable

object Day7 extends Solution(7) {
  solveEasy { input =>
    val fileSystem = FileSystem(input)

    val consideredLarge = 100000
    fileSystem.getDirectoriesSizeRecursive()
              .filter(_._2 <= consideredLarge)
              .map(_._2)
              .sum
  }

  solveHard { input =>
    val fileSystem = FileSystem(input)

    val totalSpace    = 70000000
    val neededSpace   = 30000000
    val rootDirSize   = fileSystem.rootDirSize
    val deletingSpace = neededSpace - (totalSpace - rootDirSize)

    val enoughToDeletingSpaceDir = fileSystem.getDirectoriesSizeRecursive().filter(_._2 >= deletingSpace).sortBy(_._2)
    enoughToDeletingSpaceDir.head._2
  }

  sealed abstract class SystemObject(val name: String)

  case class Directory(override val name: String,
                       var parent: Directory,
                       var children: mutable.Map[String, SystemObject]) extends SystemObject(name)

  case class File(override val name: String, size: Int) extends SystemObject(name)

  object SystemObject {
    def getChildDirectory(name: String, children: mutable.Map[String, SystemObject]): Directory =
      children(name).asInstanceOf[Directory]

    def getDirectorySize(directory: Directory): Int =
      directory.children.values.map {
        case d: Directory => getDirectorySize(d)
        case File(_, size) => size
      }.sum

    def apply(currentDirectory: Directory, entry: String): SystemObject = entry match {
      case dirPattern(name) => Directory(name, currentDirectory, mutable.Map.empty)
      case filePattern(size, name) => File(name, size.toInt)
    }

    private val dirPattern  = raw"dir (.+)".r
    private val filePattern = raw"(\d+) (.+)".r
  }

  class FileSystem {
    private val rootDirectory    = Directory("/", null, mutable.Map.empty)
    private var currentDirectory = rootDirectory

    def changeDirectory(path: String): Unit = {
      save()
      path match {
        case "/" =>
          currentDirectory = rootDirectory
        case ".." =>
          currentDirectory = currentDirectory.parent
        case path =>
          currentDirectory = SystemObject.getChildDirectory(path, currentDirectory.children)
      }
    }

    def setDirectoryContent(entryLine: String): Unit = {
      val systemObject = SystemObject.apply(currentDirectory, entryLine)
      currentDirectory.children.addOne(systemObject.name -> systemObject)
    }

    def printTree(level: Int = 0, directory: Directory = rootDirectory): Unit = {
      def indent(level: Int, str: String) =
        (0 to level + 1).toList
                        .map(_ => " ")
                        .foldLeft(" ")(_ + _) + " - " + str

      println(indent(level, s"${directory.name} [D] : ${SystemObject.getDirectorySize(directory)}"))

      directory.children.foreach {
        case (_, d: Directory) => printTree(level + 1, d)
        case (_, File(name, size)) => println(indent(level + 1, f"${name.padTo(10, ' ')} [F] : $size%-10d"))
      }
    }

    def getSize(children: mutable.Map[String, SystemObject]): Seq[(String, Int)] =
      children.toSeq.filter(_._2.isInstanceOf[Directory])
              .map(entry => entry.copy(_2 = entry._2.asInstanceOf[Directory]))
              .flatMap { entry =>
                Seq((entry._1, SystemObject.getDirectorySize(entry._2))) ++ getSize(entry._2.children)
              }

    def getDirectoriesSizeRecursive(directory: Directory = rootDirectory): Seq[(String, Int)] =
      getSize(mutable.Map(directory.name -> directory))

    def rootDirSize: Int = SystemObject.getDirectorySize(rootDirectory)

    private def save(): Unit = {
      if (currentDirectory == rootDirectory) return
      currentDirectory.parent.children(currentDirectory.name) = currentDirectory
    }
  }

  object FileSystem {
    private val cdPattern = raw"\$$ cd (.+)".r

    def apply(inputLines: Seq[String]) = {
      val fileSystem = new FileSystem()

      inputLines.foreach {
        case cdPattern(path) => fileSystem.changeDirectory(path)
        case "$ ls" => ()
        case other => fileSystem.setDirectoryContent(other)
      }

      fileSystem
    }
  }
}