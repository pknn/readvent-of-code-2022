package core

sealed class Kind(val fileName: String)

object Kind {
  case object Test extends Kind("test")

  case object Input extends Kind("input")
}