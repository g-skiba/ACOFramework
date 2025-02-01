package project.logging

object DebugLogger {
  private var enabled: Boolean = false

  def enable(): Unit = {
    enabled = true
    println("Debug logging enabled")
  }

  def debug(any: => Any): Unit = if (enabled) println(s"Debug: $any")
}
