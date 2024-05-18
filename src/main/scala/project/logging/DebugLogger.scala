package project.logging

object DebugLogger {
  private var enabled: Boolean = false

  def enable(): Unit = {
    enabled = true
    debug("Debug logging enabled")
  }

  def debug(any: => Any): Unit = if (enabled) println(any)
}
