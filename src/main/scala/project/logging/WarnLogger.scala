package project.logging

object WarnLogger {
  private var enabled: Boolean = true

  def disable(): Unit = {
    enabled = true
    warn("Debug logging enabled")
  }

  def warn(any: => Any): Unit = if (enabled) println(any)
}
