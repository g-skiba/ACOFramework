package project.logging

object WarnLogger {
  private var enabled: Boolean = true

  def disable(): Unit = {
    enabled = false
    println("Warn logging disabled")
  }

  def warn(any: => Any): Unit = if (enabled) println(s"Warn: $any")
}
