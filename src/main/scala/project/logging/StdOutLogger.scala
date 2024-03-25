package project.logging

class StdOutLogger(runId: String) extends BasicAcoLogger(runId) {
  protected def doPrint(msg: String): Unit = {
    println(msg)
  }

  override def close(): Unit = {
  }
}
