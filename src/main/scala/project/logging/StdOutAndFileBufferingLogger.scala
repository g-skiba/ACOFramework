package project.logging

import java.io.PrintWriter

class StdOutAndFileBufferingLogger(runId: String, writeToStdOut: Boolean, resultsWriter: PrintWriter)
  extends BasicAcoLogger(runId) {
  private val sb = new StringBuilder()

  protected def doPrint(msg: String): Unit = {
    sb.append(msg)
    sb.append('\n')
  }

  override def close(): Unit = {
    val msg = sb.result()
    if (writeToStdOut) println(msg)
    resultsWriter.println(msg)
    resultsWriter.close()
  }
}
