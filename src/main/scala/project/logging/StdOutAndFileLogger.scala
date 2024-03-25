package project.logging

import java.io.PrintWriter

class StdOutAndFileLogger(runId: String, writeToStdOut: Boolean, resultsWriter: PrintWriter)
  extends BasicAcoLogger(runId) {

  protected def doPrint(msg: String): Unit = {
    if (writeToStdOut) println(msg)
    resultsWriter.println(msg)
  }

  override def close(): Unit = {
    resultsWriter.close()
  }
}
