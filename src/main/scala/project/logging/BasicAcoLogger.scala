package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

import java.util.concurrent.TimeUnit

abstract class BasicAcoLogger(runId: String) extends AcoLogger {
  protected def doPrint(msg: String): Unit

  protected def printLog(msg: String): Unit = {
    val withRunId = s"[id=$runId] $msg"
    doPrint(withRunId)
  }

  protected def resultsString(result: Seq[BaseSolution]): String = {
    result
      .map { solution =>
        solution.evaluation.mkString("(", ",", ")")
      }
      .mkString("[", ",", "]")
  }

  override def config(problemConfig: ProblemConfig): Unit = {
    printLog(s"Config: $problemConfig")
  }

  override def runTimeInfo(timeNano: Long): Unit = {
    printLog(s"Run took: ${TimeUnit.NANOSECONDS.toMillis(timeNano)}ms")
  }

  override def iterationResult(
    iteration: Int,
    result: IndexedSeq[BaseSolution]
  ): Unit = {
    printLog(s"I: $iteration; R: ${resultsString(result)}")
  }

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = {
    printLog(s"Global best result: ${resultsString(result)}")
  }
}
