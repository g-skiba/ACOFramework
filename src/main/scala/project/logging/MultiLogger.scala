package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

class MultiLogger(loggers: Seq[AcoLogger]) extends AcoLogger {
  override def config(problemConfig: ProblemConfig): Unit =
    loggers.foreach(_.config(problemConfig))

  override def runTimeInfo(timeNano: Long): Unit =
    loggers.foreach(_.runTimeInfo(timeNano))

  override def iterationResult(
    iteration: Int,
    result: IndexedSeq[BaseSolution]
  ): Unit = {
    loggers.foreach(_.iterationResult(iteration, result))
  }

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit =
    loggers.foreach(_.globalBestResult(result))

  override def close(): Unit =
    loggers.foreach(_.close())
}
