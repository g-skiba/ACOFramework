package project.logging

import pareto.Hypervolume2DCalculator
import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter
import java.util.concurrent.TimeUnit

class StdOutAndCsvFileBuffering2DLogger(
  runId: String, 
  writeToStdOut: Boolean,
  resultsWriter: Option[PrintWriter],
  hvCalc: Hypervolume2DCalculator
) extends AcoLogger {
  private val sb = new StringBuilder()
  protected def doPrint(msg: String): Unit = {
    if (writeToStdOut) println(msg)
    sb.append(msg)
    sb.append(";")
  }

  override def config(problemConfig: ProblemConfig): Unit = {
    doPrint(problemConfig.toCsv)
  }

  override def runTimeInfo(timeNano: Long): Unit = {
    doPrint(TimeUnit.NANOSECONDS.toMillis(timeNano).toString)
  }

  override def iterationResult(iteration: Int, result: IndexedSeq[BaseSolution]): Unit = {
    doPrint(hvCalc.calculateRemainingPartFromUnsorted(result.map(_.evaluation)).toString)
  }

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = {
    doPrint(hvCalc.calculateRemainingPartFromUnsorted(result.map(_.evaluation)).toString)
  }

  override def close(): Unit = {
    val msg = sb.result()
    resultsWriter.foreach(_.println(msg))
    resultsWriter.foreach(_.close())
  }
}

