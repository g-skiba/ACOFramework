package project.logging

import pareto.Hypervolume2DCalculator
import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter

class StdOutAndCsvFileBufferingRepeatAgnosticOnlyFinalResult2DLogger(
  writeToStdOut: Boolean,
  resultsWriter: Option[PrintWriter],
  hvCalc: Hypervolume2DCalculator
) extends AcoLogger with FileBuffering {
  private val fileSB = new StringBuilder()

  protected def doPrintToStdout(stdOutMsg: String, stdoutPrinter: String => Unit = println(_)): Unit = {
    if (writeToStdOut) stdoutPrinter(stdOutMsg)
  }
  protected def doPrint(msg: String): Unit = {
    doPrint(msg, msg)
  }
  protected def doPrint(stdOutMsg: String, fileMsg: String): Unit = {
    doPrintToStdout(stdOutMsg)
    resultsWriter.foreach { _ =>
      fileSB.append(fileMsg)
      fileSB.append("\n")
    }
  }

  override def config(problemConfig: ProblemConfig, repeat: Int): Unit = {
    doPrint(s"${problemConfig.toCsv}")
  }

  override def runTimeInfo(timeNano: Long): Unit = {}

  override def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit = {}

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = {
    val finalResult = hvCalc.calculateRemainingPartFromUnsorted(result.map(_.evaluation)).toString
    doPrint(finalResult)
  }

  override def close(): Unit = {
    writeAndClose(resultsWriter, fileSB)
  }
}
