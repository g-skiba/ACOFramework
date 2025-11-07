package project.logging

import pareto.Hypervolume2DCalculator
import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter
import java.util.concurrent.TimeUnit

class StdOutAndCsvFileBuffering2DLogger(
  writeToStdOut: Boolean,
  iterationResultsWriter: Option[PrintWriter],
  globalResultsWriter: Option[PrintWriter],
  hvCalc: Hypervolume2DCalculator
) extends AcoLogger with FileBuffering {
  private val iterationSB = new StringBuilder()
  private val globalSB = new StringBuilder()

  protected def doPrintToStdout(stdOutMsg: String, stdoutPrinter: String => Unit = println(_)): Unit = {
    if (writeToStdOut) stdoutPrinter(stdOutMsg)
  }
  protected def doPrint(msg: String): Unit = {
    doPrint(msg, msg, msg)
  }
  protected def doPrint(stdOutMsg: String, iterationFileMsg: String, globalFileMsg: String): Unit = {
    doPrintToStdout(stdOutMsg)
    iterationResultsWriter.foreach { _ =>
      iterationSB.append(iterationFileMsg)
      iterationSB.append(";")
    }
    globalResultsWriter.foreach { _ =>
      globalSB.append(globalFileMsg)
      globalSB.append(";")
    }

  }

  override def config(problemConfig: ProblemConfig, repeat: Int): Unit = {
    doPrint(s"${problemConfig.toCsv};$repeat")
  }

  override def runTimeInfo(timeNano: Long): Unit = {
    doPrint(TimeUnit.NANOSECONDS.toSeconds(timeNano).toString)
  }

  override def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit = {
    val itHV = hvCalc.calculateRemainingPartFromUnsorted(iterationResult.map(_.evaluation)).toString
    val gHV = hvCalc.calculateRemainingPartFromUnsorted(globalResult.map(_.evaluation)).toString
    doPrint(iteration + "\t" + itHV + "\t" + gHV, itHV, gHV)
  }

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = {
    val finalResult = hvCalc.calculateRemainingPartFromUnsorted(result.map(_.evaluation)).toString
//    result.foreach { res =>
//      doPrintToStdout(res.evaluation.mkString(" "))
//    }
    doPrintToStdout(finalResult, s => print(s"$s "))
  }

  override def close(): Unit = {
    writeAndClose(iterationResultsWriter, iterationSB)
    writeAndClose(globalResultsWriter, globalSB)
  }
}
