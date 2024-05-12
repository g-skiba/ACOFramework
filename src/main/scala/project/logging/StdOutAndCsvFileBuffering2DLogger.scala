package project.logging

import pareto.Hypervolume2DCalculator
import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter
import java.util.concurrent.TimeUnit

class StdOutAndCsvFileBuffering2DLogger(
  runId: String,
  writeToStdOut: Boolean,
  iterationResultsWriter: Option[PrintWriter],
  globalResultsWriter: Option[PrintWriter],
  hvCalc: Hypervolume2DCalculator
) extends AcoLogger {
  private val iterationSB = new StringBuilder()
  private val globalSB = new StringBuilder()

  protected def doPrint(msg: String): Unit = {
    doPrint(msg, msg, msg)
  }
  protected def doPrint(stdOutMsg: String, iterationFileMsg: String, globalFileMsg: String): Unit = {
    if (writeToStdOut) println(stdOutMsg)
    iterationResultsWriter.foreach { _ =>
      iterationSB.append(iterationFileMsg)
      iterationSB.append(";")
    }
    globalResultsWriter.foreach { _ =>
      globalSB.append(globalFileMsg)
      globalSB.append(";")
    }

  }

  override def config(problemConfig: ProblemConfig): Unit = {
    doPrint(problemConfig.toCsv)
  }

  override def runTimeInfo(timeNano: Long): Unit = {
    doPrint(TimeUnit.NANOSECONDS.toMillis(timeNano).toString)
  }

  override def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit = {
    val itHV = hvCalc.calculateRemainingPartFromUnsorted(iterationResult.map(_.evaluation)).toString
    val gHV = hvCalc.calculateRemainingPartFromUnsorted(globalResult.map(_.evaluation)).toString
    doPrint(iteration + "\t" + itHV + "\t" + gHV, itHV, gHV)
  }

  override def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = {
    // this is written in the global file as a result of last iteration
//    doPrint(hvCalc.calculateRemainingPartFromUnsorted(result.map(_.evaluation)).toString)
  }

  override def close(): Unit = {
    def writeAndClose(writer: Option[PrintWriter], msgSB: StringBuilder): Unit = {
      writer.foreach { w =>
        val msg = msgSB.result()
        w.println(msg)
        w.close()
      }
    }
    writeAndClose(iterationResultsWriter, iterationSB)
    writeAndClose(globalResultsWriter, globalSB)
  }
}

