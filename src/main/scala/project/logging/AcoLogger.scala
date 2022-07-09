package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter
import java.util.concurrent.TimeUnit
import scala.collection.mutable

trait AcoLogger {
  def config(problemConfig: ProblemConfig): Unit
  def runTimeInfo(timeNano: Long): Unit
  def iterationResult(iteration: Int, result: Seq[BaseSolution]): Unit
  def globalBestResult(result: Seq[BaseSolution]): Unit
  def close(): Unit
}

object AcoLogger {
  abstract class Basic(runId: String) extends AcoLogger {
    protected def doPrint(msg: String): Unit

    protected def printLog(msg: String): Unit = {
      val withRunId = s"[runId=$runId] $msg"
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
        result: Seq[BaseSolution]
    ): Unit = {
      printLog(s"Iteration: $iteration; Result: ${resultsString(result)}")
    }

    override def globalBestResult(result: Seq[BaseSolution]): Unit = {
      printLog(s"Global best result: ${resultsString(result)}")
    }
  }

  class StdOutAndFile(runId: String, writeToStdOut: Boolean, resultsWriter: PrintWriter)
      extends Basic(runId) {

    protected def doPrint(msg: String): Unit = {
      if (writeToStdOut) println(msg)
      resultsWriter.println(msg)
    }

    override def close(): Unit = {
      resultsWriter.close()
    }
  }

  class StdOutAndFileBuffering(runId: String, writeToStdOut: Boolean, resultsWriter: PrintWriter)
      extends Basic(runId) {
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

  class SumoLogic(runId: String, collectorUrl: String, metadata: Map[String, String]) extends Basic(runId) {
    private val sb = new StringBuilder()
    private val headers = List(("X-Sumo-Fields", metadata.map { case (k, v) => s"$k=$v"}.mkString(",")))

    protected def doPrint(msg: String): Unit = {
      sb.append(msg)
      sb.append('\n')
    }

    override def close(): Unit = {
      requests.post(collectorUrl, data = sb.result(), headers = headers)
    }
  }

  class MultiLogger(loggers: Seq[AcoLogger]) extends AcoLogger {
    override def config(problemConfig: ProblemConfig): Unit =
      loggers.foreach(_.config(problemConfig))

    override def runTimeInfo(timeNano: Long): Unit =
      loggers.foreach(_.runTimeInfo(timeNano))

    override def iterationResult(
        iteration: Int,
        result: Seq[BaseSolution]
    ): Unit = {
      loggers.foreach(_.iterationResult(iteration, result))
    }

    override def globalBestResult(result: Seq[BaseSolution]): Unit =
      loggers.foreach(_.globalBestResult(result))

    override def close(): Unit =
      loggers.foreach(_.close())
  }
}
