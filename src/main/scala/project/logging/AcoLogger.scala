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
