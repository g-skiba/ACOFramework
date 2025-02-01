package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

import java.io.PrintWriter
import java.util.concurrent.TimeUnit
import scala.collection.mutable

trait AcoLogger {
  def config(problemConfig: ProblemConfig, repeat: Int): Unit
  def runTimeInfo(timeNano: Long): Unit
  def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit
  def globalBestResult(result: IndexedSeq[BaseSolution]): Unit
  def close(): Unit
}
