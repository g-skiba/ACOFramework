package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

import java.util.concurrent.TimeUnit

/**
 * NOTE: Can be used to make irace treat time taken by the algorithm run as a result (might be useful for
 * measuring execution time of the algorithm when testing configurations against test instances list).
 */
class IraceTimeAsResultStdOutLogger extends AcoLogger {
  def config(problemConfig: ProblemConfig): Unit =
    println(problemConfig)

  def runTimeInfo(timeNano: Long): Unit =
    println(s"${TimeUnit.NANOSECONDS.toMillis(timeNano)} ${TimeUnit.NANOSECONDS.toSeconds(timeNano)}")

  def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit =
    println((iteration, iterationResult.head.evaluation.head, globalResult.head.evaluation.head))

  def globalBestResult(result: IndexedSeq[BaseSolution]): Unit = ()
  //      print(result.head.evaluation.head)

  def close(): Unit = ()
}
