package project.logging

import project.config.ProblemConfig
import project.solution.BaseSolution

import java.util.concurrent.TimeUnit


/**
 * NOTE: By default irace script expects the result to be the first number in the last output line. It should be
 * 'cost time' if time budget is used. This means that the order of calls is crucial, the last methods called causing
 * output should be globalBestResult() and runTimeInfo().
 */
class IraceSingleObjectiveStdOutLogger extends AcoLogger {
  def config(problemConfig: ProblemConfig): Unit =
    println(problemConfig)

  def runTimeInfo(timeNano: Long): Unit =
    println(s" ${TimeUnit.NANOSECONDS.toSeconds(timeNano)}")

  def iterationResult(iteration: Int, iterationResult: IndexedSeq[BaseSolution], globalResult: IndexedSeq[BaseSolution]): Unit =
    println(s"$iteration\t${iterationResult.head.evaluation.head}\t${globalResult.head.evaluation.head}")

  def globalBestResult(result: IndexedSeq[BaseSolution]): Unit =
    print(result.head.evaluation.head)

  def close(): Unit = ()
}
