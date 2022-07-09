import java.util.concurrent.{Executors, ThreadFactory}
import scala.concurrent.ExecutionContext

object ExecutionContextHelper {
  private def logAsError(t: Throwable): Unit = {
//    logger.error(t.getMessage, t)
    t.printStackTrace()
  }

  def namingThreadFactory(
    namePrefix: String,
    daemon: Boolean = true,
    setUncaughtExceptionHandler: Boolean = false
  ): ThreadFactory = (r: Runnable) => {
    val thread = Executors.defaultThreadFactory().newThread(r)
    thread.setName(s"$namePrefix-${thread.getId}")
    thread.setDaemon(daemon)
    thread.setUncaughtExceptionHandler((_: Thread, e: Throwable) => logAsError(e))
    thread
  }

  def cached(namePrefix: String): ExecutionContext = {
    ExecutionContext.fromExecutor(
      Executors.newCachedThreadPool(namingThreadFactory(namePrefix)),
      logAsError
    )
  }

  def fixed(namePrefix: String, size: Int = Runtime.getRuntime.availableProcessors()): ExecutionContext = {
    ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(size, namingThreadFactory(namePrefix)),
      logAsError
    )
  }

}
