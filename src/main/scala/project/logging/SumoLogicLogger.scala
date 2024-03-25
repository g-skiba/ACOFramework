package project.logging

class SumoLogicLogger(runId: String, collectorUrl: String, metadata: Map[String, String]) extends BasicAcoLogger(runId) {
  private val sb = new StringBuilder()
  private val headers = List(("X-Sumo-Fields", metadata.map { case (k, v) => s"$k=$v" }.mkString(",")))

  protected def doPrint(msg: String): Unit = {
    sb.append(msg)
    sb.append('\n')
  }

  override def close(): Unit = {
    requests.post(collectorUrl, data = sb.result(), headers = headers)
  }
}
