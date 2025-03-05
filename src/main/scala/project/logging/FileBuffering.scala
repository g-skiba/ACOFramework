package project.logging

import java.io.PrintWriter

trait FileBuffering {
  def writeAndClose(writer: Option[PrintWriter], msgSB: StringBuilder): Unit = {
    writer.foreach { w =>
      val msg = msgSB.result()
      w.println(msg)
      w.close()
    }
  }
}
