package project.pheromone

import scala.reflect.ClassTag

class ArrayCache[V <: AnyRef](maxKey: Int) {
  private val entries = new Array[AnyRef](maxKey + 1)

  inline def getOrElseUpdate(key: Int, inline value: V): V = {
    val cached = entries(key)
    if (cached ne null) cached.asInstanceOf[V]
    else {
      val computed = value
      entries(key) = computed
      computed
    }
  }

  def clear(): Unit = {
    java.util.Arrays.fill(entries, null)
  }
}
