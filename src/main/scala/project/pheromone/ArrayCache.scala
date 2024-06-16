package project.pheromone

import scala.reflect.ClassTag

class ArrayCache[V: ClassTag](maxKey: Int, emptyValue: V) {
  private val array: Array[V] = Array.fill(maxKey + 1)(emptyValue)
  
  def getOrElseUpdate(key: Int, value: => V): V = {
    if (array(key) == emptyValue) {
      array(key) = value
    }
    array(key)
  }

  def clear(): Unit = {
    array.mapInPlace(_ => emptyValue)
  }
}
