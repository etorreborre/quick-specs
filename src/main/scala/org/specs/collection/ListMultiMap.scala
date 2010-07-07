package org.specs.collection
import scala.collection.mutable._
class ListMultiMap[A, B] {
  private val map = new HashMap[A, scala.collection.mutable.ListBuffer[B]]
  
  def put(a: A, b: B*): ListMultiMap[A, B] = {
	if (!map.get(a).isDefined) map.put(a, new ListBuffer[B])
	map(a).appendAll(b)
	this
  }
  def apply(a: A): scala.collection.immutable.List[B] = map.get(a).map(_.toList).flatten[B].toList

  override def toString = map.map { case (k, v) => (k, v.toList.mkString("[", ", ", "]")) }.toString
}
