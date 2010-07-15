package org.specs.util

trait TypesMatcher {
  private val loader = getClass.getClassLoader
  /**
   * @return true if t2 is assignable with t1 for all t2, t1
   */
  def typesMatch(t1: Seq[String], t2: Seq[String]): Boolean = {
	t1.size == t2.size &&
	(t1 zip t2).forall { case (a, b) => typesMatch(a, b) }
  }
  /**
   * @return true if t2 is assignable with t1
   */
  def typesMatch(t1: String, t2: String): Boolean = {
	def load(c: String, m: String): Option[Class[_]] = {
  	  try {
	    Some(loader.loadClass(c))
	  } catch {
	    case e: java.lang.ClassNotFoundException => None
	  }
	}
	def c1InstanceOfc2 = {
	  val c2 = load(t2, "couldn't find class " +  t2 + " to check if it is assignable from " + t1)
	  val c1 = load(t1, "couldn't find class " +  t1 + " to check if it can be assigned to " + t2)
	  c2.map(k2 => c1.map(k2.isAssignableFrom(_))).flatMap(c => c).getOrElse(false)
	}
	t1.toLowerCase == t2.toLowerCase ||
	t1.takeWhile(_ != '[') == t2.takeWhile(_ != '[') ||
	t1.replace("java.lang.", "") == t2.replace("java.lang.", "") ||
	t1 == "java.lang.Object" || t2 == "java.lang.Object" ||
	c1InstanceOfc2
  }
}