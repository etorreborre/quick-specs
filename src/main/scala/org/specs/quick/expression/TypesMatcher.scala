package org.specs.quick.expression

trait TypesMatcher {
  private val loader = getClass.getClassLoader
  /**
   * @return true if t2 is assignable with t1
   */
  def typesMatch(t1: String, t2: String) = {
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
	t1 == t2 ||
	t1.takeWhile(_ != '[') == t2.takeWhile(_ != '[') ||
	c1InstanceOfc2
  }
}