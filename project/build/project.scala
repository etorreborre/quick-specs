import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val mavenLocal = "Local Maven Repository" at "file://D:/mvn-repository"

  override def outputDirectoryName = "target"
  override def managedDependencyPath = "project" / "lib_managed"
  override def compileOptions = Unchecked :: super.compileOptions.toList
  override def javaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m -Xss1M") :: Nil
  override def testJavaCompileOptions = JavaCompileOption("-Xmx256m -Xms64m") :: Nil
  override def includeTest(s: String) = { s.endsWith("Spec")  }

  val scalacheck 	  = "org.scala-tools.testing" %% "scalacheck" % "1.8" 
  val testinterface = "org.scala-tools.testing" % "test-interface" % "0.5" 
  val hamcrest      = "org.hamcrest" % "hamcrest-all" % "1.1"
  val mockito 	 	  = "org.mockito" % "mockito-all" % "1.8.5" 
  val junit 		    = "junit" % "junit" % "4.7"
  val pegdown       = "org.pegdown" % "pegdown" % "0.9.2"
  val specs2        = "org.specs2" %% "specs2" % "1.2"
  
  def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
  override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

  val snapshotsRepo = "snapshots-repo" at "http://scala-tools.org/repo-snapshots"

}