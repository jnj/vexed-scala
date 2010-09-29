import sbt.{ProjectInfo, DefaultProject}

class VexedProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalaTest = "org.scalatest" % "scalatest" % "1.2"    
}
