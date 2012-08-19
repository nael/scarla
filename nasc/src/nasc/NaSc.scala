package nasc

import java.io.{ File, FileOutputStream }
import java.io.FileWriter
import java.io.FileReader
object G {
  var verbose = true
  val pp = new PrettyPrinter()
}
object NaSc {
  def exec() = {
    val pr = scala.sys.process.stringToProcess("cmd /C " + "do")
    pr.!!.replace("\r", "") // lol
  }

  def buildAndRun(fn: String): String = {
    val src = io.Source.fromFile(fn).mkString
    val runtimeHost = io.Source.fromFile("runtime.sc").mkString
    val runtimeIr = io.Source.fromFile("runtime.ir").mkString
    val pipeline = new ParsePhase() ++ new SymPhase() ++ new TypePhase() ++ new VirtualPhase() ++ new LiftMethodsPhase() ++ new StaticInitPhase() ++ new CodeGenPhase()
    val res = pipeline.process("{" + runtimeHost + "\n" + src + "}")
    val fw = new FileWriter("test.ir")
    fw.write(runtimeIr + "\n\n")
    fw.write(res)
    fw.close()
    exec()
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      try {
        println(buildAndRun("./src/current.sc"))
      } finally {
        G.pp.conclude()
        G.pp.toFile("./report.html")
      }
    } else {
      G.verbose = false
      val fs = new File("./src/tests").listFiles() filter { _.getName.endsWith(".sc") } toSeq

      fs foreach { f =>
        val fw = io.Source.fromFile(f.getParent() + "/" + f.getName + ".result")
        val res = buildAndRun(f.getAbsolutePath())
        val exp = fw.mkString.replace("\r", "") // lol
        if (exp == res) { println("test " + f.getName + " ok") }
        else {
          println("test " + f.getName + " fails :")
          println("expected :")
          println(exp)
          println("got :")
          println(res)
        }
      }
    }
  }


}
