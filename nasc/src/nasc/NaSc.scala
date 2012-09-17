package nasc

import java.io.{ File, FileOutputStream }
import java.io.FileWriter
import java.io.FileReader
object G {
  var verbose = true
  val pp = new PrettyPrinter()
}
object NaSc {

  def rem() = {
    val tmps = List(new File("_tmp.ir"), new File("_tmp.bc"))
    tmps foreach { x => x.deleteOnExit(); x.delete() }
  }

  def exec(log: Boolean) = {
    val asm = scala.sys.process.stringToProcess("llvm-as -o=_tmp.bc _tmp.ir")
    asm!!
    val pr = scala.sys.process.stringToProcess("lli _tmp.bc")
    if (log) pr.!!.replace("\r", "") // lol
    else { pr!; "" }
  }

  def buildAndRun(fn: String, log: Boolean = true): String = {
    rem()
    if (!log) print("[building ...")
    val src = io.Source.fromFile(fn).mkString
    val runtimeHost = io.Source.fromFile("runtime.sc").mkString
    val runtimeIr = io.Source.fromFile("runtime.ir").mkString
    val pipeline = new ParsePhase() ++ new InlineIRPhase() ++ new SymPhase() ++ new TemplateExpansionPhase() ++ new TypePhase() ++ new VirtualPhase() ++ new LiftMethodsPhase() ++ new StaticInitPhase() ++ new CodeGenPhase()
    val res = pipeline.process("{" + runtimeHost + "\n" + src + "}")
    val fw = new FileWriter("_tmp.ir")
    fw.write(runtimeIr + "\n\n")
    fw.write(res)
    fw.close()
    if (!log) println(" ok]")
    exec(log)
  }

  def go(args: Array[String]) = {
    if (args.length == 0) {
      try {
        println(buildAndRun("./src/current.sc"))
      } finally {
        G.pp.conclude()
        G.pp.toFile("./report.html")
      }
    } else if (args(0) == "test") {
      G.verbose = false
      val fs = new File("./src/tests").listFiles() filter { _.getName.endsWith(".sc") } toSeq

      println("start")
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
      println("end")
    } else {
      G.verbose = false
      buildAndRun(args(0), false)
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      go(args)
    } catch {
      case e: Exception => { println("Unrecoverable :"); e.printStackTrace() }
    }
  }

}
