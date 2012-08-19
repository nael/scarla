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
          println(exp toList)
          println("got :")
          println(res toList)
        }
      }
    }
  }

  def main2(args: Array[String]): Unit = {
    if (args.length > 0) {
      G.verbose = false
      val expct = Map(
        p6 -> "6\n2\n",
        p7 -> "42\n24\n",
        p8 -> "41\n14\n",
        p9 ->
          """1806
1806
1806
31
29
23
19
17
13
11
7
5
3
2
1
161
""")
      var i = 0
      expct.foreach {
        case (p, e) =>
          comp(p)
          val u = exec()
          if (u == e) println("Test " + i + " ok")
          else {
            println("Test " + i + " ko : ")
            println("Expected :")
            println(e)
            println("Got")
            println(u)
            println("abort")
            return
          }
          i += 1
      }
      println("gg")
    } else { comp(p7); println("Result:"); println(exec()) }
  }
  val p0 = """{
    class A {}
    class B {}
    def f(a: A): B = {}
    val aa: Function[A,B] = f
    val zz: Function[A,B]
    val uu: Function[A,Int]
    val a : Int
    val bb: B = aa(a)
    val cc: B = aa(a)
    }"""
  val q0 = """{
      native(plus) def +(x: Int, y: Int): Int
      native(printInt) def printInt(x: Int): Unit
      native(equals) def ==(x: Int, y: Int): Boolean
            native(lte) def <=(x: Int, y: Int): Boolean

      class A(z: Int) {
         val a : Int = 666 + z
         printInt(85)
         def k(): Unit = { a = a + 1 }
         def p(): Unit = { printInt(a) }
      }
      val i: Int = 0
      while(i <= 3) {
         printInt(i)
         i = i + 1
      }
    }"""
  val p9 = """{g

  }"""

  val q1 = """
        native(plus) def +(x: Int, y: Int): Int
      native(printInt) def printInt(x: Int): Unit
      
      def f(x: Int, y: Int): Int = y + y
      val zz : Function[Int, Int, Int] = f
      val a : Int = zz(1,2)
      printInt(a)
    }
        """
  def comp(p: String) = {
    /*
       * p6: funptr & ref test => 6\n2
       * p7: basic struct & ctor => 42\n24
       * p8: basic class & ctor => 41\n14
       * p9: basic class, looping, recursion => 1806\n1806\n1806\n[primes numbers up to k in reverse order]\n1\n1061
       */

    /*    val comp =
        new ParsePhase() ++
          new SymPhase() ++
          new TypePhase() ++
 //         new TraitPhase() ++
 //         new StructPhase() ++
          new RefPhase() ++
          new CodeGenPhase()
      val code = comp.process(p)
      val fw = new FileWriter("test.ir")
      fw.write(code)
      fw.close()
     G.pp.toFile("report.html")*/

  }
  val p = """{
	      def fact(x : Int) : Int = {
	          val u : Int = "plop";
	          def tmp(u : Int, y : Int) : Int = x + u * y;
			  if(x == 0) {
			  	1
			  } else {
			  	printInt(x);
			  	tmp(x, fact(x-1))
			  }
	  	  };
	      printInt(fact(4))
	}"""
  val p2 = """{
	      def a(x : Int) : Int = {
	         def b(y : Int) : Int = x;
	         b(2)
	         2
	      };
	     a(2)
	}"""
  val p3 = """{
        def fact(n : Int) : Int = {
           if(n == 0) { 1 } else {
              fact(n-1)*n
           }
        };
        printInt(fact(8))
	}"""
  val p4 = """{
      struct A { val x : Int = 2; val y : Int = 3 };
      val a : Ref[A] = A();
      printInt(a.x)
  }"""
  val p5 = """{
      
      trait E {
        def u() : Int = { 2 }
      }
      struct A extends E {
        def u() : Int = { 4 }
      }
      
      val a : E
      a.u()
  }"""
  val p6 = """{
      def u(k : Int): Unit = {
      	printInt(k + 2)
      }
      def v(k: Int): Unit = {
        printInt(k - 2)
      }
      def mod(ref a : (Int) => Unit) : Unit = {
         a = v
      }
      var b : (Int) => Unit = u
      b(4)
      mod(b)
      b(4)
      }"""
  val p7 = """{
    value class A(x : Int) {
      val a1 : Int = 3
      var a2 : Int = x-1
      def z() : Int = { printInt(42); a1 } 
      def p(u : Int): Int = {a1 + (a2*u + z())} 
    }
    val z : A = A(8)
    z.a2 = z.a2 + 2
    val zp : A = z
    zp.a2 = 0
    printInt(z.p(2))
  }"""
  val p8 = """{
     class A(x : Int) {
       val a1 : Int = 3
       var a2 : Int = x-1
       def z() : Int = { printInt(41); a1 } 
       def p(u : Int): Int = {a1 + (a2*u + z())} 
     }
    val z : A = A(8)
    z.a2 = z.a2 + 2
    val zp : A = z
    zp.a2 = 1
    printInt(z.p(8))
  }"""
  val p9x = """{
        class LL(v : Int) {
          val x : Int = v
          var next : LL
          var hasNext : Int = 0
          def prepend(y : Int) : LL = {
             val head = LL(y)
             head.next = this
             head.hasNext = 1
          }
        }
        def printSeq(l : LL) : Unit = {
          printInt(l.x)
          if(l.hasNext == 1) printSeq(l.next)
        }
        def ints(n : Int) : LL = {
           if(n == 0) LL(0)
           else {
             ints(n-1).prepend(n)
           }
        }
        printSeq(ints(5))
  }"""

  val xp9 = """{

class LL(v : Int) {
          val x : Int = v
          var next : LL
          var hasNext : Int = 0
}

def printSeq(l : LL) : Int = {
          printInt(l.x)
          if(l.hasNext == 1) { printSeq(l.next); 0} else { 0 }
}
def ints(n : Int) : LL = {
          if(n == 0) LL(0) else {
          	val  head : LL = LL(n)
          	head.next = ints(n-1)
          	head.hasNext = 1
          	head
          }
}
def fsum(l : LL) : Int = {
          if(l.hasNext == 0) l.x else {
          	l.x + fsum(l.next)
          }
}
def isum(l : LL) : Int = {
          var cur : LL = l
          var tot : Int = cur.x
          while(cur.hasNext == 1) {
             cur = cur.next
             tot = tot + cur.x
          }
          tot
}
def isIn(l : LL, v : Int) : Int = {
         val u : Int = if(l.hasNext == 1) isIn(l.next, v) else 0
         if(l.x == v) 1 else {
          u
         }
}
def addMults(l : LL, v : Int, max : Int) : LL = {
          var i : Int = 0
          var head : LL = l
          while(i <= max) {
          	i = i + v
            val newHead : LL = LL(i)
            newHead.next = head
            newHead.hasNext = 1
            head = newHead
          }
          head
}
def sieve(n : Int) : LL = {
        var primes : LL = LL(1)
        var marked : LL = LL(1)
        var i : Int = 1
        while(i <= n) {
          i = i + 1
          if(isIn(marked, i) == 0) {
            val nh : LL = LL(i)
            nh.next = primes
            nh.hasNext = 1
            primes = nh
            marked = addMults(marked, i, n)
            0
          } else { 0 }
        }
            primes
}
val lim : Int = 42
val tot1 : Int = 2*fsum(ints(lim))
val tot2 : Int = 2*isum(ints(lim))
val tot3 : Int = lim*(lim + 1)
printInt(tot1)
printInt(tot2)
printInt(tot3)
val primes : LL = sieve(30)
printSeq(primes)
val ps : Int = fsum(primes)
printInt(ps)
  }"""
  val p10 = """{
      trait E {
        def wazza() : Int = { 8 }
        def wdd(xz : Int) :Int = wazza()+1      
      }
      class A(xx : Int) extends E {
        var x : Int = xx
        def wazza():Int = x
        def u():Int = wazza()  
      }
      val b : E
      val a : A = A(45)
      printInt(a.u())
  }"""

}
