trait T1 {
  def pp(x: Int): Unit = { printInt(4)}
}

trait T2 extends T1 {
  def pp(x: Int): Unit = { printInt(888) }
}

class C1 extends T2 {
  def pp(x: Int): Unit = {
    printInt(x*x)
  }
}

class C2 extends T2 {
  def pp(x: Int): Unit = {
    printInt(2*x)
  }
}

class C3 extends T2 {}

def use(x: T2, z: Int): Unit = {
  x.pp(z)
}

native(__main) def main(): Unit = {
  val y1: C1 = new C1()
  val y2: C2 = new C2()
  val y3: C3 = new C3()
  val x1: T2 = y1
  val x2: T2 = y2
  val x3: T2 = y3
  val z1: T1 = x1
  val z2: T1 = x2
  val z3: T1 = x3
  use(x1, 4)
  use(x2, 4)
  use(x3, 4)
  z1.pp(4)
  z2.pp(4)
  z3.pp(4)
}