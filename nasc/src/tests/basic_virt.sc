trait T {
  def v1(x: Int, y: Int): Int
  def v2(): Unit = { printInt(v1(4,2)) }
}

class Add extends T {
  def v1(x: Int, y: Int): Int = { x+y }
  def v2(): Unit = { printInt(1) }
}

class Mul extends T {
  def v1(x: Int, y: Int): Int = { x*y }
  def v2(): Unit = { printInt(2) }
}

class Sub extends T {
  def v1(x: Int, y: Int): Int = { x-y }
  def v2(): Unit = { printInt(3) }
}

class Proj1 extends T {
  def v1(x: Int, y: Int): Int = { x }
}

def tbo(o: T): Unit = {
  o.v2()
  printInt(o.v1(4,6))
}

native(__main) def main(): Unit = {
  val add: Add = new Add()
  val mul: Mul = new Mul()
  val sub: Sub = new Sub()
  val p1: Proj1 = new Proj1()
  tbo(add)
  tbo(mul)
  tbo(sub)
  tbo(p1)
}