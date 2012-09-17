native(__main) def main(): Unit = {
  val n = 18
  val a = new Array[Int](n)
  val c = new Array[Boolean](n)
  val i = 0
  while(i < n) {
    a.set(i, 2*i)
    c.set(i, i % 2 == 0)
    i = i + 1
  }
  i = 0
  while(i < n) {
    if(c.get(i)) printInt(a.get(i))
    i = i + 1
  }
}
