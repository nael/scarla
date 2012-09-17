native(__main) def main(): Unit = {
  val n = 100 
  val c = new Array[Boolean](n+1)
  val i = 2
  while(i <= n) {
    c.set(i, true)
    i = i + 1
  }
  c.set(0, false)
  c.set(1, false)
  i = 0
  while(i <= n) {
    if(c.get(i)) {
      val j = 2*i
      while(j <= n) {
        c.set(j, false)
        j = j + i
      }
    }
    i = i + 1
  }
  i = 0
  while(i <= n) {
    if(c.get(i)) printInt(i)
    i = i + 1
  }
}
