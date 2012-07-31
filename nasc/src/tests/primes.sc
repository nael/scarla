class LL(v: Int) {
  val x: Int = v
  var next: LL
  var hasNext: Boolean = false
}

def printSeq(l: LL): Unit = {
  printInt(l.x)
  if (l.hasNext) {
    printSeq(l.next)
  }
}

def ints(n: Int): LL = {
  if (n == 0) new LL(0) else {
    val head: LL = new LL(n)
    head.next = ints(n - 1)
    head.hasNext = true
    head
  }
}

def fsum(l: LL): Int = {
  if (l.hasNext) {
    l.x + fsum(l.next) 
  } else {
    l.x
  }
}

def isum(l: LL): Int = {
  var cur: LL = l
  var tot: Int = cur.x
  while (cur.hasNext) {
    cur = cur.next
    tot = tot + cur.x
  }
  tot
}

def isIn(l: LL, v: Int): Boolean = {
  if (l.x == v) true else {
    if (l.hasNext) isIn(l.next, v) else false
  }
}

def addMults(l: LL, v: Int, max: Int): LL = {
  var i: Int = 0
  var head: LL = l
  while (i <= max) {
    i = i + v
    val newHead: LL = new LL(i)
    newHead.next = head
    newHead.hasNext = true
    head = newHead
  }
  head
}

def sieve(n: Int): LL = {
  var primes: LL = new LL(1)
  var marked: LL = new LL(1)
  var i: Int = 1
  while (i <= n) {
    i = i + 1
    if (isIn(marked, i)) {
    } else {
      val nh: LL = new LL(i)
      nh.next = primes
      nh.hasNext = true
      primes = nh
      marked = addMults(marked, i, n)
    }
  }
  primes
}

val lim: Int = 42
val tot1: Int = 2 * fsum(ints(lim))
val tot2: Int = 2 * isum(ints(lim))
val tot3: Int = lim * (lim + 1)
printInt(tot1)
printInt(tot2)
printInt(tot3)

val primes: LL = sieve(30)
printSeq(primes)

val ps: Int = fsum(primes)
printInt(ps)