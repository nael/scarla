native(printInt) def printInt(x: Int): Unit

native(i32_plus) def +(x: Int, y: Int): Int
native(i32_minus) def -(x: Int, y: Int): Int
native(i32_times) def *(x: Int, y: Int): Int
native(i32_div) def /(x: Int, y: Int): Int
native(i32_mod) def %(x: Int, y: Int): Int

native(i32_equals) def ==(x: Int, y: Int): Boolean
native(i32_lte) def <=(x: Int, y: Int): Boolean
native(i32_lt) def <(x: Int, y: Int): Boolean
native(i32_gte) def >=(x: Int, y: Int): Boolean
native(i32_gt) def >(x: Int, y: Int): Boolean

class Chunk[T] {
  
  val ptr: CPtr
  val size: Int = 0
  def allocate(newSize: Int): Unit = {
    size = newSize
  __ir__("""
  %sz0 = getelementptr [t:T]* null, i64 1 
%sz1 = ptrtoint [t:T]* %sz0 to i64
%size64 = zext i32 [s:size] to i64
%sz2 = mul i64 %sz1, %size64
%addr = call i8* @malloc(i64 %sz2)
  store i8* %addr, i8** [p:ptr]
  """)
  }
  
  def get(index: Int): T = {
    val res: T
    __ir__("""
      %Tptr = bitcast i8* [s:ptr] to [t:T]*
      %eptr = getelementptr [t:T]* %Tptr, i32 [s:index]
      %e = load [t:T]* %eptr
      store [t:T] %e, [t:T]* [p:res]
      """)
    res
  }
  
  def set(index: Int, v: T): Unit = {
    __ir__("""
      %Tptr = bitcast i8* [s:ptr] to [t:T]*
      %eptr = getelementptr [t:T]* %Tptr, i32 [s:index]
      store [t:T] [s:v], [t:T]* %eptr
    """)
  }
}

class Array[T](size: Int) {
  val mem = new Chunk[T]
  mem.allocate(size)
  def get(i: Int) : T = mem.get(i)
  def set(i: Int, v: T): Unit = mem.set(i, v)
}