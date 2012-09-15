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