
class Mat22(val x1: Int, val x2:Int, val y1:Int, val y2:Int) {}

def mulMat( x : Mat22, y : Mat22) : Mat22 = {
	val x1 : Int = x.x1 * y.x1 + x.x2 * y.y1
	val x2 : Int = x.x1 * y.x2 + x.x2 * y.y2
	val y1 : Int = x.y1 * y.x1 + x.y2 * y.y1
	val y2 : Int = x.y1 * y.x2 + x.y2 * y.y2
	new Mat22(x1,x2,y1,y2)
}

def powMat( m : Mat22, n : Int ) : Mat22 = {
	if (n == 0) { 
		new Mat22(1,0,0,1)
	} else {
		val tmp : Mat22 = powMat( m, n/2 )
		tmp = mulMat(tmp, tmp)
		if ( n%2 == 0 ){
			tmp
		} else {
			mulMat(m, tmp)
		}
	}
}

native(__main) def main(): Unit = {
	val m : Mat22 = new Mat22(1, 1, 1, 0)
	m = powMat(m, 35)
	printInt(m.x2)
}
