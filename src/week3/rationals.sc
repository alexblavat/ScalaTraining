
object rationals {
  class Rational(x:Int, y:Int) {
    private def gcd(a: Int, b:Int):Int = if(b==0) a else gcd(b, a % b)
    private val g = gcd(x,y)
    def numer = x / g
    def denom = y / g
    def add(that: Rational) =
      new Rational(numer * that.denom + that.numer * denom,
        denom * that.denom)

    def neg: Rational = new Rational(-numer, denom)

    def sub(that: Rational) = add(that.neg)

    def less(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) =
      if (this.less(that)) that else this

    override def toString = numer + "/" + denom
  } // end class
  println("====================================================")
  def addRational(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom, r.denom * s.denom
    )

  def makeString(r: Rational) =
    r.numer + "/" + r.denom

  makeString(addRational(new Rational(1,2), new Rational(2,3)))
  val a = new Rational(1,2)
  val b = new Rational(2,3)
  a.add(b)
  a.neg

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.add(y)
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)
  x.max(y)



}

