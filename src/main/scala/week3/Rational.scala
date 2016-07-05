package week3

class Rational(x:Int, y:Int) {
  require (y != 0, "Denominator must be non-zero")
  private def gcd(a: Int, b:Int):Int = if(b==0) a else gcd(b, a % b)
  private val g = gcd(x,y)
  def numer = x / g
  def denom = y / g

  def this(x: Int) = this (x,1)

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this.<(that)) that else this

  override def toString = {
    numer/g + "/" + denom/g
  }
} // end class



