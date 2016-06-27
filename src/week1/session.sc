object session {
  println("Welcome to scala session")
  def abs(x: Double) = if (x < 0) -x else x
  abs(-5.3)

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(25)
  sqrt(1e-6)
  sqrt(x = 1e60)

  21 % 14

  def factorial(x: Int): Int =
    if(x == 0) 1 else x * factorial(x - 1)

  factorial(5)


}