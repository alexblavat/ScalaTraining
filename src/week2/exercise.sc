import math.abs
object exercise {
  def factorial(n:Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if(n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  factorial(4)
  //summation Higher Order Function
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
  sum(x => x * x, 3, 5 )

  //Functions returning functions - Lec 2.2 sum returns functions as its result
  def sum2(f:Int => Int):(Int, Int) => Int = {
    def sumF(a:Int, b:Int): Int = {
      if(a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }

  //product curried function
  def product (f: Int => Int)(a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * product(f)(a+1, b)
  product(x=> x * x)(3,7)

  //factorial in terms of product
  def fact(n: Int) = product(x => x )(1,n)
  fact(5)

  //more generic function mapReduce
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

  //defining product in terms of MapReduce
  //product curried function
  def prod (f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a,b)
  prod(x=> x * x)(3,4)

  /*
  * Finding Fixed Points - Lecture 2.3
  * */
  val tolerance = 0.0001
  def isCloseEnough(x:Double, y:Double) =
    abs((x-y)/x)/x < tolerance
  def fixedPoint(f:Double=>Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess = "+ guess)
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  println("Fixed Point call")
  fixedPoint(x=>1 + x/2)(1)

  def sqrt(x: Double) = fixedPoint(y => ((y+x)/y)/2)(1.0)
  sqrt(2)
}