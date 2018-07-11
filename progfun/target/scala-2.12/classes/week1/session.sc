object session {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      // want sqrt(x)^2 == x -> sqrt(x)^2 - x == 0
      abs(guess * guess - x)/x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)

  }
  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  def factorial_notail(n: Int): Int =
    if (n==0) 1 else n * factorial_notail(n-1)

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n==0) acc
      else go(n-1, acc=acc*n)
    }
    go(n, acc=1)
  }

  factorial_notail(0) == factorial(0)
  factorial_notail(1) == factorial(1)
  factorial_notail(4) == factorial(4)
  factorial_notail(10) == factorial(10)


}