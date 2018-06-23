import math.abs

object fixed_point {

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean =
    abs(y - x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
   def iterate(guess: Double): Double = {
    val next = f(guess)
//     println(next)  // print for debugging
     if (isCloseEnough(guess, next)) next
     else iterate(next)
   }
    iterate(firstGuess)
  }
  /** averageDamp(f) is the return value of the function averageDamp
    * averageDamp(f) itself is a function with a return value of averageDamp(f)(x)
    * The explicit form of the function averageDamp(f) is (x+f(x)) / 2 */
  def averageDamp(f: Double => Double)(x: Double)=
    (x + f(x)) / 2

  def sqrt(x: Double): Double =
    fixedPoint(averageDamp(y => x / y))(1.0)
  sqrt(2)

}