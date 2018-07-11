class Rational(x: Int, y: Int) {

  require(y!=0, "Denominator must be a non-zero")

  private def gcd(a: Int, b: Int): Int =
    if (b==0) a
    else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer = x / gcd(x, y)
  def denom = y / gcd(x, y)

  def add(that: Rational) =
    new Rational(
    numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg =
    new Rational(-numer, denom)

  def sub(that: Rational) =
    new Rational(
     numer * that.denom - that.numer * denom,
      denom * that.denom)

  def less(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) =
    if (this.less(that)) that else this

  // How does it know that this method should be used
  // when printing to REPL??
  override def toString = numer + "/" + denom

}

// Examples:

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.numer
x.denom

x.add(y)
x.neg
x.sub(y)

x add y

x.less(y)
x.max(y)

x.sub(y).sub(z)

val a = new Rational(20, 40)

val strange = new Rational(1,0)
strange.add(strange)
