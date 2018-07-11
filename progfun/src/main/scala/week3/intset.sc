
/**
  * Abstract classes form the template for its subclasses
  * The subclasses implement the methods declared in the abstract class
  * */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this

  def union(other: IntSet): IntSet =
  // take union of smaller sets first, include the element
  // elem of this, then take union with other
    ((left union right) incl elem) union other

  override def toString = "{" + left + elem + right + "}"
}

val t1 = new NonEmpty(5, new Empty, new Empty)
val t2 = new NonEmpty(12, new Empty, new Empty)
val t3 = new NonEmpty(7, t1, t2)
val t3_incl = t3.incl(3)
