package u02

import u02.Optionals.Option.Some
import u02.ProductTypes.Point2D

import scala.annotation.tailrec

// Task 3a
val positiveInline: Int => String = (x: Int) =>
  x match
    case x if x > 0 => "positive"
    case x if x < 0 => "negative"

def positive(x: Int): String = x match
  case x if x > 0 => "positive"
  case x if x < 0 => "negative"

// Task 3b
val negInline: (String => Boolean) => String => Boolean =
  (f: String => Boolean) => (s: String) => !f(s)

def neg(pred: String => Boolean): String => Boolean =
  (s: String) => !pred(s)

def genericNeg[A](pred: A => Boolean) =
  (s: A) => !pred(s)

// Task 4
val thernaryRelationCurried: Int => Int => Int => Boolean =
  (x: Int) => (y: Int) => (z: Int) => x <= y && y == z

val thernaryRelationUncurried: (Int, Int, Int) => Boolean =
  (x: Int, y: Int, z: Int) => x <= y && y == z

def thernaryRelationMethodCurried(x: Int)(y: Int)(z: Int): Boolean =
  x <= y && y == z

def thernaryRelationMethodUncurried(x: Int, y: Int, z: Int): Boolean =
  x <= y && y == z

// Task 5
val compose: (Int => Int, Int => Int) => Int => Int =
  (f: Int => Int, g: Int => Int) => (x: Int) => f(g(x))

// Task 6
@tailrec
def gcd(a: Int, b: Int): Int = (a, b) match
  case (a, b) if a == 0 => b
  case (a, b) if a > b =>
    (a, b) match
      case (a, b) if a % b == 0 => b
      case (a, b) => gcd(b, a % b)

// Task 7
enum Shape:
  case Rectangle(h: Double, w: Double)
  case Square(s: Double)
  case Circle(r: Double)

object Shape:
  def perimeter(s: Shape): Double = s match
    case Rectangle(h, w) => 2 * (h + w)
    case Square(s) => 4 * s
    // the circle is centered on the origin
    case Circle(r) => 2 * math.Pi * r

  def contains(s: Shape, p: Point2D): Boolean = p match
    case Point2D(x, y) =>
      s match
        case Rectangle(h, w) => x >= 0 && x <= w && y >= 0 && y <= h
        case Square(s) => x >= 0 && x <= s && y >= 0 && y <= s
        case Circle(r) => math.sqrt(x * x + y * y) <= r

enum Option[A]:
  case Some(a: A)
  case None() // here parens are needed because of genericity

// Task 8
object Option:
  def fold[A](opt: Option[A])(ifEmpty: => A)(f: A => A): A = opt match
    case Some(a) => f(a)
    case _ => ifEmpty

  def map[A](opt: Option[A])(f: A => A): Option[A] = opt match
    case Some(a) => Some(f(a))
    case _ => None()

  def filter[A](opt: Option[A])(prd: A => Boolean): Option[A] = opt match
    case Some(a) => prd(a) match
      case v if v => Some(a)
      case _ => None()
    case _ => None()