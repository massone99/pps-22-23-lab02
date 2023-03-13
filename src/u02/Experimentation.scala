package u02

val positiveInline: Int => String = (x: Int) =>
  x match
    case x if x > 0 => "positive"
    case x if x < 0 => "negative"

def positive(x: Int): String = x match
  case x if x > 0 => "positive"
  case x if x < 0 => "negative"

val negInline: (String => Boolean) => String => Boolean =
  (f: String => Boolean) => (s: String) => !f(s)

def neg(pred: String => Boolean): String => Boolean =
  (s: String) => !pred(s)

def genericNeg[A](pred: A => Boolean) =
  (s: A) => !pred(s)

val thernaryRelationCurried: Int => Int => Int => Boolean =
  (x: Int) => (y: Int) => (z: Int) => x <= y && y == z

val thernaryRelationUncurried: (Int, Int, Int) => Boolean =
  (x: Int, y: Int, z: Int) => x <= y && y == z

def thernaryRelationMethodCurried(x: Int)(y: Int)(z: Int): Boolean =
  x <= y && y == z

def thernaryRelationMethodUncurried(x: Int, y: Int, z: Int): Boolean =
  x <= y && y == z

val compose: (Int => Int, Int => Int) => Int => Int =
  (f: Int => Int, g: Int => Int) => (x: Int) => f(g(x))