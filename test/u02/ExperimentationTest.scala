package u02

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import u02.Optionals.Option.filter
import u02.Optionals.Option
import u02.ProductTypes.Point2D
import u02.Shape.{contains, perimeter}
import Option.*

class ExperimentationTest {

  val testString = "ciao"
  val notEmptyPred: (String => Boolean) = s => s.isEmpty

  @Test def testPositiveInline(): Unit =
    val x: Int = 1
    val y: Int = -1
    assertEquals("positive", positiveInline(x))
    assertEquals("negative", positiveInline(y))
  @Test def testPositive(): Unit =
    val x: Int = 1
    val y: Int = -1
    assertEquals("positive", positive(x))
    assertEquals("negative", positive(y))

  @Test def testNegInline(): Unit =
    assertTrue(negInline(notEmptyPred)(testString))

  @Test def testNegMethod(): Unit =
    assertTrue(neg(notEmptyPred)(testString))

  @Test def testGenericNeg(): Unit =
    def genericIsInt[A](x: A): Boolean = x.isInstanceOf[Int]
    val x = 20
    assertFalse(genericNeg(genericIsInt)(x))

  @Test def testThernaryRelation(): Unit =
    val x = 1
    val y = 2
    val z = 2
    assertTrue(thernaryRelationCurried(x)(y)(z))
    assertTrue(thernaryRelationUncurried(x, y, z))
    assertTrue(thernaryRelationMethodCurried(x)(y)(z))
    assertTrue(thernaryRelationMethodUncurried(x, y, z))

  @Test def testCompose(): Unit =
    val f: Int => Int = _ + 1
    val g: Int => Int = _ - 1
    val x = 5
    assertEquals(5, compose(f, g)(x))

  @Test def testGcd(): Unit =
    val a = 12
    val b = 8
    assertEquals(4, gcd(a, b))

  @Test def testRectanglePerimeter(): Unit =
    val rect = Shape.Rectangle(10, 20)
    assertEquals(60, perimeter(rect))

  @Test def testSquarePerimeter(): Unit =
    val square = Shape.Square(10)
    assertEquals(40, perimeter(square))

  @Test def testCirclePerimeter(): Unit =
    val circle = Shape.Circle(10)
    assertEquals(62.83185307179586, perimeter(circle))

  @Test def testContainsRectangle(): Unit =
    val rect = Shape.Rectangle(10, 20)
    val point = Point2D(5, 5)
    assertTrue(contains(rect, point))

  @Test def testContainsSquare(): Unit =
    val square = Shape.Square(10)
    val point = Point2D(5, 5)
    assertTrue(contains(square, point))

  @Test def testContainsCircle(): Unit =
    val circle = Shape.Circle(10)
    val point = Point2D(5, 5)
    assertTrue(contains(circle, point))

  @Test def testFilter(): Unit =
    val x: Option[Int] = Some(10)
    val value = filter(x)(_ > 5)
    assertTrue(value.isInstanceOf[Some[Int]])
    val none = filter(x)(_ > 15)
    assertTrue(none.isInstanceOf[None[Int]])

  @Test def testMap(): Unit =
    val x: Option[Int] = Some(10)
    val value: Option[Int] = map(x)(_ + 1)
    assertTrue(value.isInstanceOf[Some[Int]])
    val none: Option[Int] = map(None())(_ + 1)
    assertTrue(none.isInstanceOf[None[Int]])

  @Test def testFold(): Unit =
    val x: Option[Int] = Some(10)
    val value: Int = fold(x)(0)(_ + 1)
    assertEquals(11, value)
    val none: Int = fold(None())(8)(_ + 1)
    assertEquals(8, none)

}
