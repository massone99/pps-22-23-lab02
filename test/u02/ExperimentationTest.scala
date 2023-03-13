package u02

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

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
    assertEquals(5, compose(f,g)(x))

}
