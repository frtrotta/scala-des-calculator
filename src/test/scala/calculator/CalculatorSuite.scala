package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("Polynomial") {
    val a = Var(1.0)
    val b = Var(2.0)
    val c = Var(1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 0, "delta must be 0")
    val s = Polynomial.computeSolutions(a, b, c, delta)
    assert(s().size == 1, "only one solution in case of delta == 0")
    assert(s().contains(-1.0), "solution does not match expectation")

    a() = 4.0
    b() = 5.0
    assert(delta() === 9, "delta must be 9")
    assert(s().size === 2, "two distinct solutions expected")
    assert(s().contains(-1.0) && s().contains(-0.25), "solutions do not match expectations")
  }

  test("Cyclic dependencies check 1") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      ("a", Signal(Literal(5))),
      ("b", Signal(Literal(6.0))),
      ("c", Signal(Plus(Ref("a"), Ref("b")))),
      ("d", Signal(Ref("p")))
    )

    val values = Calculator.computeValues(namedExpressions)

    assert(values("a")() === 5.0)
    assert(values("b")() === 6.0)
    assert(values("c")() === 11.0)
    assert(values("d")().isNaN)
  }

  test("Cyclic dependencies check 2") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      ("a", Signal(Ref("a")))
    )

    val values = Calculator.computeValues(namedExpressions)

    assert(values("a")().isNaN)
  }

  test("Cyclic dependencies check 3") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      ("a", Signal(Ref("b"))),
      ("b", Signal(Ref("a")))
    )

    val values = Calculator.computeValues(namedExpressions)

    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
  }



  test("Cyclic dependencies check 4") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      ("a", Signal(Ref("b"))),
      ("b", Signal(Ref("c"))),
      ("c", Signal(Ref("a"))),
      ("d", Signal(Literal(-1.0)))

    )

    val values = Calculator.computeValues(namedExpressions)

    assert(values("a")().isNaN)
    assert(values("b")().isNaN)
    assert(values("c")().isNaN)
    assert(values("d")() === -1.0)
  }
}
