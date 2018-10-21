import org.specs2._
import Parser._
import scalaz.Monad
import scalaz.Monad._
import scalaz.MonadPlus._
import scalaz.syntax.monad._

class ParserTest extends mutable.Specification {
  "Primitives" should {
    "item parser" >>  {
      any.run("abc".toList) must contain(('a', List('b', 'c')))
    }
    "conditional parser" >> {
      sat(_ == 'c').run("cab".toList) must contain(('c', List('a', 'b')))
      sat(_ == 'c').run("acb".toList) must beEmpty
    }
    "char parser" >> {
      char('c').run("cab".toList) must contain(('c', List('a', 'b')))
      char('c').run("acb".toList) must beEmpty
    }
    "string parser" >> {
      val testCase = "cab".toList
      string("ca".toList).run(testCase) must contain((testCase.take(2), List('b')))
      string("ab".toList).run(testCase) must beEmpty
    }
    "many parser" >> {
      val testCase = "ccb".toList
      many(char('c')).run(testCase) must contain((testCase.take(2), List('b')))
      many(char('b')).run(testCase) must contain((List.empty[Char], testCase))
    }
    "many1 parser" >> {
      val testCase = "ccb".toList
      many1(char('c')).run(testCase) must contain((testCase.take(2), List('b')))
      many1(char('b')).run(testCase) must beEmpty
    }

    "sepBy parser" >> {
      val testCase = "c,cb".toList
      sepBy(char('c'), char(',')).run(testCase) must contain(("cc".toList, List('b')))
      sepBy(char('c'), char('-')).run(testCase) must contain(("c".toList, testCase.drop(1)))
      sepBy(char('b'), any).run(testCase) must contain((List.empty[Char], testCase))
    }

    "sepBy1 parser" >> {
      val testCase = "c,cb".toList
      sepBy1(char('c'), char(',')).run(testCase) must contain(("cc".toList, List('b')))
      sepBy1(char('c'), char('-')).run(testCase) must contain(("c".toList, testCase.drop(1)))
      sepBy1(char('b'), any).run(testCase) must beEmpty
    }

    "chainl parser" >> {
      val testCase = "11b".toList
      val digitParser: Parser[Int] =  sat(_.isDigit).map((v: Char) => v.toInt - '0'.toInt)
      val sumParser: Parser[(Int, Int) => Int] = Monad[Parser].pure[(Int, Int) => Int]((a: Int, b: Int) => a + b)

      chainl(digitParser, sumParser, 0).run(testCase) must contain((2, List('b')))
      chainl(digitParser, sumParser, 0).run(testCase.drop(1)) must contain((1, List('b')))
    }

    "chainl1 parser" >> {
      val testCase = "11b".toList
      val digitParser: Parser[Int] =  sat(_.isDigit).map((v: Char) => v.toInt - '0'.toInt)
      val sumParser: Parser[(Int, Int) => Int] = Monad[Parser].pure[(Int, Int) => Int]((a: Int, b: Int) => a + b)

      chainl1(digitParser, sumParser).run(testCase) must contain((2, List('b')))
      chainl1(digitParser, sumParser).run(testCase.drop(2)) must beEmpty
    }

    "space parser" >> {
      space.run("  a".toList) must contain(("  ".toList, List('a')))
      space.run("a".toList) must contain((List.empty[Char], List('a')))
    }

    "token parser" >> {
      val testCase = "a  b"
      token(char('a')).run(testCase.toList) must contain(('a', List('b')))
    }

    "symbol parser" >> {
      val testCase = "ac  b"
      symbol("ac".toList).run(testCase.toList) must contain(("ac".toList, List('b')))
    }
  }
}
