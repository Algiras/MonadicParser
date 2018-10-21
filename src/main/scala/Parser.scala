import scalaz.{Monad, MonadPlus}
import scalaz.syntax.monadPlus._
import Parser._

case class Parser[A](run: List[Char] => List[(A, List[Char])]){
  def +++(b: Parser[A]): Parser[A] = Parser[A](cs => run(cs) ++ b.run(cs) match {
    case Nil => Nil
    case x :: _ => List(x)
  })
}

object Parser {
  implicit val parserMonad = new MonadPlus[Parser] {
    override def empty[A]: Parser[A] = Parser[A](_ => Nil)

    override def plus[A](a: Parser[A], b: => Parser[A]): Parser[A] = Parser(cs => a.run(cs) ++ b.run(cs))

    override def point[B](a: => B): Parser[B] = Parser[B](cs => List((a, cs)))

    override def bind[B, C](fa: Parser[B])(f: B => Parser[C]): Parser[C] = Parser[C](cs => {
        fa.run(cs).flatMap { case (a, ls) => f(a).run(ls) }
    })
  }

  val any: Parser[Char] = Parser[Char] {
    case Nil => Nil
    case x :: xs => List((x, xs))
  }

  def sat(fn: Char => Boolean): Parser[Char] = for {
    c <- any
    if fn(c)
  } yield c


  def char(char: Char): Parser[Char] = sat(char == _)

  def string(str: List[Char]): Parser[List[Char]] = str match {
    case Nil => Monad[Parser].pure[List[Char]]("".toList)
    case x :: xs => for {
      c <- char(x)
      cs <- string(xs)
    } yield c :: cs
  }

  def many[A](parser: Parser[A]): Parser[List[A]] = {
    many1(parser) +++ Monad[Parser].pure[List[A]](Nil)
  }

  def many1[A](parser: Parser[A]): Parser[List[A]] = for {
    a <- parser
    as <- many(parser)
  } yield a :: as

  def sepBy[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = {
    sepBy1(parser, separator) +++ Monad[Parser].pure[List[A]](Nil)
  }

  def sepBy1[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = for {
    a <- parser
    as <- many(for{_ <- separator; p <- parser} yield p)
  } yield a :: as

  def chainl[A](parser: Parser[A], opParser: Parser[(A, A) => A], value: A): Parser[A] = {
    chainl1(parser, opParser) +++ Monad[Parser].pure[A](value)
  }

  def chainl1[A](parser: Parser[A], opParser: Parser[(A, A) => A]): Parser[A] = {
    def rest(v: A): Parser[A] = (for {
      f <- opParser
      b <- parser
      r <- rest(f(v, b))
    } yield r) +++ Monad[Parser].pure[A](v)

    for {
      a <- parser
      b <- rest(a)
    } yield b
  }

  val space: Parser[List[Char]] = many(sat(_ == ' '))

  def token[A](parser: Parser[A]): Parser[A] = for {
    a <- parser
    _ <- space
  } yield a

  def symbol(cs: List[Char]): Parser[List[Char]] = token (string(cs))

  def parse[A](parser: Parser[A], str: List[Char]): List[(A, List[Char])] = (for {
    _ <- space
    p <- parser
  } yield p).run(str)
}

object Math {
  lazy val expr: Parser[Int] = chainl1(term, addop)
  lazy val term: Parser[Int] = chainl1(factor, mullop)

  lazy val factor: Parser[Int] = digit +++ (for {
    _ <- symbol(List('('))
    n <- expr
    _ <- symbol(List(')'))
  } yield n)

  val digit: Parser[Int] = for {
    x <- token(sat(_.isDigit))
  } yield x.toInt - '0'.toInt

  val addop: Parser[(Int, Int) => Int] = {
    val left: Parser[(Int, Int) => Int] = for {
      _ <- symbol(List('+'))
      p <- Monad[Parser].pure((a: Int, b: Int) => a + b)
    } yield p

    val right: Parser[(Int, Int) => Int] = for {
      _ <- symbol(List('-'))
      p <- Monad[Parser].pure((a: Int, b: Int) => a - b)
    } yield p

    left +++ right
  }

  val mullop: Parser[(Int, Int) => Int] = {
    val left: Parser[(Int, Int) => Int] = for {
      _ <- symbol(List('*'))
      p <- Monad[Parser].pure((a: Int, b: Int) => a * b)
    } yield p

    val right: Parser[(Int, Int) => Int] = for {
      _ <- symbol(List('/'))
      p <- Monad[Parser].pure((a: Int, b: Int) => a / b)
    } yield p

    left +++ right
  }
}

object App {
  def main(args: Array[String]): Unit = {
    println(parse(Math.expr,  " 9 - 2 * 3 + 4 ".toList ))
  }
}