

import scala.util.parsing.combinator._



/**
 * S -> T '+' S
 * S -> T
 * T -> 'x'
 */
object sums extends JavaTokenParsers {
  
  def S: Parser[Any] = (T ~ "+" ~ S) | T
  def T: Parser[Any] = "x" | "y" | "z"
  
  def main(args: Array[String]) {
    val input = "x + y + z"
    val result = parseAll(S, input)
    println(result)
  }
  
  /* HOMEWORK: If you want to understand the internals of parser combinators, then search for Daniel Spiewak's blog entry on parser combinators. Follow simple instructions there to implement your own small parser combinator library. */
  
}
