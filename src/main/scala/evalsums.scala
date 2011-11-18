

import scala.util.parsing.combinator._



/**
 * S -> T '+' S
 * S -> T
 * T -> 'x'
 */
object evalsums extends JavaTokenParsers {
  
  def S: Parser[Double] = (T ~ "+" ~ S) ^^ {
    case a ~ _ ~ b => a + b
  } | T
  def T: Parser[Double] = floatingPointNumber ^^ {
    s => s.toDouble
  }
  
  def main(args: Array[String]) {
    val input = "1 + 2 + 3"
    val result = parseAll(S, input)
    println(result)
  }
  
}
