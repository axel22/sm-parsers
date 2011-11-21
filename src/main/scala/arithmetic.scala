

import scala.util.parsing.combinator._



/**
 *  expr -> term { "+" term | "-" term }
 *  term -> factor { "*" factor | "/" factor }
 *  factor -> number | "(" expr ")"
 */
object arithmetic extends JavaTokenParsers {
  
  def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)
  
  def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)
  
  def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
  
  def main(args: Array[String]) {
    println(parseAll(expr, "2 + 3 * 4"))
  }
  
  /* HOMEWORK: Use the transform result operator on parsers (^^) to complete this with expression evaluation. Modify the parser result type appropriately. */
  
}




