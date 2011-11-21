

import scala.util.parsing.combinator._



/**
 * Language: a b d | a b c d
 * Examples: "a b d", "a b c d"
 * Grammar:
 *   S -> 'a' 'b' 'd' | 'a' 'b' 'c' 'd'
 */
object errors extends JavaTokenParsers {
  
  def S: Parser[Any] = "a" ~ "b" ~ "d" | "a" ~ "b" ~ "c" ~ "d"
  
  def main(args: Array[String]) {
    // which error message will be emitted?
    val result = parseAll(S, "a b c f")
    println(result)
    
    // what about here?
    val result2 = parseAll(S, "a b f")
    println(result2)
  }
  
}
