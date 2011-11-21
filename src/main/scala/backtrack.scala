

import scala.util.parsing.combinator._



/**
 * Language: (a )*a
 * Examples: "a", "a a", "a a a a a a", etc.
 * Grammar:
 *   S -> S 'a' | 'a'
 * The same grammar, written differently:
 *   S -> 'a' | S 'a'
 * A third grammar:
 *   S -> 'a' S | 'a'
 */
object backtrack extends JavaTokenParsers {
  
  // stackoverflow!!!
  def S: Parser[Any] = S ~ "a" | "a"
  
  // the first alternative works, so the second is never tried
  // we're left with the input " a" which hasn't been parsed
  def S2: Parser[Any] = "a" | S2 ~ "a"
  
  // this works
  def S3: Parser[Any] = "a" ~ S3 | "a"
  
  def main(args: Array[String]) {
    val result = parseAll(S3, "a a")
    println(result)
  }
  
}
