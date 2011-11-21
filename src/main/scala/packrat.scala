

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
object packrat extends JavaTokenParsers with PackratParsers {
  
  // no stackoverflow with packrat parsers
  lazy val S: PackratParser[Any] = S ~ "a" | "a"
  
  // the first alternative works, so the second is never tried
  // we're left with the input " a" which hasn't been parsed
  // --> packrat doesn't help here
  lazy val S2: PackratParser[Any] = "a" | S2 ~ "a"
  
  // this works again
  lazy val S3: PackratParser[Any] = "a" ~ S3 | "a"
  
  def main(args: Array[String]) {
    val result = parseAll(S, "a a")
    println(result)
  }
  
}
