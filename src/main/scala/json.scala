

import scala.util.parsing.combinator._



/** Example:
 *  {
 *    "name": "John",
 *    "year": 1970,
 *    "assets": ["house", "car", "bike"],
 *    "wife": {
 *      "name": "Jane",
 *      "year": 1972
 *    }
 *  }
 *  
 *  Grammar:
 *  value -> obj | arr | stringLiteral | numberLiteral | "null" | "true" | "false"
 *  obj -> "{" [members] "}"
 *  members -> member { "," member }
 *  member -> stringLiteral ":" value
 *  arr -> "[" [values] "]"
 *  values -> value { "," value }
 */
object json extends JavaTokenParsers {
  
  def json: Parser[Any] = value
  
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
  
  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
  
  def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
  
  def member: Parser[Any] = stringLiteral ~ ":" ~ value
  
  def main(args: Array[String]) {
    val input = """
    {
      "name": "John",
      "year": 1970,
      "assets": ["house", "car", "bike"],
      "wife": {
        "name": "Jane",
        "year": 1972
      }
    }
    """
    val result = parseAll(json, input)
    println(result)
  }
  
}
