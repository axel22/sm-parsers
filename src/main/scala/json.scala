

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
  
  def obj: Parser[Map[String, Any]] = "{" ~ repsep(member, ",") ~ "}" ^^ {
    case _ ~ xs ~ _ => Map(xs: _*)
  }
  
  def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]" ^^ {
    case _ ~ xs ~ _ => collection.mutable.Seq(xs: _*)
  }
  
  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ {
    case s ~ _ ~ v => (s, v)
  }
  
  // alternative:
  
  def obj2: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ {
    case xs => Map(xs: _*)
  }
  
  def arr2: Parser[Any] = "[" ~> repsep(value, ",") <~ "]" ^^ {
    case xs => collection.mutable.Seq(xs: _*)
  }
  
  def member2: Parser[(String, Any)] = (stringLiteral <~ ":") ~ value ^^ {
    case s ~ v => (s, v)
  }
  
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
    
    val errinput = """{ "name": John """
    println(parseAll(json, errinput))
  }
  
}



