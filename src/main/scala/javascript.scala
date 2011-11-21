

import scala.util.parsing.combinator._
import collection._



/**
 * program -> { statement }
 * statement -> declaration | invocation
 * declaration -> "function" ~ identifier ~ "(" ~ { ident } ~ ")" ~ "{" ~ expression ~ "}"
 * expression -> invocation | identifier | number
 * invocation -> identifier "(" { expression } ")"
 */
object javascript extends JavaTokenParsers {
  
  val functions = mutable.Map[String, (List[String], Map[String, Any] => Any)](
    "inc" -> (List("x"), bindings => bindings("x").asInstanceOf[Double] + 1)
  );
  
  sealed trait AST
  case class Number(d: Double) extends AST
  case class Ident(name: String) extends AST
  case class Invocation(name: String, args: List[AST]) extends AST
  
  def program: Parser[Any] = rep(statement) ^^ {
    case xs => xs.last
  }
  
  def statement: Parser[Any] = invocation | declaration
  
  def invocation: Parser[Any] = (ident <~ "(") ~ repsep(expression, ",") <~ ")" ^^ {
    case funcname ~ params => invoke(funcname, params)
  }
  
  def invoke(funcname: String, args: List[Any]) = {
    val (argnames, ast) = functions(funcname)
    ast(argnames.zip(args).toMap)
  }
  
  def declaration: Parser[Any] = ("function" ~> ident <~ "(") ~ (repsep(ident, ",") <~ ")") ~ ("{" ~> expressionAST <~ "}") ^^ {
    case funcname ~ paramnames ~ expressionAST =>
      functions.put(funcname, (paramnames, bindings => eval(bindings, expressionAST)))
  }
  
  def expressionAST: Parser[AST] = (
    floatingPointNumber ^^ { x => Number(x.toDouble) }
    | invocationAST
    | ident ^^ { Ident(_) }
  );
  
  def invocationAST: Parser[AST] = (ident <~ "(") ~ repsep(expressionAST, ",") <~ ")" ^^ {
    case funcname ~ paramsAST => Invocation(funcname, paramsAST)
  }
  
  def expression: Parser[Any] = (
    floatingPointNumber ^^ { _.toDouble }
    | invocation
  );
  
  def eval(context: Map[String, Any], ast: AST): Any = ast match {
    case Number(n) => n
    case Ident(s) => context(s)
    case Invocation(fname, params) =>
      val evaluatedparams = params.map(x => eval(context, x))
      invoke(fname, evaluatedparams)
  }
  
  def main(args: Array[String]) {
    val input = """
      function inc2(x) {
        inc(inc(x))
      }
      inc2(2)
    """
    println(parseAll(program, input))
  }
  
  /* HOMEWORK: Modify the code above so that functions become first-class values - i.e. they can be passed around as arguments to other functions. */
  
  /* HOMEWORK: Implement nested function declarations. */
  
}




