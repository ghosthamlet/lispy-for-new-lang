

package lispy

import scala.collection.GenTraversableOnce


// https://github.com/Mononofu/Scalisp
// http://matt.might.net/articles/denotational-interpreter-for-lisp-and-scheme-like-lambda-calculus-based-language-lambdo/

def Sym () = new String

def parse (s: String) = read_from_tokens(tokenize(s))

def tokenize(s: String) = {
  s 
  .replace("(", " ( ") 
  .replace(")", " ) ") 
  .split(" ") 
  .filter(_.length > 0)
  .toList
}

def read_from_tokens(tokens: List[String]) = {
  var ntokens = tokens

  // def f (): GenTraversableOnce[Any] = {
  def f (): Any = {
    if (ntokens.length == 0) {
      throw new Exception("unexpected EOF while reading")
    }
    val token = ntokens{0}
    ntokens = ntokens.drop(1)
    token match {
      case "(" => 
        var L = List[Any]()
        while (ntokens.head != ")") {
          L :+= f()
        }
        ntokens = ntokens.drop(1)
        L
      case ")" => throw new Exception("unexpected )")
      case _ => atom(token)
    }
  }

  // f().toList
  f()
  // use in where use res = read_from_tokens
  // if (res.isInstanceOf[List[Any]])
    // res.asInstanceOf[List[Any]]
  // else
    // res
}

def atom(token: String) = {
  try {
    Integer.parseInt(token)
  } catch {
    case _: NumberFormatException =>
      try {
        token.toDouble
      } catch {
        case _: NumberFormatException => token + ""
      }
  }
}

def standard_env() = {
  val env = new Env()
  val math_f = classOf[Math].getMethods
  val math_k = math_f.map(_.getName)
  // math_f{0}.invoke(null, 3.asInstanceOf[Int], 10.asInstanceOf[Int])
  // math_f{0}.invoke(null, 3.asInstanceOf[AnyRef], 10.asInstanceOf[AnyRef])
  env.update((math_k zip math_f).toMap)
  env.update(Map(
    "+" -> { (x: Int, y: Int) => x + y },
    "-" -> { (x: Int, y: Int) => x - y },
    "*" -> { (x: Int, y: Int) => x * y },
    "/" -> { (x: Int, y: Int) => x / y },
    ">" -> { (x: Int, y: Int) => x > y },
    "<" -> { (x: Int, y: Int) => x < y },
    ">=" -> { (x: Int, y: Int) => x >= y },
    "<=" -> { (x: Int, y: Int) => x <= y },
    "=" -> { (x: Int, y: Int) => x == y },
    "append" -> { (x: List[Any], y: Any) => x :+ y },
    // "apply" ->  { (f: Any => Any, x: List[Any]) => x.apply(f)},
    // "begin" ->  lambda *x: x[-1],
    "car" ->    { x: List[Any] => x.head},
    "cdr" ->    { x: List[Any] => x.drop(1)},
    "cons" ->   { (x: Any, y: List[Any]) => x +: y},
    "eq?" ->    { (x: Int, y: Int) => x == y },
    "equal?" -> { (x: Int, y: Int) => x == y },
    "length" -> { x: List[Any] => x.length},
    // "list" ->   List,
    "list?" ->  { x: Any => x.isInstanceOf[List[Any]]},
    // map(_.asInstanceOf[Int] + 2, List(1, 2, 3))
    "map" ->    { (f: Any => Any, x: List[Any]) => x.map(f) },
    "not" ->    { (x: Any) => x == false },
    "null?" ->  { (x: Any) => x == Nil },
    "number?" -> { x: Any => x.isInstanceOf[Number]},
    "procedure?" -> { x: Any => x },
    "symbol?" -> { x: Any => x.isInstanceOf[Symbol]}
    // "time" ->  function(x) return x[1] end
  ))
}


class Env (
  params: List[Any] = List(), 
  args: List[Any] = List(), 
  outer: Env = Nil
) {

  private var map = Map[String, Any]()
  map ++= (params zip args).toMap

  private val outer = outer

  def update(m: Map[String, Any]) = {
    map ++= m
  }

  def find(va: String) = {
    if (map.contains(va)) {
      map
    } else if (outer != Nil) {
      outer.find(va)
    }
  }
}

val global_env = standard_env()

def repl(prompt: String = "lis.py> ") = {

}

def lispstr(exp: List[String]) = {

}

class Procedure() {

}

def eval(x: List[String], env=global_env) {

}

object Lispy extends App {
  repl()
}

