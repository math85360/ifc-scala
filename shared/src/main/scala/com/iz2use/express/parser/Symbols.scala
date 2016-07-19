package com.iz2use.express.parser

//import fastparse.Utils._
import com.iz2use.express.tree

trait Symbols {
  import fastparse.all._
  
  val COMA = P(",")
  val NBRO = P("(")
  val NBRC = P(")")
  val SBRO = P("[")
  val SBRC = P("]")
  val CBRO = P("{")
  val CBRC = P("}")
  val STAR = P("*")
  val SQU = P("'")
  val DOT = P(".")
  val EOS = P(";")
  val MINUS = P("-")
  val PLUS = P("+")
  val SLASH = P("/")
  val BACKSLASH = P("\\")
  val PIPE = P("|")
  val COLON = P(":")
  val LT = P("<")
  val GT = P(">")
  val EQ = P("=")
  val QMARK = P("?")

  val COLON_EQ = P(":=")
  val COLON_EQ_COLON = P(":=:")
  val COLON_LT_GT_COLON = P(":<>:")
  val EQ_EQ = P("==")
  val GT_EQ = P(">=")
  val LT_EQ = P("<=")
  val LT_GT = P("<>")
  val LT_STAR = P("<*")

  val PIPE_PIPE = P("||")

  val commentStart = P("(*")

  val commentEnd = P("*)")
  
  val digit = P(CharPred(_.isDigit).rep(1))
  
  val startName = P(CharPred(a => a.isLetter || a == '_'))
  val continueName = P(startName | digit)
  val name = P(startName ~ (continueName).rep(0))
  
  val number = P(((digit.rep(1) ~ (DOT ~/ digit.rep(0)).?) ~ (("e" | "E") ~ (MINUS | PLUS) ~ digit.rep(1)).? | (DOT ~ digit.rep(1))).!).map({
    case c if c.contains('.') => tree.Literal(tree.Constant(c.toDouble))
    case c => tree.Literal(tree.Constant(c.toLong))
  })
  
  val text = P(SQU ~/ (!SQU ~ AnyChar).rep(0).! ~/ SQU).map({
    case c => tree.Literal(tree.Constant(c))
  })

}
