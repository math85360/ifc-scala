package com.iz2use.express.parser

import fastparse.Utils._
import com.iz2use.express.tree

trait Symbols {
  import fastparse.all._

  val EOS = P(";")

  val commentStart = P("(*")

  val commentEnd = P("*)")

  val comment = P(commentStart ~/ (!commentEnd ~ AnyChar).rep(0).! ~/ commentEnd).map(tree.Comment)

  val header = P(comment.?)

  val ABSTRACT = P(IgnoreCase("ABSTRACT"))
  val AND = P(IgnoreCase("AND"))
  val ARRAY = P(IgnoreCase("ARRAY"))

  val BAG = P(IgnoreCase("BAG"))
  val BEGIN = P(IgnoreCase("BEGIN"))
  val BINARY = P(IgnoreCase("BINARY"))
  val BOOLEAN = P(IgnoreCase("BOOLEAN"))

  val CASE = P(IgnoreCase("CASE"))

  val DERIVE = P(IgnoreCase("DERIVE"))

  val ELSE = P(IgnoreCase("ELSE"))
  val END = P(IgnoreCase("END"))
  val END_CASE = P(IgnoreCase("END_CASE"))
  val END_ENTITY = P(IgnoreCase("END_ENTITY"))
  val END_FUNCTION = P(IgnoreCase("END_FUNCTION"))
  val END_IF = P(IgnoreCase("END_IF"))
  val END_LOCAL = P(IgnoreCase("END_LOCAL"))
  val END_REPEAT = P(IgnoreCase("END_REPEAT"))
  val END_RULE = P(IgnoreCase("END_RULE"))
  val END_SCHEMA = P(IgnoreCase("END_SCHEMA"))
  val END_TYPE = P(IgnoreCase("END_TYPE"))
  val ENTITY = P(IgnoreCase("ENTITY"))
  val ENUMERATION = P(IgnoreCase("ENUMERATION"))
  val ESCAPE = P(IgnoreCase("ESCAPE"))
  val EXISTS = P(IgnoreCase("EXISTS"))

  val FIXED = P(IgnoreCase("FIXED"))
  val FOR = P(IgnoreCase("FOR"))
  val FUNCTION = P(IgnoreCase("FUNCTION"))

  val GENERIC = P(IgnoreCase("GENERIC"))

  val IF = P(IgnoreCase("IF"))
  val IN = P(IgnoreCase("IN"))
  val INTEGER = P(IgnoreCase("INTEGER"))
  val INVERSE = P(IgnoreCase("INVERSE"))

  val LIST = P(IgnoreCase("LIST"))
  val LOCAL = P(IgnoreCase("LOCAL"))
  val LOGICAL = P(IgnoreCase("LOGICAL"))
  val LONG = P(IgnoreCase("LONG"))

  val MOD = P(IgnoreCase("MOD"))

  val NOT = P(IgnoreCase("NOT"))
  val NUMBER = P(IgnoreCase("NUMBER"))

  val OF = P(IgnoreCase("OF"))
  val ONEOF = P(IgnoreCase("ONEOF"))
  val OPTIONAL = P(IgnoreCase("OPTIONAL"))
  val OR = P(IgnoreCase("OR"))
  val OTHERWISE = P(IgnoreCase("OTHERWISE"))

  val QUERY = P(IgnoreCase("QUERY"))

  val REAL = P(IgnoreCase("REAL"))
  val REPEAT = P(IgnoreCase("REPEAT"))
  val RETURN = P(IgnoreCase("RETURN"))
  val RULE = P(IgnoreCase("RULE"))

  val SCHEMA = P(IgnoreCase("SCHEMA"))
  val SELECT = P(IgnoreCase("SELECT"))
  val SELF = P(IgnoreCase("SELF"))
  val SET = P(IgnoreCase("SET"))
  val SIZEOF = P(IgnoreCase("SIZEOF"))
  val STRING = P(IgnoreCase("STRING"))
  val SUBTYPE = P(IgnoreCase("SUBTYPE"))
  val SUPERTYPE = P(IgnoreCase("SUPERTYPE"))

  val THEN = P(IgnoreCase("THEN"))
  val TO = P(IgnoreCase("TO"))
  val TYPE = P(IgnoreCase("TYPE"))
  val TYPEOF = P(IgnoreCase("TYPEOF"))

  val UNIQUE = P(IgnoreCase("UNIQUE"))

  val WHERE = P(IgnoreCase("WHERE"))

  val XOR = P(IgnoreCase("XOR"))

  val digit = P(CharPred(_.isDigit).rep(1))
  val questionMark = P("?")
  val startName = P(CharPred(a => a.isLetter || a == '_'))
  val continueName = P(startName | digit)
  val name = P(startName ~ (continueName).rep(0))

  val number = P(((digit.rep(1) ~ ("." ~/ digit.rep(0)).?) | ("." ~ digit.rep(1))).!).map({
    case c if c.contains('.') => tree.Literal(tree.Constant(c.toDouble))
    case c => tree.Literal(tree.Constant(c.toLong))
  })

  val text = P("'" ~/ (!"'" ~ AnyChar).rep(0).! ~/ "'").map({
    case c => tree.Literal(tree.Constant(c))
  })

  val emptyBlock = tree.Literal(tree.Constant("()"))

  val unit = P(questionMark).map({
    case c => emptyBlock
  })

  val primitives = P(number | text | unit)
}
