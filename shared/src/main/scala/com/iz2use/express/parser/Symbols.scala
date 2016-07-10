package com.iz2use.express.parser

import fastparse.Utils._

trait Symbols {
  import fastparse.all._

  val EOS = P(";")

  val commentStart = P("(*")

  val commentEnd = P("*)")

  val comment = P(commentStart ~/ (!commentEnd ~ AnyChar).rep(0).! ~/ commentEnd)

  val header = P(comment.?)

  val ABSTRACT = P(IgnoreCase("ABSTRACT"))
  val AND = P(IgnoreCase("AND"))
  val ARRAY = P(IgnoreCase("ARRAY"))

  val BAG = P(IgnoreCase("BAG"))
  val BEGIN = P(IgnoreCase("BEGIN"))
  val BINARY = P(IgnoreCase("BINARY"))

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

  val FIXED = P(IgnoreCase("FIXED"))
  val FOR = P(IgnoreCase("FOR"))
  val FUNCTION = P(IgnoreCase("FUNCTION"))

  val GENERIC = P(IgnoreCase("GENERIC"))

  val IF = P(IgnoreCase("IF"))
  val IN = P(IgnoreCase("IN"))
  val INVERSE = P(IgnoreCase("INVERSE"))

  val LIST = P(IgnoreCase("LIST"))
  val LOCAL = P(IgnoreCase("LOCAL"))

  val NOT = P(IgnoreCase("NOT"))

  val OF = P(IgnoreCase("OF"))
  val ONEOF = P(IgnoreCase("ONEOF"))
  val OPTIONAL = P(IgnoreCase("OPTIONAL"))
  val OR = P(IgnoreCase("OR"))
  val OTHERWISE = P(IgnoreCase("OTHERWISE"))

  val REPEAT = P(IgnoreCase("REPEAT"))
  val RETURN = P(IgnoreCase("RETURN"))
  val RULE = P(IgnoreCase("RULE"))

  val SCHEMA = P(IgnoreCase("SCHEMA"))
  val SELECT = P(IgnoreCase("SELECT"))
  val SELF = P(IgnoreCase("SELF"))
  val SET = P(IgnoreCase("SET"))
  val STRING = P(IgnoreCase("STRING"))
  val SUBTYPE = P(IgnoreCase("SUBTYPE"))
  val SUPERTYPE = P(IgnoreCase("SUPERTYPE"))

  val THEN = P(IgnoreCase("THEN"))
  val TO = P(IgnoreCase("TO"))
  val TYPE = P(IgnoreCase("TYPE"))

  val UNIQUE = P(IgnoreCase("UNIQUE"))

  val WHERE = P(IgnoreCase("WHERE"))

  val digit = P(CharPred(_.isDigit).rep(1))
  val questionMark = P("?")
  val startName = P(CharPred(a => a.isLetter || a == '_'))
  val continueName = P(startName | digit)
  val name = P(startName ~ (continueName).rep(0))
}
