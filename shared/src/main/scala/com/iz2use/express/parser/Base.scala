package com.iz2use.express.parser

import com.iz2use.express.tree
import fastparse.WhitespaceApi

trait Base extends Symbols {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    //NoTrace((" " | "\t" | "\r\n" | "\r" | "\n").rep(0))
    (" " | "\t" | "\r\n" | "\r" | "\n").rep(0)
  }

  import fastparse.noApi._
  import White._

  def group[A](parser: Parser[A]) = P(NBRO ~/ parser ~ NBRC)

  val nameList = group(name.!.rep(1, COMA))

  //val expr = P((!EOS ~ AnyChar).rep(1))

  val ABS = P(IgnoreCase("ABS"))
  val ABSTRACT = P(IgnoreCase("ABSTRACT"))
  val AND = P(IgnoreCase("AND"))
  val AREA = P(IgnoreCase("AREA"))
  val ARRAY = P(IgnoreCase("ARRAY"))

  val BAG = P(IgnoreCase("BAG"))
  val BEGIN = P(IgnoreCase("BEGIN"))
  val BINARY = P(IgnoreCase("BINARY"))
  val BLENGTH = P(IgnoreCase("BLENGTH"))
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

  val FALSE = P(IgnoreCase("FALSE"))
  val FIXED = P(IgnoreCase("FIXED"))
  val FOR = P(IgnoreCase("FOR"))
  val FUNCTION = P(IgnoreCase("FUNCTION"))

  val GENERIC = P(IgnoreCase("GENERIC"))

  val HIINDEX = P(IgnoreCase("HIINDEX"))

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
  val NVL = P(IgnoreCase("NVL"))

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
  val SQRT = P(IgnoreCase("SQRT"))
  val STRING = P(IgnoreCase("STRING"))
  val SUBTYPE = P(IgnoreCase("SUBTYPE"))
  val SUPERTYPE = P(IgnoreCase("SUPERTYPE"))

  val THEN = P(IgnoreCase("THEN"))
  val TO = P(IgnoreCase("TO"))
  val TRUE = P(IgnoreCase("TRUE"))
  val TYPE = P(IgnoreCase("TYPE"))
  val TYPEOF = P(IgnoreCase("TYPEOF"))

  val USEDIN = P(IgnoreCase("USEDIN"))
  val UNIQUE = P(IgnoreCase("UNIQUE"))

  val WHERE = P(IgnoreCase("WHERE"))

  val XOR = P(IgnoreCase("XOR"))

  val comment = P(commentStart ~/ (!commentEnd ~ AnyChar).rep(0).! ~/ commentEnd).map(tree.Comment)

  val header = P(comment.?)

  val emptyBlock = tree.EmptyTree

  val unit = P(QMARK).map({
    case c => emptyBlock
  })

  val _true = P(TRUE ~ nextNotName).map(_ => tree.Literal(tree.Constant(true)))
  val _false = P(FALSE ~ nextNotName).map(_ => tree.Literal(tree.Constant(false)))
  val boolean = P(_true | _false)

  val primitives = P(boolean | number | text | unit)
}