package com.iz2use.express.parser

import fastparse.Utils._

trait Expr extends Base {
  import fastparse.noApi._
  import White._

  val arrayAccess = P("[" ~/ condition.rep(0, ",") ~/ "]")

  val nameSelect = P(name.! ~ arrayAccess.rep(0))

  val applySelect: Parser[Any] = P("(" ~/ condition.rep(0, ",") ~/ ")")

  val arrayDef: Parser[Any] = P("[" ~/ (condition ~ ((":" ~/ condition).rep(1) | ("," ~/ condition).rep(1)).?).? ~/ "]")

  val primitives = P(digit.!.rep(1, ".", 2) | "'" ~/ (!"'" ~ AnyChar).rep(0) ~/ "'" | questionMark)

  val dotSelect = P("." ~ nameSelect)

  val pathSelect = P("\\" ~ nameSelect)

  val selectSelect = P(nameSelect ~ (pathSelect | dotSelect | applySelect | arrayAccess).rep(0))

  val smallestPart: Parser[Any] = P(primitives | selectSelect | arrayDef)

  val evaluable: Parser[Any] = P(smallestPart)

  val parens: Parser[Any] = P("(" ~/ condition ~/ ")")

  val factor: Parser[Any] = P(parens | evaluable)

  val repeat = P(REPEAT ~/ name ~/ ":=" ~/ evaluable ~/ TO ~/ evaluable ~/ EOS ~/ functionBlock ~/ END_REPEAT ~/ EOS)

  val assign = P(selectSelect ~ ":=" ~ condition ~ EOS)

  val productOperation = P(factor ~ (("*" | "/") ~/ factor).rep(0))

  val term = P("-" ~/ productOperation | productOperation)

  val sumOperation = P(term ~ (("+" | "-") ~/ term).rep(0))

  val comparatorOperation = P(sumOperation ~ ((":" ~ ("=:" | "<>:") | "<>" | "<" ~ ("=" | "*").? | "=" | ">" ~ "=".?) ~/ sumOperation).rep(0))

  val unaryOperation = P((NOT ~/ comparatorOperation | comparatorOperation))

  val binaryLogicalOperation = P(unaryOperation ~ ((OR | AND | IN | "|" ~ "|".?) ~/ unaryOperation).rep(0))

  //val isolatedOperation = P( unaryLogicalOperation))

  val condition = P(binaryLogicalOperation)

  val ifBlock = P(IF ~/ condition ~/ THEN ~/ functionBlock ~/ (ELSE ~/ functionBlock).? ~ END_IF ~ EOS)

  val block = P(BEGIN ~/ functionBlock ~/ END ~ EOS)

  val returnStmt = P(RETURN ~/ condition ~ EOS)

  val escapeStmt = P(ESCAPE ~/ EOS)

  val caseLabel = P(!OTHERWISE ~ (name.!.rep(1, ".") | primitives) ~/ ":")

  val caseBlock = P(CASE ~/ name.! ~/ OF ~/ (caseLabel ~ lineBlock).rep(1) ~/ (OTHERWISE ~/ ":" ~/ lineBlock).? ~/ END_CASE ~/ EOS)

  val lineBlock: Parser[Any] = P((block | caseBlock | escapeStmt | ifBlock | repeat | returnStmt | assign))

  val functionBlock: Parser[Any] = P(lineBlock.rep(0))
}