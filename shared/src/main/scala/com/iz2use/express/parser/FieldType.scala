package com.iz2use.express.parser

import fastparse.Utils._

trait FieldType extends Base {
  import fastparse.noApi._
  import White._

  val dimension = P("[" ~/ (digit | questionMark | name).rep(2, ":") ~/ "]")

  val list: Parser[Any] = P(LIST ~/ dimension.!.? ~/ OF ~/ UNIQUE.? ~/ fieldType)

  val array: Parser[Any] = P(ARRAY ~/ dimension.!.? ~/ OF ~/ UNIQUE.? ~/ fieldType)

  val set: Parser[Any] = P(SET ~/ dimension.!.? ~/ OF ~/ UNIQUE.? ~/ fieldType)

  val enumeration: Parser[Any] = P(ENUMERATION ~/ OF ~/ nameList)

  val select: Parser[Any] = P(SELECT ~/ nameList)

  val string: Parser[Any] = P(STRING ~/ (group(digit.rep(1).!) ~/ FIXED.?).?)

  val binary: Parser[Any] = P(BINARY ~/ (group(digit.rep(1).!) ~/ FIXED.?).?)

  val generic: Parser[Any] = P(GENERIC ~/ ":" ~/ name.!)

  //val list = P(LIST ~/ "[" ~/ "]" ~/ OF ~/ name)

  val fieldType = P(list | array | set | enumeration | select | string | binary | generic | name.!)
}