package com.iz2use.express.parser

import com.iz2use.express.tree
import fastparse.Utils._

trait FieldType extends Expr {
  import fastparse.noApi._
  import White._

  //val dimension: Parser[tree.Tree] = P(digit | questionMark | name)

  val dimensions = P("[" ~/ condition.rep(2, ":") ~/ "]")

  val listOrArrayOrSet: Parser[tree.FieldType] = P(((LIST.!.map(_ => tree.ListType.tupled)) | (ARRAY.!.map(_ => tree.ArrayType.tupled)) | (SET.!.map(_ => tree.SetType.tupled))) ~/ dimensions.? ~/ OF ~/ UNIQUE.!.? ~/ fieldType).map({
    case (colTpe, dims, unique, tpe) => colTpe.apply(tpe, dims, unique.isDefined)
  })

  val enumeration: Parser[tree.EnumerationType] = P(ENUMERATION ~/ OF ~/ nameList).map({
    case list => tree.EnumerationType(list)
  })

  val select: Parser[tree.SelectType] = P(SELECT ~/ group(fieldType.rep(1, COMA))).map({
    case list => tree.SelectType(list)
  })

  val stringOrBinary: Parser[tree.PrimitiveType] = P((STRING.!.map(_ => tree.StringType.tupled) | BINARY.!.map(_ => tree.BinaryType.tupled)) ~/ (group(digit.rep(1).!) ~/ FIXED.!.?).?).map({
    case (tpe, None) => tpe.apply(None, false)
    case (tpe, Some((len, fixed))) => tpe.apply(Some(len.toInt), fixed.isDefined)
  })

  val generic: Parser[tree.GenericType] = P(GENERIC ~/ ":" ~/ userDefinedType).map({
    case defined => tree.GenericType(defined)
  })

  val userDefinedType: Parser[tree.UserDefinedType] = P(name.!).map({
    case name =>
      if (name equals name.toUpperCase()) println(name)
      tree.UserDefinedType(name)
  })

  val primitiveTypes: Parser[tree.PrimitiveType] = P(
    stringOrBinary |
      LONG.!.map(_ => tree.LongType) |
      REAL.!.map(_ => tree.RealType) |
      INTEGER.!.map(_ => tree.IntegerType) |
      LOGICAL.!.map(_ => tree.LogicalType) |
      BOOLEAN.!.map(_ => tree.BooleanType) |
      NUMBER.!.map(_ => tree.NumberType))

  val fieldType: Parser[tree.FieldType] = P(listOrArrayOrSet | enumeration | select | generic | primitiveTypes | userDefinedType)
}