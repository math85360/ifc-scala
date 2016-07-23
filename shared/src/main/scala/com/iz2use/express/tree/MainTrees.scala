package com.iz2use.express.tree

case class Field(name: String, tpe: FieldType) extends Named

abstract sealed class FieldType

abstract sealed class PrimitiveType extends FieldType

abstract sealed trait NumericBasedType extends PrimitiveType

case class OptionalField(tpe: FieldType) extends FieldType

//case class GenericType(tpe: FieldType, genericTypes: FieldType) extends FieldType

case class ArrayType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class SetType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class ListType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class EnumerationType( /*tpe: FieldType*/ list: Seq[String]) extends FieldType

case class SelectType( /*tpe: FieldType*/ list: Seq[UserDefinedType]) extends FieldType

case class UserDefinedType(name: String) extends FieldType

case class GenericType(tpe: FieldType) extends FieldType

case class StringType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case class BinaryType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case object LongType extends PrimitiveType

case object LogicalType extends PrimitiveType

case object RealType extends PrimitiveType with NumericBasedType

case object IntegerType extends PrimitiveType with NumericBasedType

case object BooleanType extends PrimitiveType

case object NumberType extends PrimitiveType with NumericBasedType
/*case object ArrayType extends FieldType
case object SetType extends FieldType
case object ListType extends FieldType
case object EnumerationType extends FieldType
case object SelectType extends FieldType
case object GenericType extends FieldType*/

case class Derive(name: Tree, tpe: FieldType, expr: Tree) extends Typed

case class Inverse(name: String, tpe: FieldType, source: Ident) extends Named with Typed

case class Unique(name: String, sources: Seq[Ident]) extends Named

case class Where(name: String, expr: Tree) extends Named

case class Entity(name: String, isAbstract: Boolean, inheritsFrom: Seq[String], fields: Seq[Field], derives: Seq[Derive], inverses: Seq[Inverse], uniques: Seq[Unique], wheres: Seq[Where]) extends DefinedType

case class Type(name: String, tpe: FieldType, wheres: Seq[Where]) extends DefinedType with Typed

case class Comment(body: String)

case class Schema(name: String, definedTypes: Seq[DefinedType]) extends MainType with Named

case class Argument(name: String, tpe: FieldType) extends Named with Typed

case class Local(name: String, tpe: FieldType, expr: Option[Tree]) extends Named with Typed

case class Function(name: String, args: Seq[Argument], tpe: FieldType, locals: Seq[Local], body: Tree) extends DefinedType with Typed

case class Rule(name: String, types: Seq[FieldType], locals: Seq[Local], body: Tree, wheres: Seq[Where]) extends DefinedType

sealed trait MainType extends Named

sealed trait DefinedType extends MainType

sealed trait Named {
  val name: String
}

sealed trait Typed {
  val tpe: FieldType
}