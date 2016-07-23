package com.iz2use.express.tree

case class Field(name: String, tpe: DataType) extends Named with Typed

abstract sealed class DataType

abstract sealed class PrimitiveType extends DataType

abstract sealed trait NumericBasedType extends PrimitiveType

case class OptionalField(tpe: DataType) extends DataType

//case class GenericType(tpe: DataType, genericTypes: DataType) extends DataType

case class ArrayType(tpe: DataType, dimensions: Option[Seq[Tree]], unique: Boolean) extends DataType

case class SetType(tpe: DataType, dimensions: Option[Seq[Tree]], unique: Boolean) extends DataType

case class ListType(tpe: DataType, dimensions: Option[Seq[Tree]], unique: Boolean) extends DataType

case class EnumerationType( /*tpe: DataType*/ list: Seq[String]) extends DataType

case class SelectType( /*tpe: DataType*/ list: Seq[UserDefinedType]) extends DataType

case class UserDefinedType(name: String) extends DataType

case class GenericType(tpe: DataType) extends DataType

case class StringType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case class BinaryType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case object LongType extends PrimitiveType

case object LogicalType extends PrimitiveType

case object RealType extends PrimitiveType with NumericBasedType

case object IntegerType extends PrimitiveType with NumericBasedType

case object BooleanType extends PrimitiveType

case object NumberType extends PrimitiveType with NumericBasedType
/*case object ArrayType extends DataType
case object SetType extends DataType
case object ListType extends DataType
case object EnumerationType extends DataType
case object SelectType extends DataType
case object GenericType extends DataType*/

case class Derive(name: Tree, tpe: DataType, expr: Tree) extends Typed

case class Inverse(name: String, tpe: DataType, source: Ident) extends Named with Typed

case class Unique(name: String, sources: Seq[Ident]) extends Named

case class Where(name: String, expr: Tree) extends Named

case class Entity(name: String, isAbstract: Boolean, inheritsFrom: Seq[String], fields: Seq[Field], derives: Seq[Derive], inverses: Seq[Inverse], uniques: Seq[Unique], wheres: Seq[Where]) extends DefinedType

case class Type(name: String, tpe: DataType, wheres: Seq[Where]) extends DefinedType with Typed

case class Comment(body: String)

case class Schema(name: String, definedTypes: Seq[DefinedType]) extends MainType with Named

case class Argument(name: String, tpe: DataType) extends Named with Typed

case class Local(name: String, tpe: DataType, expr: Option[Tree]) extends Named with Typed

case class Function(name: String, args: Seq[Argument], tpe: DataType, locals: Seq[Local], body: Tree) extends DefinedType with Typed

case class Rule(name: String, types: Seq[DataType], locals: Seq[Local], body: Tree, wheres: Seq[Where]) extends DefinedType

sealed trait MainType extends Named

sealed trait DefinedType extends MainType

sealed trait Named {
  val name: String
}

sealed trait Typed {
  val tpe: DataType
}