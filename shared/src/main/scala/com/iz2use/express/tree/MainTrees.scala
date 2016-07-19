package com.iz2use.express.tree

case class Field(name: String, tpe: FieldType)

abstract sealed class FieldType

abstract sealed class PrimitiveType extends FieldType

case class OptionalField(tpe: FieldType) extends FieldType

//case class GenericType(tpe: FieldType, genericTypes: FieldType) extends FieldType

case class ArrayType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class SetType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class ListType(tpe: FieldType, dimensions: Option[Seq[Tree]], unique: Boolean) extends FieldType

case class EnumerationType( /*tpe: FieldType*/ list: Seq[String]) extends FieldType

case class SelectType( /*tpe: FieldType*/ list: Seq[String]) extends FieldType

case class GenericType(tpe: FieldType) extends FieldType

case class StringType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case class BinaryType(length: Option[Int], fixed: Boolean) extends PrimitiveType

case class UserDefinedType(name: String) extends FieldType

case object LongType extends PrimitiveType

case object LogicalType extends PrimitiveType

case object RealType extends PrimitiveType

case object IntegerType extends PrimitiveType

case object BooleanType extends PrimitiveType

case object NumberType extends PrimitiveType
/*case object ArrayType extends FieldType
case object SetType extends FieldType
case object ListType extends FieldType
case object EnumerationType extends FieldType
case object SelectType extends FieldType
case object GenericType extends FieldType*/

case class Derive(name: Tree, tpe: FieldType, expr: Tree)

case class Inverse(name: String, tpe: FieldType, source: Ident)

case class Unique(name: String, sources: Seq[Ident])

case class Where(name: String, expr: Tree)

case class Entity(name: String, isAbstract: Boolean, inheritsFrom: Seq[String], fields: Seq[Field], derives: Seq[Derive], inverses: Seq[Inverse], uniques: Seq[Unique], wheres: Seq[Where])

case class Type(name: String, tpe: FieldType, wheres: Seq[Where])

case class Comment(body: String)

case class Schema(name: String, entities: Seq[Entity], types: Seq[Type], functions: Seq[Function])

case class Argument(name: String, tpe: FieldType)

case class Local(name: String, tpe: FieldType, expr: Option[Tree])

case class Function(name: String, args: Seq[Argument], tpe: FieldType, locals: Seq[Local], body: Tree)