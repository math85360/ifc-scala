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

case class Where(name: String, expr: Tree)

case class Entity(name: String, inheritsFrom: Seq[String], fields: Seq[Field], wheres: Seq[Where])

case class Comment(body:String)

case class Schema(name: String, entities: Seq[Entity])