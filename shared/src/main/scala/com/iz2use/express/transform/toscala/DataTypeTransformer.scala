package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

object TraverserDefault {
  val traverseSeqLike: PartialFunction[tree.DataType, tree.DataType] = {
    case tree.OptionalField(tpe) => tpe
    case tree.ListType(tpe, _, _) => tpe
    case tree.SetType(tpe, _, _) => tpe
    case tree.ArrayType(tpe, _, _) => tpe
  }
  // TODO activate when Type has UserDefinedType
  /*def userDefinedType(implicit context:Context): PartialFunction[tree.DataType, tree.DataType] = {
    case tree.UserDefinedType(_name) =>
      context.schema.definedTypes.collectFirst({
        case tree.Type(name, tpe, _) if name == _name => tpe
      })
  }*/
}

case class DataTypeTransformer(val self: tree.DataType) extends AnyVal {

  /*val defaultValueForType: PartialFunction[(tree.DataType, Context), String] = {
    case (tree.OptionalField(tpe), ctx) => tpe match {
      case a if defaultValueForType.isDefinedAt((a, ctx)) => defaultValueForType((a, ctx))
      case _ => "null"
    }
    case (tree.RealType, _) => "0.0"
    case (tree.NumberType, _) | (tree.LongType, _) | (tree.IntegerType, _) => "0"
    case (tree.UserDefinedType(x), ctx) => ctx.schema
  }*/

  def defaultValue(implicit context: Context): String = defaultValue(a => a, "null")

  def defaultValue(found: String => String, notFound: => String)(implicit context: Context): String = self match {
    //case tree.OptionalField(tpe) => defaultValueForType(tpe, found, notFound)
    case tree.OptionalField(tpe) => found("None")
    case tree.RealType => found("0.0")
    case tree.NumberType | tree.LongType | tree.IntegerType => found("0")
    case tree.UserDefinedType(_name) =>
      context.schema.definedTypes.collectFirst({
        case tree.Type(name, tpe, _) if name == _name => tpe
      }).map(_.defaultValue(found, notFound)).getOrElse(notFound)
    //case (tree.UserDefinedType(x), ctx) => ctx.schema
    case _ => notFound
  }

  def traverse(implicit visitor: PartialFunction[tree.DataType, tree.DataType]): Stream[tree.DataType] =
    Stream.cons(self, visitor.lift(self).map(_.traverse).getOrElse(Stream.empty))

  def collectFirst[A](visitor: PartialFunction[tree.DataType, tree.DataType])(pf: PartialFunction[tree.DataType, A]): Option[A] = traverse(visitor).collectFirst(pf)

  /*@tailrec
  def traverseAndCollect[A](collector: PartialFunction[tree.DataType, A])(implicit visitor: PartialFunction[tree.DataType, tree.DataType]) :Option[A]= traverse(visitor) match {
    case Stream.empty => None
    case head #:: tail => if(collector.isDefinedAt(head)) Some(collector(head)) else tail
  }*/
  //traverse().collectFirst(pf)

  //def traverseType[A](implicit toKeep: PartialFunction[tree.DataType, A], context: Context): Option[A] = traverseTypeFor(self)

  // TraversableAgain ?
  // Stream ?

  /*@tailrec
  private def traverseTypeFor[A](_tpe: tree.DataType)(implicit toKeep: PartialFunction[tree.DataType, A], context: Context): Option[A] = {
    _tpe match {
      case tpe if toKeep.isDefinedAt(tpe) => Some(toKeep(tpe))
      case tree.OptionalField(tpe) => traverseTypeFor(tpe)
      case tree.ListType(tpe, _, _) => traverseTypeFor(tpe)
      case tree.SetType(tpe, _, _) => traverseTypeFor(tpe)
      case tree.ArrayType(tpe, _, _) => traverseTypeFor(tpe)
      /*case tree.UserDefinedType(name) =>
        val r = context.schema.definedTypes.collectFirst({
          case tree.Type(_name, tree.SelectType(lst), _) if _name.equalsIgnoreCase(name) =>
            tree.GenericType(tree.UserDefinedType("A : " + name))
        })
        if (r.isEmpty)
          None
        else
          traverseType(r.get)*/
      case _ => None
    }
  }*/

  def scalaCode(implicit context: Context): String = self match {
    case tree.OptionalField(tpe) => tpe.scalaCode //s"Option[${tpe.scalaCode}]" // 
    case tree.ArrayType(tpe, dims, unique) => s"Array[${tpe.scalaCode}]"
    case tree.SetType(tpe, dims, unique) => s"Set[${tpe.scalaCode}]"
    case tree.ListType(tpe, dims, unique) => s"List[${tpe.scalaCode}]"
    case tree.EnumerationType(list) => list.mkString("String /* Enumeration(", ", ", ") */")
    case tree.SelectType(list) => list.mkString("String /* Select(", ", ", ") */")
    case tree.StringType(length, fixed) => s"String /* $length, $fixed */"
    case tree.BinaryType(length, fixed) => s"String /*Binary $length, $fixed */"
    case tree.LogicalType | tree.BooleanType => s"Boolean"
    case tree.LongType => s"Long"
    case tree.IntegerType => s"Int"
    case tree.RealType | tree.NumberType => s"Double"
    case tree.UserDefinedType(name) =>
      /*val r = context.schema.definedTypes.collectFirst({
        case tree.Type(_name, tree.SelectType(lst), _) if _name.equalsIgnoreCase(name) => "A"
      })
      r.getOrElse(name)*/
      name
    case tree.GenericType(tpe) => tpe.scalaCode
  }

  def nativeScalaCode(implicit context: Context): String = self match {
    case tree.StringType(length, fixed) => s"String"
    case tree.BinaryType(length, fixed) => s"String"
    case tree.LogicalType | tree.BooleanType => s"Boolean"
    case tree.LongType => s"Long"
    case tree.IntegerType => s"Int"
    case tree.RealType | tree.NumberType => s"Double"
    case Extends(tpe) => tpe.nativeScalaCode
    case _ => scalaCode
  }

  /*def scalaType[A](implicit context: Context): Class[_] = self match {
    case tree.OptionalField(tpe: A) => classOf[Option[A]]
    case tree.ArrayType(tpe: A, dims, unique) => classOf[collection.mutable.ArrayBuffer[A]]
    case tree.SetType(tpe: A, dims, unique) => classOf[collection.mutable.Set[A]]
    case tree.ListType(tpe: A, dims, unique) => classOf[collection.mutable.ListBuffer[A]]
    case tree.EnumerationType(list) => classOf[String] //list.mkString("String /* Enumeration(", ", ", ") */")
    case tree.SelectType(list) => classOf[String] //list.mkString("String /* Select(", ", ", ") */")
    case tree.StringType(length, fixed) => classOf[String] //s"String /* $length, $fixed */"
    case tree.BinaryType(length, fixed) => classOf[String] //s"String /*Binary $length, $fixed */"
    case tree.LogicalType | tree.BooleanType => classOf[Boolean] //s"Boolean"
    case tree.LongType => classOf[Long]
    case tree.IntegerType => classOf[Int]
    case tree.RealType => classOf[Double]
    case tree.NumberType => classOf[Double]
    case tree.UserDefinedType(name) =>
      /*val r = context.schema.definedTypes.collectFirst({
        case tree.Type(_name, tree.SelectType(lst), _) if _name.equalsIgnoreCase(name) => "A"
      })
      r.getOrElse(name)*/
      null
    case tree.GenericType(tpe) => tpe.scalaType
  }*/
}