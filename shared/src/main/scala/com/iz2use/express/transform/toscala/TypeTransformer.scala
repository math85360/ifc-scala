package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

case class TypeTransformer(val entity: tree.Type) extends AnyVal {

  def scalaCode(implicit context: Context): String = {
    val wheres = entity.wheres.map(TreeTransformer.transformWhere).mkString(context.NL)
    s"""package express.${context.schema.name.toLowerCase}
""" +
      (entity.tpe match {
        case enum: tree.EnumerationType =>
          val elements = enum.list.map(i => s"""case object $i extends ${entity.name}""").mkString(context.NL)
          s"""
sealed trait ${entity.name} {
$wheres
}

object ${entity.name} {
$elements
}

"""

        case tree.SelectType(lst) =>
          val alternatives = lst.collect({ case tree.UserDefinedType(tpe) => s"""implicit object ${tpe}Witness extends ${entity.name}[$tpe]""" }).mkString(context.NL)
          val toImplements = lst.collect({
            case tree.UserDefinedType(tpe) => tpe
          }).flatMap(_name => context.schema.definedTypes.collectFirst({
            case tree.Entity(entName, _, _, fields, derives, _, _, _) if entName == _name =>
              fields.collect({ case tree.Field(name, tpe) => name -> tpe }) ++
                derives.collect({
                  case tree.Derive(tree.Ident(name), tpe, _) => name -> tpe
                  case tree.Derive(name, tpe, _) => TreeTransformer(name) + "/**/" -> tpe
                })
            /*(fields ++ derives).map(tpe => (tpe.name,tpe))*/
          })).reduceOption({ (a, b) => /*a.intersect(b)*/ (a ++ b).distinct }).map(_.map({
            case (name, tpe) =>
              val default = tpe.defaultValue
              s"def $name : ${tpe.scalaCode} = ${tpe.defaultValue}"
          })).getOrElse(Seq.empty).mkString(context.NL)
          /*.reduce({
            case (a, b) => a ++ b
          })*/
          //.flatten.groupBy(_.name).flatMap({ case (name, defined) => })

          s"""
trait ${entity.name} {
$toImplements
}
/*class ${entity.name}[T] {
}

object ${entity.name} {
$alternatives
}*/
"""
        /*s"""
trait ${entity.name} {
}"""*/
        case _ =>
          val addedBody = entity.tpe match {
            case UnderylingPrimitive(x) =>
              val default = x.defaultValue
              s"""
object ${entity.name} {

trait ${entity.name}Integral extends Integral[${x.scalaCode}] {
}

implicit object ${entity.name}Integral extends ${entity.name}Integral

implicit def valueToExpress(underlying: ${x.scalaCode}): ${entity.name} = new ${entity.name}(underlying)

implicit def valueToOptionExpress(underlying: ${x.scalaCode}): Option[${entity.name}] = Some(valueToExpress(underlying))

implicit def expressToValue(underlying: ${entity.name}): ${x.scalaCode} = underlying.SELF

implicit def expressOptToValue(underlying: Option[${entity.name}]): ${x.scalaCode} = underlying.map(_.SELF).getOrElse(${x.defaultValue})
}
"""
            case _ => ""
          }
          val extended = entity.tpe match {
            //case UnderylingPrimitive(x) => s"(val SELF: ${entity.tpe.scalaCode}) extends AnyVal"
            case UnderylingPrimitive(x) if x != entity.tpe => s"(val SELF: ${x.scalaCode}) extends ${entity.tpe.scalaCode}(SELF)"
            case _ => s"(val SELF: ${entity.tpe.scalaCode})"
          }
          s"""
class ${entity.name}$extended {
$wheres
}

$addedBody
"""
      })
  }
}

object UnderylingPrimitive {
  @inline
  def unapply(x: tree.DataType)(implicit context: Context): Option[tree.NumericBasedType] = {
    x.collectFirst({
      case Extends(tpe) => tpe
    }) {
      case x: tree.NumericBasedType => x
    }
  }
}

object Supertype {
  @inline
  def unapply(x: tree.DataType)(implicit context: Context): Option[tree.Type] = x match {
    case tree.UserDefinedType(_name) => context.schema.definedTypes.collectFirst({
      case tpe @ tree.Type(name, tree.SelectType(subtypes), _) if subtypes.contains(tree.UserDefinedType(_name)) => tpe
    })
    case _ => None
  }
}

object Extends {
  @inline
  def unapply(x: tree.DataType)(implicit context: Context): Option[tree.DataType] = x match {
    case tpe: tree.UserDefinedType => tpe.getType.map(_.tpe)
    case _ => None
  }
}