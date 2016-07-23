package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

case class EntityTransformer(val entity:tree.Entity) extends AnyVal {
  
    //def allEntitiesInherited(implicit context:Context) = allEntitiesInheritedBy(Seq(self))
  
  @tailrec
  private def allEntitiesInheritedBy(processing: Seq[tree.Entity], rest: Seq[tree.Entity] = Seq.empty)(implicit context: Context): Seq[tree.Entity] = {
    //entity.inheritsFrom.foldLeft(
    processing match {
      case Nil =>
        rest
      case head :: tail =>
        val entities = (for {
          name <- head.inheritsFrom
          ent <- context.schema.definedTypes.collect({
            case e @ tree.Entity(_name, _, _, _, _, _, _, _) if name == _name => e
          })
        } yield ent)
        allEntitiesInheritedBy(tail ++ entities, head +: rest)
    } /*
    if (entity.inheritsFrom.isEmpty) {
      rest
    } else {
      
      entities.flatMap(allEntitiesInheritedBy(_, entities ++ rest))
    }*/
  }

    
  def scalaCode(implicit context: Context) = {
    val inheritedEntities = allEntitiesInheritedBy(Seq(entity))
    context.pushNamesFrom(inheritedEntities)
    val inverses = entity.inverses.map(TreeTransformer.transformInverse).mkString(context.NL)
    val derives = entity.derives.map(TreeTransformer.transformDerive).mkString(context.NL)
    val uniques = entity.derives.map(TreeTransformer.transformDerive).mkString(context.NL)
    val wheres = entity.wheres.map(TreeTransformer.transformWhere).mkString(context.NL)
    s"""package express.${context.schema.name.toLowerCase}
""" + (
      if (false && entity.isAbstract) {
        val fields = entity.fields.map(TreeTransformer.transformField(true)).mkString("(" + context.NL, context.NL + ", ", context.NL + ")")
        val inheritsFrom = if (entity.inheritsFrom.isEmpty) "" else " extends " + entity.inheritsFrom.mkString(" with " + context.NL)
        s"""
trait ${entity.name}$inheritsFrom {
$fields
$inverses
$uniques
$wheres
}
"""
      } else {
        val extended = entity.inheritsFrom.map({ inh =>
          //if (inh.isAbstract) { inh } else {
          inheritedEntities.flatMap({ ent =>
            if (ent == entity)
              Seq.empty
            else
              ent.fields
          }).map(_.name).mkString(s"$inh(", ", ", ")")
          //}
        }) ++ context.schema.definedTypes.collect({
          case tree.Type(name, tree.SelectType(subtypes), _) if subtypes.contains(tree.UserDefinedType(entity.name)) => name
        })

        /*def findSelectFrom(entity: tree.Entity) = {
    schema.definedTypes.collect({
      case tree.Type(name, tree.SelectType(lst), _) if lst.contains(entity.name) => name
    })
  }*/
        val inheritsFrom = if (extended.isEmpty) "" else " extends " + context.NL + extended.mkString(" with " + context.NL) + context.NL
        val fields = inheritedEntities.flatMap(_.fields).map(TreeTransformer.transformField(false)).mkString("(" + context.NL, "," + context.NL, context.NL + ")")
        val prefix = (if (entity.isAbstract) "abstract " else "")
        s"""
${prefix}class ${entity.name}$fields$inheritsFrom {SELF =>
$inverses
$uniques
$wheres
}
"""
      })
  }

}