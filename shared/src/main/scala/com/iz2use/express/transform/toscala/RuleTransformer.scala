package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

case class RuleTransformer(val entity: tree.Rule) extends AnyVal {
    def scalaCode(implicit context: Context) = {
    //val args = entity.args.map(transformArgument).mkString("," + NL)
    /*val genericList = (entity.args.map(_.tpe) :+ entity.tpe).flatMap(traverseType(_)({
      case tree.GenericType(tpe) => tpe
    })).map({
      case tree.UserDefinedType(tpe) => tpe
    }).distinct
    val generics = (if (genericList.isEmpty) "" else genericList.mkString("[", ", ",
    "]"))*/
    val wheres = entity.wheres.map(TreeTransformer.transformWhere).mkString(context.NL)
    val locals = entity.locals.map({ local =>
      val expr = local.expr.map(TreeTransformer(_)).fold(" = " + local.tpe.defaultValue)(" = " + _)
      s"""var ${local.name} : ${local.tpe.scalaCode}$expr"""
    }).mkString(context.NL)
    s"""package express.${context.schema.name.toLowerCase}

import com.iz2use.express.datatype._

object ${entity.name} {
def apply() = {
$locals
${TreeTransformer(entity.body)}
}
$wheres
}
"""
  }
}