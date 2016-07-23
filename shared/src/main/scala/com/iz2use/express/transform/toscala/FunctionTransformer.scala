package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

case class FunctionTransformer(val entity: tree.Function) extends AnyVal {
  def scalaCode(implicit context: Context) = {
    val args = entity.args.map(TreeTransformer.transformArgument).mkString("," + context.NL)
    val genericList = (entity.args.map(_.tpe) :+ entity.tpe).flatMap(
      _.collectFirst(TraverserDefault.traverseSeqLike) {
        case tree.GenericType(tree.UserDefinedType(tpe)) => tpe
      }).distinct
    val generics = (if (genericList.isEmpty) "" else genericList.mkString("[", ", ", "]"))
    val locals = entity.locals.map({ local =>
      val expr = local.expr.map(TreeTransformer(_)).fold(" = " + local.tpe.defaultValue)(" = " + _)
      s"""var ${local.name} : ${local.tpe.scalaCode}$expr"""
    }).mkString(context.NL)
    s"""package express.${context.schema.name.toLowerCase}

object ${entity.name} {
def apply$generics($args) : ${entity.tpe.scalaCode} = {
$locals
${TreeTransformer(entity.body)}
}
}
"""
  }
}