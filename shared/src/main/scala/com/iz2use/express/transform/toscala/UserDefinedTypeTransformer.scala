package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

case class UserDefinedTypeTransformer(val entity: tree.UserDefinedType) extends AnyVal {
  @inline
  def getType(implicit context: Context) = {
    context.schema.definedTypes.collectFirst({
      case tpe @ tree.Type(name, _, _) if name equalsIgnoreCase entity.name => tpe
    })
  }
}