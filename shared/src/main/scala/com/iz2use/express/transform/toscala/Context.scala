package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec

case class Context(
    schema: tree.Schema,
    private[Context] var current: tree.MainType = null, //private[Context] stack: Vector[tree.MainType] = Vector.empty,
    private[Context]parentContext: Context = null,
    private[Context] var namesCache: Map[String, String] = Map.empty,
    NL: String) {

  if (current == null) current = schema

  //def collectFirst[A](pf: PartialFunction[tree.MainType, A]): Option[A] = //pf.lift(current)
  //pathToRoot.map(_.current).collectFirst(pf)

  def +(tpe: tree.MainType) = this.copy(current = tpe, parentContext = this)

  def withLocals(locals: (String, String)*) = this.copy(namesCache = locals.toMap, parentContext = this)

  private def transformName(name: String) = name.toLowerCase()

  private def transformEntityToNameMap(tpe: tree.MainType) = (tpe match {
    case s: tree.Schema => s.definedTypes
    case e: tree.Entity => e.fields
    case e: tree.Function => e.args ++ e.locals
    //case e: tree.Function => e.args.map(_.name)
    case _ => Seq.empty
  }).map(_.name).map(name => transformName(name) -> name).toMap

  var names: Map[String, String] = transformEntityToNameMap(current)

  //var namesCache: Map[String, String] = Map.empty

  /*val noNormalizedName: PartialFunction[String, String] = {
    case name => name + "/* notFound */"
  }

  val parentNormalizedName: PartialFunction[String, String] =
    if (parentContext != null)
      parentContext._getNormalizedNameFor
    else
      noNormalizedName*/

  def pathToRoot: Stream[Context] = Stream.cons(this, if (parentContext == null) Stream.empty else parentContext.pathToRoot)

  def getNormalizedNameFor(name: String) = {
    def _getFirstName(firstContext: Context, lowerName: String, originalName: String) = {
      pathToRoot.collectFirst({
        case ctx if ctx.hasLocalName.isDefinedAt(lowerName) => ctx.hasLocalName(lowerName)
      })
    }
    _getFirstName(this, transformName(name), name).getOrElse(s"$name /*!*/")
  }

  val hasLocalName = namesCache.orElse(names)

  def cacheNormalizedName(lowerName: String, rightName: String): String = {
    namesCache = namesCache + (lowerName -> rightName)
    rightName
  }
  /*def getNormalizedNameFor(rightName: String): String = {
    val lowerName = transformName(rightName)
    namesCache.orElse(
      _getNormalizedNameFor.andThen(cacheNormalizedName(lowerName, _)))(lowerName)
  }


  private val _getNormalizedNameFor: PartialFunction[String, String] = {
    names.orElse(parentNormalizedName)
  }*/

  def pushNamesFrom(entities: Seq[tree.Entity]) = {
    names = names ++ entities.flatMap(transformEntityToNameMap)
  }

}
