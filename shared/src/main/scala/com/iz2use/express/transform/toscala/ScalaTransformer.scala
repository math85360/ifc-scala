package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import com.iz2use.express.tree.OptionalField

object ScalaTransformer {
  val NL = "\r\n"
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(path: String, data: String): Unit =
    using(new FileWriter(path))(_.write(data))

  def appendToFile(path: String, data: String): Unit =
    using(new PrintWriter(new FileWriter(path, true)))(_.println(data))

  def transformSchema(schema: tree.Schema) = {
    val context = Context(schema) + schema
    val path = s"../ifc4test/src/main/scala/express/${schema.name.toLowerCase}"
    (
      new File(path)).mkdirs()

    schema.definedTypes.foreach({ tpe =>
      implicit val ctx = context + tpe
      val content = (tpe match {
        case e: tree.Entity => transformEntity(e)
        case e: tree.Function => transformFunction(e)
        case e: tree.Type => transformDefinedType(e)
        case e: tree.Rule => transformRule(e)
      })
      writeToFile(s"$path/${tpe.name}.scala", content)
    })
  }

  @tailrec
  def allEntitiesInheritedBy(processing: Seq[tree.Entity], rest: Seq[tree.Entity] = Seq.empty)(implicit context: Context): Seq[tree.Entity] = {
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

  def transformEntity(entity: tree.Entity)(implicit context: Context) = {
    val inheritedEntities = allEntitiesInheritedBy(Seq(entity))
    context.pushNamesFrom(inheritedEntities)
    val inverses = entity.inverses.map(transformInverse).mkString(NL)
    val derives = entity.derives.map(transformDerive).mkString(NL)
    val uniques = entity.derives.map(transformDerive).mkString(NL)
    val wheres = entity.wheres.map(transformWhere).mkString(NL)
    s"""package express.${context.schema.name.toLowerCase}
""" + (
      if (false && entity.isAbstract) {
        val fields = entity.fields.map(transformField(true)).mkString("(" + NL, NL + ", ", NL + ")")
        val inheritsFrom = if (entity.inheritsFrom.isEmpty) "" else " extends " + entity.inheritsFrom.mkString(" with " + NL)
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
          case tree.Type(name, tree.SelectType(subtypes), _) if subtypes.contains(entity.name) => name
        })
        val inheritsFrom = if (extended.isEmpty) "" else " extends " + NL + extended.mkString(" with " + NL) + NL
        val fields = inheritedEntities.flatMap(_.fields).map(transformField(false)).mkString("(" + NL, "," + NL, NL + ")")
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

  def transformDefinedType(entity: tree.Type)(implicit context: Context) = {
    val wheres = entity.wheres.map(transformWhere).mkString(NL)
    s"""package express.${context.schema.name.toLowerCase}
""" +
      (entity.tpe match {
        case enum: tree.EnumerationType =>
          val elements = enum.list.map(i => s"""case object $i extends ${entity.name}""").mkString(NL)
          s"""
sealed trait ${entity.name} {
$wheres
}

object ${entity.name} {
$elements
}

"""

        case tree.SelectType(lst) =>
          val alternatives = lst.collect({ case tree.UserDefinedType(tpe) => s"""implicit object ${tpe}Witness extends ${entity.name}[$tpe]""" }).mkString(NL)
          s"""
class ${entity.name}[T]

object ${entity.name} {
$alternatives
}
"""
        /*s"""
trait ${entity.name} {
}"""*/
        case _ =>
          s"""
class ${entity.name}(SELF: ${transformType(entity.tpe)}) {
$wheres
}
"""
      })
  }

  val defaultValueForType: PartialFunction[tree.FieldType, String] = {
    case tree.OptionalField(tpe) => tpe match {
      case a if defaultValueForType.isDefinedAt(a) => defaultValueForType(a)
      case _ => "null"
    }
    case tree.RealType => "0.0"
    case tree.NumberType | tree.LongType | tree.IntegerType => "0"
  }

  @tailrec
  def traverseType[A](_tpe: tree.FieldType)(implicit toKeep: PartialFunction[tree.FieldType, A], context: Context): Option[A] = {
    _tpe match {
      case tpe if toKeep.isDefinedAt(tpe) => Some(toKeep(tpe))
      case tree.OptionalField(tpe) => traverseType(tpe)
      case tree.ListType(tpe, _, _) => traverseType(tpe)
      case tree.SetType(tpe, _, _) => traverseType(tpe)
      case tree.ArrayType(tpe, _, _) => traverseType(tpe)
      case tree.UserDefinedType(name) =>
        val r = context.schema.definedTypes.collectFirst({
          case tree.Type(_name, tree.SelectType(lst), _) if _name.equalsIgnoreCase(name) =>
            tree.GenericType(tree.UserDefinedType("A : " + name))
        })
        if (r.isEmpty)
          None
        else
          traverseType(r.get)
      case _ => None
    }
  }

  def transformFunction(entity: tree.Function)(implicit context: Context) = {
    val args = entity.args.map(transformArgument).mkString("," + NL)
    val genericList = (entity.args.map(_.tpe) :+ entity.tpe).flatMap(traverseType(_)({
      case tree.GenericType(tree.UserDefinedType(tpe)) => tpe
    }, context)).distinct
    val generics = (if (genericList.isEmpty) "" else genericList.mkString("[", ", ", "]"))
    val locals = entity.locals.map({ local =>
      val expr = local.expr.map(transformTree).fold(" = " + defaultValueForType.lift(local.tpe).getOrElse("null"))(" = " + _)
      s"""var ${local.name} : ${transformType(local.tpe)}$expr"""
    }).mkString(NL)
    s"""package express.${context.schema.name.toLowerCase}

object ${entity.name} {
def apply$generics($args) : ${transformType(entity.tpe)} = {
$locals
${transformTree(entity.body)}
}
}
"""
  }

  def transformRule(entity: tree.Rule)(implicit context: Context) = {
    //val args = entity.args.map(transformArgument).mkString("," + NL)
    /*val genericList = (entity.args.map(_.tpe) :+ entity.tpe).flatMap(traverseType(_)({
      case tree.GenericType(tpe) => tpe
    })).map({
      case tree.UserDefinedType(tpe) => tpe
    }).distinct
    val generics = (if (genericList.isEmpty) "" else genericList.mkString("[", ", ",
    "]"))*/
    val wheres = entity.wheres.map(transformWhere).mkString(NL)
    val locals = entity.locals.map({ local =>
      val expr = local.expr.map(transformTree).fold(" = " + defaultValueForType.lift(local.tpe).getOrElse("null"))(" = " + _)
      s"""var ${local.name} : ${transformType(local.tpe)}$expr"""
    }).mkString(NL)
    s"""package express.${context.schema.name.toLowerCase}

object ${entity.name} {
def apply() = {
$locals
${transformTree(entity.body)}
}
$wheres
}
"""
  }

  def transformArgument(arg: tree.Argument)(implicit context: Context) = {
    s"""${arg.name} : ${transformType(arg.tpe)}"""
  }

  def transformField(inBody: Boolean)(field: tree.Field)(implicit context: Context) = {
    val suffix = (if (inBody) { " = _" } else { defaultValueForType.lift(field.tpe).map(" = " + _).getOrElse("") })
    s"""var ${field.name} : ${transformType(field.tpe)}$suffix"""
  }

  def transformInverse(field: tree.Inverse)(implicit context: Context) = {
    s"""def ${field.name} : ${transformType(field.tpe)} = null // inverse from ${field.source}"""
  }

  def transformUnique(field: tree.Unique)(implicit context: Context) = {
    s"""def unique_${field.name} = ${field.sources} // unique"""
  }

  def transformDerive(field: tree.Derive)(implicit context: Context) = {
    s"""def ${transformTree(field.name)} : ${transformType(field.tpe)} = ${transformTree(field.expr)} // derive"""
  }

  def transformWhere(where: tree.Where)(implicit context: Context) = {
    s"""def ${where.name} = ${transformTree(where.expr)} // where"""
  }

  def transformOperator(op: tree.Operator) = op match {
    case tree.NotEquals => "!="
    case tree.Equals => "=="
    case tree.LessThan => "<"
    case tree.LessThanOrEquals => "<="
    case tree.GreaterThan => ">"
    case tree.GreaterThanOrEquals => ">="
    case tree.And => "&&"
    case tree.Or => "||"
    case tree.Xor => "xor"
    case tree.In => "in"
    case tree.OrElse => "orElse"
    case tree.Sum => "+"
    case tree.Difference => "-"
    case tree.Multiply => "*"
    case tree.Divide => "/"
    case tree.Mod => "%"
    case tree.InstanceEquals => "=="
    case tree.InstanceNotEquals => "!="
    case e => e
  }

  def transformArgs(args: Seq[tree.Tree])(implicit context: Context): String = {
    args.map(transformTree).mkString(", ")
  }

  def transformTree(_expr: tree.Tree)(implicit context: Context): String = {
    _expr match {
      case tree.Apply(fun, args) => args.map(transformTree).mkString(s"${transformTree(fun)}(", ", ", ")")
      case tree.Self => "SELF"
      case tree.Ident(name) => context.getNormalizedNameFor(name)
      case tree.Select(tree.Super(tree.Self, tree.Ident(_)), name) => transformTree(name)
      case tree.Select(tree.Self, name) => transformTree(name)
      case tree.Select(qlf, name) =>
        // TODO .property must be a property from qlf
        s"${transformTree(qlf)}.${transformTree(name)}"
      case tree.Group(expr) => s"(${transformTree(expr)})"
      case tree.Literal(tree.Constant(constant)) => constant match {
        case str: String => "\"" + str + "\""
        case e => e.toString()
      }
      case tree.DefaultFunctionCall(tree.EXISTS, args) => s"${transformArgs(args)} != null" // s"${transformArgs(args)}.isDefined"
      case tree.Super(tree.Self, tree.Ident(name)) => s"this" // case tree.Super(tree.Self, tree.Ident(name)) => s"super[$name]"
      case tree.Super(root, tree.Ident(name)) => s"${transformTree(root)}" //case tree.Super(root, tree.Ident(name)) => s"${transformTree(root)}.super[$name]"
      case tree.BooleanOperation(tree.Literal(tree.Constant(lhs: String)), tree.In, tree.DefaultFunctionCall(tree.TYPEOF, rhs)) =>
        s"${transformTree(rhs.head)}.getClass.toString.toLowerCase.endsWith(" + "\"" + lhs.toLowerCase + "\"" + ")"
      //s"${transformTree(rhs)}.isInstanceOf($lhs)"
      case tree.BooleanOperation(lhs, tree.In, rhs) => s"${transformTree(rhs)}.contains(${transformTree(lhs)})"
      case tree.ArithmeticOperation(tree.Literal(tree.Constant(lhs: Long)), tree.Difference, tree.Literal(tree.Constant(rhs: Long))) => (lhs - rhs).toString()
      case tree.BinaryOperation(lhs, op, rhs) => s"${transformTree(lhs)} ${transformOperator(op)} ${transformTree(rhs)}"
      case tree.NotOperator(tree) => s"!${transformTree(tree)}"
      case tree.NegativeOperator(tree) => s"-${transformTree(tree)}"
      case tree.DefaultFunctionCall(tree.SIZEOF, args) => s"${transformArgs(args)}.size"
      case tree.DefaultFunctionCall(tree.TYPEOF, args) => s"${transformArgs(args)}.getClass"
      case tree.DefaultFunctionCall(tree.ABS, args) => s"${transformTree(args.head)}.abs"
      case tree.DefaultFunctionCall(tree.SQRT, args) => s"Math.sqrt(${transformTree(args.head)})"
      case tree.DefaultFunctionCall(tree.NVL, args) => args.map(arg => s"Option(${transformTree(arg)})").reduce((a, b) => s"$a.orElse($b).get")
      case tree.DefaultFunctionCall(tree.HIINDEX, args) => s"(${transformTree(args.head)}.size - 1)"
      case tree.Between(lv, il, v, ih, hv) =>
        val c = transformTree(v)
        val l = (if (il) "=" else "")
        val h = (if (ih) "=" else "")
        s"((${transformTree(lv)} <$l $c) && ($c <$h ${transformTree(hv)}))"
      case tree.Query(name, query, cond) => s"${transformTree(query)}.filter({$name => ${transformTree(cond)}})"
      case tree.ArrayPrimitives(items) => items.map(transformTree).mkString("collection.mutable.ArrayBuffer(", ", ", ")")
      case tree.If(cond, thenp, elsep) => s"""if(${transformTree(cond)}) {
${transformTree(thenp)}
}else{
${transformTree(elsep)}
}"""
      case tree.Block(items) => items.map(transformTree).mkString(NL)
      case tree.ValDef(lhs, _, _, _, rhs) => s"""${transformTree(lhs)} = ${transformTree(rhs)}"""
      case tree.Return(expr) => s"""return ${transformTree(expr)}"""
      case tree.EmptyTree => "null"
      case expr => s"/*${expr.toString()}*/"
    }
  }

  def transformType(_tpe: tree.FieldType)(implicit context: Context): String = _tpe match {
    case tree.OptionalField(tpe) => transformType(tpe) // s"Option[${transformType(tpe)}]"
    case tree.ArrayType(tpe, dims, unique) => s"collection.mutable.ArrayBuffer[${transformType(tpe)}]"
    case tree.SetType(tpe, dims, unique) => s"collection.mutable.Set[${transformType(tpe)}]"
    case tree.ListType(tpe, dims, unique) => s"collection.mutable.ListBuffer[${transformType(tpe)}]"
    case tree.EnumerationType(list) => list.mkString("String /* Enumeration(", ", ", ") */")
    case tree.SelectType(list) => list.mkString("String /* Select(", ", ", ") */")
    case tree.StringType(length, fixed) => s"String /* $length, $fixed */"
    case tree.BinaryType(length, fixed) => s"String /*Binary $length, $fixed */"
    case tree.LogicalType | tree.BooleanType => s"Boolean"
    case tree.LongType => s"Long"
    case tree.IntegerType => s"Int"
    case tree.RealType => s"Double"
    case tree.NumberType => s"Double"
    case tree.UserDefinedType(name) =>
      val r = context.schema.definedTypes.collectFirst({
        case tree.Type(_name, tree.SelectType(lst), _) if _name.equalsIgnoreCase(name) => "A"
      })
      r.getOrElse(name)
    case tree.GenericType(tpe) => transformType(tpe)
  }
}

case class Context(
    schema: tree.Schema,
    private[Context] var current: tree.MainType = null, //private[Context] stack: Vector[tree.MainType] = Vector.empty,
    private[Context]parentContext: Context = null) {

  if (current == null) current = schema

  def +(tpe: tree.MainType) = this.copy(
    current = tpe, //stack = stack :+ tpe,
    parentContext = this)

  private def transformName(name: String) = name.toLowerCase()

  private def transformEntityToNameMap(tpe: tree.MainType) = (tpe match {
    case s: tree.Schema => s.definedTypes
    case e: tree.Entity => e.fields
    case e: tree.Function => e.args ++ e.locals
    //case e: tree.Function => e.args.map(_.name)
    case _ => Seq.empty
  }).map(_.name).map(name => transformName(name) -> name).toMap

  var names: Map[String, String] = transformEntityToNameMap(current)

  var namesCache: Map[String, String] = Map.empty

  /*val noNormalizedName: PartialFunction[String, String] = {
    case name => name + "/* notFound */"
  }

  val parentNormalizedName: PartialFunction[String, String] =
    if (parentContext != null)
      parentContext._getNormalizedNameFor
    else
      noNormalizedName*/

  def getNormalizedNameFor(name: String) = {
    _getFirstName(this, transformName(name), name).getOrElse(s"$name /* notFound */")
  }

  private def _getFirstName(firstContext: Context, lowerName: String, originalName: String) = {
    getFirstInStack[String](firstContext) { ctx =>
      ctx.namesCache.orElse(ctx.names.andThen(cacheNormalizedName(lowerName, _))).lift(lowerName)
    }
  }

  @tailrec
  private def getFirstInStack[A](ctx: Context)(implicit todo: Context => Option[A]): Option[A] = {
    todo(ctx) match {
      case None =>
        if (ctx.parentContext != null)
          getFirstInStack(ctx.parentContext)
        else
          None
      case a => a
    }
  }

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