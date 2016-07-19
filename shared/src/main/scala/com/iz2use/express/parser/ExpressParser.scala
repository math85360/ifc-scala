package com.iz2use.express.parser

import com.iz2use.express.tree

object ExpressParser extends Symbols with FieldType with Base with Expr {
  import fastparse.noApi._
  import White._

  //val space = P(" " | "\t" | "\r\n" | "\r" | "\n")

  val field: Parser[tree.Field] = P(name.! ~ ":" ~/ OPTIONAL.!.? ~/ fieldType ~/ EOS).map({
    case (name, opt, tpe) => tree.Field(name, opt.foldLeft(tpe)((acc, cur) => tree.OptionalField(acc)))
  })

  val fields = P(field.rep(0))

  val deriveField = P((SELF.! ~ "\\").? ~ name.!.map(tree.Ident).rep(1, ".") ~ ":" ~/ fieldType ~/ ":=" ~/ condition ~/ EOS).map({
    case (selfOpt, names, tpe, expr) => tree.Derive(selfOpt.fold[Seq[tree.Tree]](names)(_ => tree.Super(tree.Self, names.head) +: names.tail).reduce((a, b) => tree.Select(a, b)), tpe, expr)
  })

  val derives = P(DERIVE ~/ deriveField.rep(1))

  val inverseField = P(name.! ~ ":" ~/ fieldType ~/ FOR ~/ name.! ~/ EOS).map({
    case (name, tpe, source) => tree.Inverse(name, tpe, tree.Ident(source))
  })

  val inverses = P(INVERSE ~/ inverseField.rep(1))

  val whereField: Parser[tree.Where] = P(name.! ~ ":" ~/ condition ~/ EOS).map({
    case (name, expr) => tree.Where(name, expr)
  })

  val wheres = P(WHERE ~/ whereField.rep(1))

  val uniqueField = P(name.! ~ ":" ~/ name.!.rep(1, ",") ~/ EOS).map({
    case (name, sources) => tree.Unique(name, sources.map(tree.Ident))
  })

  val uniques = P(UNIQUE ~/ uniqueField.rep(1))

  val isAbstract = P(ABSTRACT.!)

  val supertypesOf = P(SUPERTYPE ~/ OF ~/ group(ONEOF ~/ nameList))

  val subtypeOf = P(SUBTYPE ~/ OF ~/ nameList)

  val entityDef = P(ENTITY ~/ name.! ~/ isAbstract.? ~ supertypesOf.? ~/ subtypeOf.? ~/ EOS ~/ fields ~/ derives.? ~/ inverses.? ~/ uniques.? ~/ wheres.? ~/ END_ENTITY ~/ EOS).map({
    case (name, abstracts, superstypes, subtypes, fields, derives, inverses, uniques, wheres) => tree.Entity(name, abstracts.isDefined, subtypes.getOrElse(Seq.empty), fields, derives.getOrElse(Seq.empty), inverses.getOrElse(Seq.empty), uniques.getOrElse(Seq.empty), wheres.getOrElse(Seq.empty))
  })

  val typeDef = P(TYPE ~/ name.! ~/ "=" ~/ fieldType ~/ EOS ~/ wheres.? ~/ END_TYPE ~/ EOS).map({
    case (name, tpe, wheres) => tree.Type(name, tpe, wheres.getOrElse(Seq.empty))
  })

  val localDef = P(name.!.rep(1, ",") ~ ":" ~/ fieldType ~/ (":=" ~/ condition).? ~/ EOS).map({
    case (names, tpe, expr) => names.map(tree.Local(_, tpe, expr))
  })

  val localDefs = P(LOCAL ~/ localDef.rep(1) ~/ END_LOCAL ~/ EOS).map(_.flatten)

  val argDef = P(name.!.rep(1, ",") ~ ":" ~/ fieldType).map({
    case (lst, tpe) => lst.map(tree.Argument(_, tpe))
  })

  val argDefs = P(group(argDef.rep(1, EOS))).map(_.flatten)

  val functionDef = P(FUNCTION ~/ name.!.map({ x => println(x); x }) ~/ argDefs ~ ":" ~/ fieldType ~ EOS ~/ localDefs.? ~/ functionBlock ~/ END_FUNCTION ~/ EOS).map({
    case (name, argList, tpe, locals, body) => tree.Function(name, argList, tpe, locals.getOrElse(Seq.empty), body)
  })

  val ruleDef = P(RULE ~/ name.! ~/ FOR ~/ group(fieldType.rep(1, COMA)) ~/ EOS ~/ localDefs.? ~ functionBlock.? ~ wheres.? ~ /*(!END_RULE ~ AnyChar).rep(0)
  ~ */ END_RULE ~/ EOS).map({
    case (name, types, locals, body, wheres) => tree.Rule(name, types, locals.getOrElse(Seq.empty), body.getOrElse(tree.EmptyTree), wheres.getOrElse(Seq.empty))
  })

  val element = P(entityDef | typeDef | functionDef | ruleDef)

  val schemaBody = P(element.rep(0))

  val schemaDef = P(SCHEMA ~/ name.! ~/ EOS ~/ schemaBody ~/ END_SCHEMA ~ EOS).map({
    case (name, body) => tree.Schema(name, body)
  })

  val file = P(header ~ schemaDef).map({
    case (header, schema) => schema
  })
}
