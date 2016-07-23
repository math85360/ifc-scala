package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import java.io.Closeable
import Implicits._

object ScalaTransformer {
  def using[A <: Closeable, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(path: String, data: String): Unit =
    using(new FileWriter(path))(_.write(data))

  def appendToFile(path: String, data: String): Unit =
    using(new PrintWriter(new FileWriter(path, true)))(_.println(data))

  def transformSchema(schema: tree.Schema) = {
    val context = Context(schema, NL = "\r\n") + schema
    val path = s"../ifc4test/src/main/scala/express/${schema.name.toLowerCase}"
    (
      new File(path)).mkdirs()

    schema.definedTypes.foreach({ tpe =>
      implicit val ctx = context + tpe
      val content = (tpe match {
        case e: tree.Entity => e.scalaCode
        case e: tree.Function => e.scalaCode
        case e: tree.Rule => e.scalaCode
        case e: tree.Type => e.scalaCode
      })
      writeToFile(s"$path/${tpe.name}.scala", content)
    })
  }

}

