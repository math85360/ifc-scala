package com.iz2use.express.transform.toscala

import com.iz2use.express.parser._
import com.iz2use.express.tree
import fastparse.all._
import utest._
import scala.io.Source
import scala.util._

object ScalaTransformerTests extends TestSuite {

  val tests = TestSuite {
    'schema{
      val fileParser = ExpressParser.file
      'file{
        val fileContent = Source.fromFile("IFC4.exp").getLines().mkString
        fileParser.parse(fileContent) match {
          case Parsed.Success(schema: tree.Schema, _) =>
            ScalaTransformer.transformSchema(schema)
        }
      }
    }
  }
}