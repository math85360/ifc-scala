package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.io.Source
import scala.util._

object ExpressParserTestsJVM extends TestSuite {

  val tests = TestSuite {
    'schema{
      val fileParser = ExpressParser.file
      'file{
        val fileContent = Try(Source.fromFile("IFC4.exp").getLines().mkString)
        fileContent match {
          case Success(data) =>
            val Parsed.Success(value, successIndex) = fileParser.parse(data)
        }
      }
    }
  }
}