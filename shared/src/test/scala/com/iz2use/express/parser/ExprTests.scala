package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.io.Source
import scala.util._

object ExprTests extends TestSuite {

  val tests = TestSuite {
    val exprParser = ExpressParser.condition
    'd1{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem()""")
      println(value)
    }
    'd2{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcGeometricRepresentationItem ()""")
      println(value)
    }
    'd3{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem() || IfcGeometricRepresentationItem ()""")
      println(value)
    }
    'd4{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0])""")
      println(value)
    }
    'd5{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0]))""")
      println(value)
    }
    'd6{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem ())""")
      println(value)
    }
    'd7{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem())""")
      println(value)
    }
  }
}