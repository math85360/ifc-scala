package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.util._
import com.iz2use.express.tree

object ExprTests extends TestSuite {

  val tests = TestSuite {
    val exprParser = ExpressParser.condition
    'd1{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem()""")
    }
    'd2{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcGeometricRepresentationItem ()""")
    }
    'd3{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem() || IfcGeometricRepresentationItem ()""")
    }
    'd4{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0])""")
    }
    'd5{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0]))""")
    }
    'd6{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem ())""")
    }
    'd7{
      val Parsed.Success(value, successIndex) = exprParser.parse("""NVL(IfcNormalise(Axis3), IfcRepresentationItem())""")
    }
    'select0{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcNormalise\IfcVector""")
      assert(value == tree.Super(tree.Ident("IfcNormalise"), tree.Ident("IfcVector")))
    }
    'select1{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcNormalise\IfcVector.Orientation""")
      assert(value == tree.Select(tree.Super(tree.Ident("IfcNormalise"), tree.Ident("IfcVector")), tree.Ident("Orientation")))
    }
    'select2{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcNormalise(IfcCrossProduct)\IfcVector""")
      assert(value == tree.Super(tree.Apply(tree.Ident("IfcNormalise"), Array(tree.Ident("IfcCrossProduct"))), tree.Ident("IfcVector")))
    }
    'select3{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcNormalise(IfcCrossProduct)\IfcVector.Orientation""")
      assert(value == tree.Select(tree.Super(tree.Apply(tree.Ident("IfcNormalise"), Array(tree.Ident("IfcCrossProduct"))), tree.Ident("IfcVector")), tree.Ident("Orientation")))
    }
    'select4{
      val Parsed.Success(value, successIndex) = exprParser.parse("""IfcNormalise(IfcCrossProduct(D1,D2))\IfcVector.Orientation""")
      assert(value == tree.Select(tree.Super(tree.Apply(tree.Ident("IfcNormalise"), Array(tree.Apply(tree.Ident("IfcCrossProduct"), Array(tree.Ident("D1"), tree.Ident("D2"))))), tree.Ident("IfcVector")), tree.Ident("Orientation")))
    }
  }
}