package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.util._

object ConditionTests extends TestSuite {

  val tests = TestSuite {
    val ifParser = ExpressParser.ifBlock

    'eq{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF Dim = 3 THEN END_IF;""")
    }

    'eqparens{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Dim = 3) THEN END_IF;""")
    }

    'exists{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF EXISTS(Axis1) THEN END_IF;""")
    }

    'lt{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF Factor < 0.0 THEN END_IF;""")
    }

    'ltparens{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Factor < 0.0) THEN END_IF;""")
    }

    'B{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF B THEN END_IF;""")
    }

    'if1{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Dim = 3) THEN 
      D1 := NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0]));
    END_IF;""")
    }

    'if2{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Dim = 3) THEN 
      D2 := IfcFirstProjAxis(D1, Axis1);
    END_IF;""")
    }

    'if3{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Dim = 3) THEN 
U  := [D2, IfcSecondProjAxis(D1, D2, Axis2), D1];
END_IF;""")
    }

    'if4{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Dim = 3) THEN 
D1 := NVL(IfcNormalise(Axis3), IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0,0.0,1.0]));
D2 := IfcFirstProjAxis(D1, Axis1);
U  := [D2, IfcSecondProjAxis(D1, D2, Axis2), D1];
END_IF;""")
    }

    'if5{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF (Factor < 0.0) THEN
U[2].DirectionRatios[1] := -U[2].DirectionRatios[1];
U[2].DirectionRatios[2] := -U[2].DirectionRatios[2];
END_IF;""")
    }

    'if6{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF EXISTS(Axis2) THEN
Factor := IfcDotProduct(Axis2, U[2]);
IF (Factor < 0.0) THEN
U[2].DirectionRatios[1] := -U[2].DirectionRatios[1];
U[2].DirectionRatios[2] := -U[2].DirectionRatios[2];
END_IF;
END_IF;""")
    }

    'if7{
      val Parsed.Success(value, successIndex) = ifParser.parse("""IF EXISTS(Axis2) THEN
D1 := IfcNormalise(Axis2);
U  := [IfcOrthogonalComplement(D1), D1];
U[1].DirectionRatios[1] := -U[1].DirectionRatios[1];
U[1].DirectionRatios[2] := -U[1].DirectionRatios[2];
ELSE
U := [IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([1.0, 0.0]), 
IfcRepresentationItem() || IfcGeometricRepresentationItem () || IfcDirection([0.0, 1.0])];
END_IF;""")
    }
  }
}