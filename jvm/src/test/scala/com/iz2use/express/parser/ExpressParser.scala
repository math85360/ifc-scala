package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.util._

object ExpressParserTests extends TestSuite {

  val tests = TestSuite {
    'entity{
      val entityParser = ExpressParser.entityDef

      'empty{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY Female;
END_ENTITY;""")
      }
      'abstract1{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY Person
   ABSTRACT SUPERTYPE OF (ONEOF (Male, Female));
END_ENTITY;
""")
      }

      'abstract2{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY Person
   ABSTRACT SUPERTYPE OF (ONEOF (Male, Female));
     name: STRING;
     mother: OPTIONAL Female;
     father: OPTIONAL Male;
END_ENTITY;
""")
      }

      'subtype{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY Female
   SUBTYPE OF (Person);
END_ENTITY;""")
      }

      'IfcCP{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY IfcCartesianPoint
 SUBTYPE OF (IfcPoint);
	Coordinates : LIST [1:3] OF IfcLengthMeasure;
 DERIVE
	Dim : IfcDimensionCount := HIINDEX(Coordinates);
 WHERE
	CP2Dor3D : HIINDEX(Coordinates) >= 2;
END_ENTITY;""")
      }
      'listOfList{
        val Parsed.Success(value, successIndex) = entityParser.parse("""ENTITY IfcCartesianPointList3D
 SUBTYPE OF (IfcCartesianPointList);
	CoordList : LIST [1:?] OF LIST [3:3] OF IfcLengthMeasure;
END_ENTITY;""")
      }
    }

    'schema{
      val fileParser = ExpressParser.file

      'empty{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA Family;
END_SCHEMA;""")
      }

      'empty2{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA Family;
 
END_SCHEMA;""")
      }

      'single{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA Family;
ENTITY Female;
END_ENTITY;

END_SCHEMA;""")
      }

      'double{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA Family;
ENTITY Female;
END_ENTITY;

ENTITY Male;
END_ENTITY;

END_SCHEMA;""")
      }

      'tree1{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA Family;

ENTITY Person
   ABSTRACT SUPERTYPE OF (ONEOF (Male, Female));
     name: STRING;
     mother: OPTIONAL Female;
     father: OPTIONAL Male;
END_ENTITY;

ENTITY Female
   SUBTYPE OF (Person);
END_ENTITY;

ENTITY Male
   SUBTYPE of (Person);
END_ENTITY;

END_SCHEMA;""")
      }
      'tree2{
        val Parsed.Success(value, successIndex) = fileParser.parse("""SCHEMA IFC4;
ENTITY IfcCableSegmentType
 SUBTYPE OF (IfcFlowSegmentType);
	PredefinedType : IfcCableSegmentTypeEnum;
 WHERE
	CorrectPredefinedType : (PredefinedType <> IfcCableSegmentTypeEnum.USERDEFINED) OR
                              ((PredefinedType = IfcCableSegmentTypeEnum.USERDEFINED) AND EXISTS(SELF\IfcElementType.ElementType));
END_ENTITY;

ENTITY IfcCartesianPoint
 SUBTYPE OF (IfcPoint);
	Coordinates : LIST [1:3] OF IfcLengthMeasure;
 DERIVE
	Dim : IfcDimensionCount := HIINDEX(Coordinates);
 WHERE
	CP2Dor3D : HIINDEX(Coordinates) >= 2;
END_ENTITY;

ENTITY IfcCartesianPointList
 ABSTRACT SUPERTYPE OF (ONEOF
	(IfcCartesianPointList3D))
 SUBTYPE OF (IfcGeometricRepresentationItem);
END_ENTITY;

ENTITY IfcCartesianPointList3D
 SUBTYPE OF (IfcCartesianPointList);
	CoordList : LIST [1:?] OF LIST [3:3] OF IfcLengthMeasure;
END_ENTITY;
END_SCHEMA;
""")
      }
    }

    'expr{
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

    }

    'condition{
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
}