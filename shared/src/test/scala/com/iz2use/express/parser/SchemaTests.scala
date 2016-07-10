package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.io.Source
import scala.util._

object SchemaTests extends TestSuite {

  val tests = TestSuite {
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
}