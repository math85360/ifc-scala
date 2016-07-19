package com.iz2use.express.parser

import fastparse.all._
import utest._
import scala.util._

object EntityTests extends TestSuite {

  val tests = TestSuite {
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
}