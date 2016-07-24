package com.iz2use.express.datatype

import utest._

/*object ExpressBooleanTests extends TestSuite {
  val tests = this{
    /*'ExpressBoolean_apply{
      'null{
        val e = ExpressBoolean(null)
        assert(e != null)
        assert(e.value == None)
      }
    }*/
    'implicits{
      /*'null{
        val v = (null: ExpressBoolean)
        assert(v != null)
        assert(v.value == None)
        'isEmpty{ assert(v.value.isEmpty) }
        //'orNull{ assert(v.orNull == null) }
        'getOrElse{ assert(v.getOrElse(true) == true) }
      }*/
      'nothing{
        val v = (Null: ExpressBoolean)
        assert(v != null)
        assert(v.value == None)
        'isEmpty{ assert(v.value.isEmpty) }
        //'orNull{ assert(v.orNull == null) }
        'getOrElse{ assert(v.getOrElse(true) == true) }
      }
      'false{
        val v = (false: ExpressBoolean)
        assert(v != null)
        assert(v.value == Some(false))
        'isDefined{ assert(v.value.isDefined) }
        //'orNull{ assert(v.orNull == false) }
        'getOrElseFalse{ assert(v.getOrElse(false) == false) }
        'getOrElseTrue{ assert(v.getOrElse(true) == false) }
      }
      'true{
        val v = (true: ExpressBoolean)
        assert(v != null)
        assert(v.value == Some(true))
        'isDefined{ assert(v.value.isDefined) }
        //'orNull{ assert(v.orNull == true) }
        'getOrElseFalse{ assert(v.getOrElse(false) == true) }
        'getOrElseTrue{ assert(v.getOrElse(true) == true) }
      }
    }
  }
}*/