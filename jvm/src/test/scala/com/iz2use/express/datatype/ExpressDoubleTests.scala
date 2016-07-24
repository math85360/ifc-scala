package com.iz2use.express.datatype

import utest._

/*object ExpressDoubleTests extends TestSuite {
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
        val v = (Null: ExpressReal)
        assert(v != null)
        assert(v.value == None)
        'isEmpty{ assert(v.value.isEmpty) }
        //'orNull{ assert(v.orNull == null) }
        'getOrElse{ assert(v.getOrElse(0.0) == 0.0) }
      }
      '_50{
        val v = (50: ExpressReal)
        assert(v != null)
        assert(v.value == Some(50))
        'isDefined{ assert(v.value.isDefined) }
        //'orNull{ assert(v.orNull == false) }
        'getOrElse{ assert(v.getOrElse(0.0) == 50) }
        'getOrElse_0{ assert(v.getOrElse(0.0) == 50.0) }
      }
      'min50{
        val v = (-50.0: ExpressReal)
        assert(v != null)
        assert(v.value == Some(-50.0))
        'isDefined{ assert(v.value.isDefined) }
        //'orNull{ assert(v.orNull == true) }
        'getOrElse{ assert(v.getOrElse(0.0) == -50) }
        'getOrElse_0{ assert(v.getOrElse(0.0) == -50.0) }
      }
    }
  }
}*/