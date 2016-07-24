package com.iz2use.express.datatype

import scala.reflect.ClassTag

class Value[T](val originalValue: Any = null, val definitive: Boolean = true) {
  private var currentValue: Any = originalValue

  def apply()(implicit getter: Getter[T]): T = getter(this)

  def :=(value: Any)(implicit setter: Setter[T]) = setter(this, value)

  def ? : Boolean = currentValue != null

  def collect[B](pf: PartialFunction[Any, B]): B = if (pf.isDefinedAt(currentValue)) pf(currentValue) else throw new RuntimeException("can not convert this one")

  def _set(newValue: Any) = if (!definitive) {
    currentValue = newValue
  }

  def _unset() = _set(null)

  def isChanged() = !definitive && (currentValue != originalValue)

}

class VariableValue[T](originalValue: Any = null) extends Value[T](originalValue, false) {
  
}

object Value {
  implicit object stringGetter extends Format[String] {
    override def apply(value: Value[String]): String = value collect {
      case x: String => x
    }
    override def apply(value: Value[String], newValue: Any) = newValue match {
      case x: String => value._set(x)
    }
  }
  implicit object booleanGetter extends Getter[Boolean] {
    override def apply(value: Value[Boolean]): Boolean = value collect {
      case x: Boolean => x
    }
  }
  implicit object doubleGetter extends Getter[Double] {
    override def apply(value: Value[Double]): Double = value collect {
      case x: Double => x
    }
  }
  implicit object longGetter extends Getter[Long] {
    override def apply(value: Value[Long]): Long = value collect {
      case x: Long => x
    }
  }
  implicit object intGetter extends Format[Int] {
    override def apply(value: Value[Int]): Int = value collect {
      case x: Int => x
    }
    override def apply(value: Value[Int], newValue: Any) = newValue match {
      case x: Int => value._set(x)
    }
  }

  def apply[T](v: Any): Value[T] = new Value(v)
}

trait Getter[T] {
  def apply(value: Value[T]): T
}

trait Setter[T] {
  def apply(value: Value[T], newValue: Any): Unit
}
abstract class Format[T] extends Getter[T] with Setter[T]
/*
abstract class ExpressType[T](private[datatype] var value: Option[T]) {
  assert(value != null, "ExpressType accept only non-null value because it's an option")
  //if (value == null) value = None

  @inline def getOrElse[B](default: => B)(implicit ev: T <:< B): B = if (value.isEmpty) default else value.get

  @inline def isDefined: Boolean = value.isDefined

  //@inline def orNull[T1 >: T](implicit ev: Null <:< T1): T1 = value.orNull
  //@inline def orNothing: T = if (value.isEmpty) ENull else value.get

  /*  @inline def orElse[B](ifNull: => B)(implicit ev: T <:< B): B = {
    value match {
      case Some(v) => v
      case None => ifNull
    }
  }*/
}

//class Nothing[T] extends ExpressType[scala.Nothing](None)
case object Null extends ExpressType[Nothing](None)

abstract class ExpressSeqLikeType[T](value: Option[T]) extends ExpressType[T](value) {

}

abstract class ExpressPrimitiveType[T](value: Option[T]) extends ExpressType[T](value) {
}

abstract class ExpressObject[A, B <: ExpressType[A]] {
  @inline implicit def nativeToExpress(native: A): B = if (native == null) Null else apply(native)

  //@inline implicit def nativeToExpress[T](native: T)(implicit ev: T =:= Null): B = apply(None)
  //@inline implicit def nativeToExpress(native: Null): B = apply(None)
  @inline implicit def nativeToExpress(native: ExpressType[Nothing]): B = apply(None)

  //@inline implicit def expressToNative[T1 >: A](express: B)(implicit ev: Null <:< T1): T1 = express.orNull
  //@inline implicit def expressToNative(express: B): Any = express.orNull

  @inline def apply(native: Option[A]): B

  @inline final def apply(native: A): B = if (native == null) Null else apply(Option(native))

  //@inline final def apply[T](native: T)(implicit ev: T =:= Null): B = apply(None)
}

class ExpressBoolean(value: Option[Boolean]) extends ExpressPrimitiveType[Boolean](value)
object ExpressBoolean extends ExpressObject[Boolean, ExpressBoolean] {
  override def apply(native: Option[Boolean]): ExpressBoolean = if (native == null) Null else new ExpressBoolean(native)
}

abstract class ExpressNumeric[T](value: Option[T]) extends ExpressPrimitiveType[T](value)
/*abstract class ExpressNumericObject[A, B <: ExpressNumeric[A]] extends ExpressObject[A, B] {
  trait ExpressIntegral extends Integral[A] {
    
  }
  implicit object ExpressIntegral extends ExpressIntegral
}*/

class ExpressReal(value: Option[Double]) extends ExpressNumeric[Double](value)
object ExpressReal extends ExpressObject[Double, ExpressReal] {
  override def apply(native: Option[Double]): ExpressReal = if (native == null) Null else new ExpressReal(native)
  /*trait ExpressIntegral extends Integral[Double] {
  }
  implicit object ExpressIntegral extends ExpressIntegral*/
}
class ExpressInt(value: Option[Int]) extends ExpressNumeric[Int](value)
object ExpressInt extends ExpressObject[Int, ExpressInt] {
  override def apply(native: Option[Int]): ExpressInt = if (native == null) Null else new ExpressInt(native)
}
class ExpressLong(value: Option[Long]) extends ExpressNumeric[Long](value)
object ExpressLong extends ExpressObject[Long, ExpressLong] {
  override def apply(native: Option[Long]): ExpressLong = if (native == null) Null else new ExpressLong(native)
}

class ExpressString(value: Option[String]) extends ExpressPrimitiveType[String](value)
object ExpressString extends ExpressObject[String, ExpressString] {
  override def apply(native: Option[String]): ExpressString = if (native == null) Null else new ExpressString(native)
}

class ExpressArray[T](SELF: Array[T]) extends ExpressSeqLikeType[Array[T]](Option(SELF)) {

}

class ExpressList[T](SELF: List[T]) extends ExpressSeqLikeType[List[T]](Option(SELF)) {

}

class ExpressSet[T](SELF: Set[T]) extends ExpressSeqLikeType[Set[T]](Option(SELF)) {

}
*/