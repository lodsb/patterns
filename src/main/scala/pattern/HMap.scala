// taken from
// https://github.com/kennknowles/scala-heterogeneous-map
// thanks to kenn knowles!
/*Copyright 2012 Kenneth Knowles

Licensed under the Apache License,
Version 2.0 (the "License"); you may not use this file except in compliance with the License.
You may obtain a copy of the License at
*/

               /*

trait HMap[TypedKey[_]] { self =>
  def get[T](key: TypedKey[T]) : Option[T]
  def put[T](key: TypedKey[T], value: T) : HMap[TypedKey]
}

object HMap {
  private class WrappedMap[TypedKey[_]](m: Map[TypedKey[_], AnyRef])
    extends HMap[TypedKey] {
    def get[T](key: TypedKey[T]) = m.get(key).asInstanceOf[Option[T]]
    def put[T](key: TypedKey[T], value: T) =
      new WrappedMap[TypedKey](m + (key -> value.asInstanceOf[AnyRef]))
  }

  def empty[TypedKey[_]] : HMap[TypedKey] = new WrappedMap[TypedKey](Map())
}



import java.net.URI

case class WithPhantom[T, Phantom: Manifest](v: T) {
  private val m = implicitly[Manifest[Phantom]]

  override def equals(other: Any) = other.isInstanceOf[WithPhantom[T, Phantom]] && {
    val otherPh = other.asInstanceOf[WithPhantom[T, Phantom]]
    (otherPh.m.erasure == this.m.erasure) && (otherPh.v == this.v)
  }

  override def hashCode = (v, implicitly[Manifest[Phantom]].hashCode).hashCode

  override def toString = "WithPhantom[%s](%s)".format(implicitly[Manifest[Phantom]].erasure.getName, v)
}

object WithPhantom {
  type TInt[T] = WithPhantom[Int, T]
  type TString[T] = WithPhantom[String, T]
  type TURI[T] = WithPhantom[URI, T]

  def TInt[T: Manifest](i: Int) = WithPhantom[Int, T](i)
  def TString[T: Manifest](str: String) = WithPhantom[String, T](str)
  def TURI[T: Manifest](uri: URI) = WithPhantom[URI, T](uri)
}*/