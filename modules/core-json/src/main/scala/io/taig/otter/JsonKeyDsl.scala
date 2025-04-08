package io.taig.otter

trait JsonKeyDsl
    extends // ConstantDsl.Primitive.String[Json.Key.Constant, Json.Key.Primitive],
      //EnumerationDsl[Json.Key.Enumeration, Json.Key.Primitive],
      PrimitiveDsl.String[Json.Key.Primitive]:
      //UnionDsl.Untagged[Json.Key.Union, Json.Key]:
  // override protected def fromConstant[A](self: Constant[Json.Key.Primitive, A]): Json.Key.Constant[A] =
  //   Json.Key.Constant(self)
  // override protected def fromEnumeration[A](self: Enumeration[Json.Key.Primitive, A]): Json.Key.Enumeration[A] =
  //   Json.Key.Enumeration(self)
  override protected def fromPrimitiveString[A](self: Primitive.String[A]): Json.Key.Primitive[A] =
    Json.Key.Primitive(self)
  // override protected def fromUnionUntagged[A](self: Union.Untagged[Json.Key, A]): Json.Key.Union[A] =
  //   Json.Key.Union(self)

object JsonKeyDsl extends JsonKeyDsl