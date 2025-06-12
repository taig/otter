package io.taig.otter

object KeyPrimitiveToJsonPrimitive extends Translator[Key.Primitive, Json.Primitive]:
  override def translate[A](key: Key.Primitive[A]): Json.Primitive[A] = key match
    case Key.Primitive.Boolean(self) => Json.Primitive(self)
    case Key.Primitive.Number(self) => Json.Primitive(self)
    case Key.Primitive.String(self) => Json.Primitive(self.mapK(this))