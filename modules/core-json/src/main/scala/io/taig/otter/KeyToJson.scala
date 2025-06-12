package io.taig.otter

object KeyToJson extends Translator[Key, Json]:
  override def translate[A](key: Key[A]): Json[A] = key match
    case key: Key.Primitive[A] => KeyPrimitiveToJsonPrimitive.translate(key)
    case Key.Constant(self)    => Json.Constant(self.mapK(KeyPrimitiveToJsonPrimitive))
    case Key.Enumeration(self) => Json.Enumeration(self.mapK(KeyPrimitiveToJsonPrimitive))
    case Key.Union(self)       => Json.Union(self.mapK(this))
