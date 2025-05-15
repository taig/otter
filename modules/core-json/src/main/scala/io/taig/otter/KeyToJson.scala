package io.taig.otter

def keyToJson[A](key: Json.Key[A]): Json[A] = key match
  case Json.Key.Constant(self) =>
    Json.Constant(self.mapK[Json.Key.Primitive, Json.Primitive]([A] => (self: Json.Key.Primitive[A]) => Json.Primitive(self.self)))
  case Json.Key.Enumeration(self) =>
    Json.Enumeration(self.mapK[Json.Key.Primitive, Json.Primitive]([A] => (self: Json.Key.Primitive[A]) => keyPrimitiveToJsonPrimitive(self)))
  case Json.Key.Primitive(self) => Json.Primitive(self)
  case Json.Key.Union(self) => Json.Union(self.mapK[Json.Key, Json]([A] => (self: Json.Key[A]) => keyToJson(self)))

def keyPrimitiveToJsonPrimitive[A](key: Json.Key.Primitive[A]): Json.Primitive[A] = Json.Primitive(key.self)