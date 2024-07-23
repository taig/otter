package io.taig.otter

type Format[A] <: Data = A match
  // case Primitive[?]     => Data.Primitive
  case Dictionary[o, ?] => Data.Object[Format[o]] | Data.Null.type
  // case Product[o, ?]    => Data.Array[Format[o]]
  case Collection[o, ?]     => Data.Array[Format[o]] | Data.Null.type
  case Value.Required[o, ?] => Format[o]
  case Value[o, ?] => Format[o] | Data.Null.type
  case Codec.Required[?, ?] => Data.Value
  case Codec[?, ?]          => Data
