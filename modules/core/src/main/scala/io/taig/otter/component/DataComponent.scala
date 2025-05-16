package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Data

trait DataComponent[
    Collection[a] <: Value[a]: Invariant,
    Dictionary[a] <: Value[a]: Invariant,
    Nullable[a] <: Value[a]: Invariant,
    Primitive[a] <: Value[a],
    Sum[a] <: Value[a],
    Branch[_],
    Key[_],
    Value[_]
] extends BranchComponent.Primitive.String[Branch, Key, Value, Sum],
      CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent[Primitive],
      SumComponent[Sum, Branch]:
  object data:
    val any: Nullable[Data.Any] = value.nullable.imap(_.getOrElse(Data.Null)) {
      case Data.Null        => None
      case data: Data.Value => Some(data)
    }

    val number: Sum[Data.Number] =
      branch("bigDecimal", jBigDecimal) |
        branch("bigInterger", jBigInteger) |
        branch("long", long) |
        branch("int", int) |
        branch("float", float) |
        branch("double", double)

    val primitive: Sum[Data.Primitive] = number | branch("boolean", boolean) | branch("string", string)

    val value: Sum[Data.Value] = primitive | branch("object", obj) | branch("array", array)

    def obj[A <: Data.Any](codec: => Value[A]): Dictionary[Data.Object[A]] =
      dictionary.list(key.string, codec).imap(Data.Object[A])(_.values)

    val obj: Dictionary[Data.Object[Data.Any]] = obj(any)

    def array[A <: Data.Any](codec: => Value[A]): Collection[Data.Array[A]] =
      collection.vector(codec).imap(Data.Array[A])(_.values)

    val array: Collection[Data.Array[Data.Any]] = array(any)
