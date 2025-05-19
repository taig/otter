package io.taig.otter.component
import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.schema.CollectionSchema
import io.taig.otter.schema.DictionarySchema
import io.taig.otter.schema.NullableSchema

trait DataComponent[
    Collection[a] <: Value[a],
    Constant[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using CollectionSchema[Collection, Value], DictionarySchema[Dictionary, Key, Value], NullableSchema[Nullable, Value]) extends CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent[Primitive],
      SumComponent[Constant, Record, Field, Key, Value],
      UnionComponent[Union, Value]:
  this: PrimitiveComponent.String[Value] =>

  object data:
    val any: Nullable[Data.Any] = value.nullable.imap(_.getOrElse(Data.Null)) {
      case Data.Null        => None
      case data: Data.Value => Some(data)
    }

    val number: Union[Data.Number] = jBigDecimal | jBigInteger | long | int | float | double

    val primitive: Union[Data.Primitive] = number | boolean | string

    val value: Union[Data.Value] = primitive | obj | array

    def obj[A <: Data.Any](codec: => Value[A]): Dictionary[Data.Object[A]] =
      dictionary.list(key.string, codec).imap(Data.Object[A])(_.values)

    val obj: Dictionary[Data.Object[Data.Any]] = obj(any)

    def array[A <: Data.Any](codec: => Value[A]): Collection[Data.Array[A]] =
      collection.vector(codec).imap(Data.Array[A])(_.values)

    val array: Collection[Data.Array[Data.Any]] = array(any)
