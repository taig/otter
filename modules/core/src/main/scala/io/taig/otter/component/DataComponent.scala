package io.taig.otter.component

import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.Keys.*
import io.taig.otter.operation.EnrichedCollectionSchemaInvariant
import io.taig.otter.operation.EnrichedDictionarySchemaInvariant
import io.taig.otter.operation.EnrichedNullableSchemaInvariant
import io.taig.otter.operation.EnrichedSchemaInvariant

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
](using
    EnrichedCollectionSchemaInvariant[Collection, Value],
    EnrichedDictionarySchemaInvariant[Dictionary, Key, Value],
    EnrichedNullableSchemaInvariant[Nullable, Value],
    EnrichedSchemaInvariant[Union]
) extends CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      NullableComponent[Nullable, Value],
      PrimitiveComponent[Primitive],
      SumComponent[Constant, Record, Field, Key, Value],
      UnionComponent[Union, Value]:
  this: PrimitiveComponent.String[Value] =>

  object data:
    val number: Value[Data.Number] = jBigDecimal | jBigInteger | long | int | float | double

    val primitive: Value[Data.Primitive] = (number | boolean | string).metadata(name, "Primitive")

    def obj[A <: Data.Any](schema: => Value[A]): Dictionary[Data.Object[A]] =
      dictionary.list(key.string, schema).imap(Data.Object[A])(_.values)

    val obj: Dictionary[Data.Object[Data.Any]] = obj(any).metadata(name, "Object")

    def array[A <: Data.Any](schema: => Value[A]): Collection[Data.Array[A]] =
      collection.vector(schema).imap(Data.Array[A])(_.values).metadata(name, "Collection")

    val array: Collection[Data.Array[Data.Any]] = array(any)

    val value: Union[Data.Value] = (primitive | obj | array).metadata(name, "Value")

    val any: Nullable[Data.Any] = value.nullable
      .imap(_.getOrElse(Data.Null)) {
        case Data.Null        => None
        case data: Data.Value => Some(data)
      }
      .metadata(name, "Any")
