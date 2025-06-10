package io.taig.otter.component

import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*

trait DataComponent[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Primitive[a] <: Value[a],
    Union[a] <: Value[a],
    Key[_],
    Value[_]
](using
    SchemaInvariant.Nullable[Value, Value],
    SchemaInvariant.Unionable[Value, Union],
    SchemaInvariant[Collection],
    SchemaInvariant[Dictionary],
    UnionSchemaInvariant[Union, Value]
) extends CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      PrimitiveComponent[Primitive]:
  def key: PrimitiveComponent.String[Key]

  object data:
    val number: Value[Data.Number] = jBigDecimal | jBigInteger | long | int | float | double

    val primitive: Value[Data.Primitive] = (number | boolean | string).name("Data.Primitive")

    def obj[A <: Data](schema: => Value[A]): Dictionary[Data.Object[A]] =
      dictionary.list(key.string, schema).imap(Data.Object[A])(_.values)

    val obj: Dictionary[Data.Object[Data]] = obj(any).name("Data.Object")

    def array[A <: Data](schema: => Value[A]): Collection[Data.Array[A]] =
      collection.vector(schema).imap(Data.Array[A])(_.values).name("Data.Collection")

    val array: Collection[Data.Array[Data]] = array(any)

    val value: Union[Data.Value] = (primitive | obj | array).name("Data.Value")

    val any: Value[Data] = value.nullable
      .imap(_.getOrElse(Data.Null)) {
        case Data.Null        => None
        case data: Data.Value => Some(data)
      }
      .name("Data")
