package io.taig.otter

import cats.syntax.all.*

trait ViolationsDsl[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Sum[a] <: Value[a],
    Union[a] <: Value[a],
    Branch[_],
    Field[_],
    Key[_],
    Value[_]
](using
    Codec.Extension.Unionable[Value, Union],
    Codec.Dictionary[Dictionary, Key, Value],
    Codec.Field[Field, Key, Value, Record],
    Codec.Record[Record, Field],
    Codec.Union[Union, Value]
) extends CollectionDsl[Collection, Value],
      DictionaryDsl[Dictionary, Key, Value],
      FieldDsl.Primitive.String[Field, Key, Value, Record],
      ViolationDsl[Collection, Dictionary, Nullable, Primitive, Record, Sum, Union, Branch, Field, Key, Value]:

  val violations: Union[Violations] =
    val step: Key[Step] = key.parser(name = "step")(Step.parse(_).leftMap(_.show))(_.show)

    val root: Record[Violations.Root] = (
      field("values", dictionary.sortedMap(step, violations)) :*
        field("violations", collection.nonEmptyChain(violation))
    ).to

    val namespace: Dictionary[Violations.Namespace] = dictionary.nonEmptyMap(step, violations).to

    (root :+ namespace).to
