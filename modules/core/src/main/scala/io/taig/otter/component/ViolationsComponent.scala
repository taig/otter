package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.syntax.InvariantSyntax.*

trait ViolationsComponent[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a]: Invariant,
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a]: Invariant,
    Sum[a] <: Value[a],
    Union[a] <: Value[a]: Invariant,
    Branch[_],
    Field[_],
    Key[_],
    Value[_]
] extends CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      FieldComponent.Primitive.String[Field, Key, Value],
      UnionComponent[Union, Value],
      ViolationComponent[Collection, Dictionary, Nullable, Primitive, Record, Sum, Union, Branch, Field, Key, Value]:

  val violations: Union[Violations] =
    val step: Key[Step] = key.parser(name = "step")(Step.parse(_).leftMap(_.show))(_.show)

    val root: Record[Violations.Root] = (
      field("values", dictionary.sortedMap(step, violations)) :*
        field("violations", collection.nonEmptyChain(violation))
    ).to

    val namespace: Dictionary[Violations.Namespace] = dictionary.nonEmptyMap(step, violations).to

    (root :+ namespace).to
