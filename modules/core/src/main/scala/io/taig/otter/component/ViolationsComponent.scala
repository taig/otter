package io.taig.otter.component
import cats.syntax.all.*
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.schema.DictionarySchema
import io.taig.otter.schema.FieldSchema
import io.taig.otter.schema.RecordSchema
import io.taig.otter.schema.UnionSchema

trait ViolationsComponent[
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
    DictionarySchema[Dictionary, Key, Value],
    FieldSchema[Field, Key, Value],
    RecordSchema[Record, Field],
    UnionSchema[Union, Value]
) extends CollectionComponent[Collection, Value],
      DictionaryComponent[Dictionary, Key, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      UnionComponent[Union, Value],
      ViolationComponent[Collection, Constant, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value]:
  this: PrimitiveComponent.String[Value] =>

  val violations: Union[Violations] =
    val step: Key[Step] = key.parser(name = "step")(Step.parse(_).leftMap(_.show))(_.show)

    val root: Record[Violations.Root] = (
      field("values", dictionary.sortedMap(step, violations)) :*
        field("violations", collection.nonEmptyChain(violation))
    ).to

    val namespace: Dictionary[Violations.Namespace] = dictionary.nonEmptyMap(step, violations).to

    (root :+ namespace).to
