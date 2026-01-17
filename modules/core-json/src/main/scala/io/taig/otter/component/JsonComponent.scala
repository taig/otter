package io.taig.otter.component

import io.taig.otter.Json
import io.taig.enumeration.ext.Mapping

trait JsonComponent
    extends PrimitiveComponent.Boolean[Json.Primitive.Boolean],
      PrimitiveComponent.Coerce.Boolean[Json.Primitive.Coerce.Boolean, Json.Primitive.Boolean],
      PrimitiveComponent.Coerce.Boolean.Read[Json.Primitive.Coerce.Boolean.Read, Json.Primitive.Boolean.Read],
      PrimitiveComponent.Coerce.Boolean.Write[Json.Primitive.Coerce.Boolean.Write, Json.Primitive.Boolean.Write],
      PrimitiveComponent.Coerce.Number[Json.Primitive.Coerce.Number, Json.Primitive.Number],
      PrimitiveComponent.Coerce.Number.Read[Json.Primitive.Coerce.Number.Read, Json.Primitive.Number.Read],
      PrimitiveComponent.Coerce.Number.Write[Json.Primitive.Coerce.Number.Write, Json.Primitive.Number.Write],
      PrimitiveComponent.Coerce.Text[Json.Primitive.Coerce.Text, Json.Primitive.Text],
      PrimitiveComponent.Coerce.Text.Read[Json.Primitive.Coerce.Text.Read, Json.Primitive.Text.Read],
      PrimitiveComponent.Coerce.Text.Write[Json.Primitive.Coerce.Text.Write, Json.Primitive.Text.Write],
      PrimitiveComponent.Number[Json.Primitive.Number],
      PrimitiveComponent.Text[Json.Primitive.Text],
      PrimitiveComponent.Text.Read[Json.Primitive.Text.Read],
      PrimitiveComponent.Text.Write[Json.Primitive.Text.Write],
      RecordComponent[Json.Record, Json.Field],
      TupleComponent[Json.Tuple, Json]:
  object collection
      extends CollectionComponent[Json.Collection, Json],
        CollectionComponent.Read[Json.Collection.Read, Json.Read],
        CollectionComponent.Write[Json.Collection.Write, Json.Write]

  object constant
      extends ConstantComponent[Json.Constant, Json.Primitive],
        ConstantComponent.Write[Json.Constant.Write, Json.Primitive.Write]

  object dictionary
      extends DictionaryComponent[Json.Dictionary, Json],
        DictionaryComponent.Read[Json.Dictionary.Read, Json.Read],
        DictionaryComponent.Write[Json.Dictionary.Write, Json.Write]

  object enumeration
      extends EnumerationComponent[Json.Enumeration, Json.Primitive],
        EnumerationComponent.Write[Json.Enumeration.Write, Json.Primitive.Write]

  object field
      extends FieldComponent[Json.Field, Json],
        FieldComponent.Read[Json.Field.Read, Json.Read],
        FieldComponent.Write[Json.Field.Write, Json.Write]

object JsonComponent extends JsonComponent

object Playground:
  val json = JsonComponent

  val p: Json.Primitive[String] = ???
  val pr: Json.Primitive.Read[Int] = ???
  val pw: Json.Primitive.Write[Long] = ???

  val t: Json.Tuple[String] = ???
  val tr: Json.Tuple.Read[Int] = ???
  val tw: Json.Tuple.Write[Long] = ???

  val _: Json.Tuple[Unit] = json.TNil
  val _: Json.Tuple.Read[Unit] = json.TNil
  val _: Json.Tuple.Write[Unit] = json.TNil

  val _: Json.Tuple[String] = p.toTuple
  val _: Json.Tuple.Read[Int] = pr.toTuple
  val _: Json.Tuple.Write[Long] = pw.toTuple

  val _: Json.Tuple[(String, String)] = p :* p
  val _: Json.Tuple.Read[(String, Int)] = p :* pr
  val _: Json.Tuple.Write[(String, Long)] = p :* pw

  val _: Json.Tuple[(String, String, String)] = p :* p :* p
  val _: Json.Tuple.Read[(String, Int, String)] = p :* pr :* p
  val _: Json.Tuple.Write[(String, Long, String)] = p :* pw :* p

  val _: Json.Tuple[(String, String)] = t :* p
  val _: Json.Tuple.Read[(String, Int)] = t :* pr
  val _: Json.Tuple.Read[(Int, Int)] = tr :* pr
  val _: Json.Tuple.Read[(Int, String)] = tr :* p
  val _: Json.Tuple.Write[(Long, Long)] = tw :* pw
  val _: Json.Tuple.Write[(Long, String)] = tw :* p

  val _ = json.field(name = "foo", p)
  val _ = json.field(name = "bar", pr)
  val _ = json.field(name = "baz", pw)

  val _: Json.Collection[List[String]] = json.collection.list(p)
  val _: Json.Collection.Read[List[Int]] = json.collection.list(pr)
  val _: Json.Collection.Write[List[Long]] = json.collection.list(pw)

  val _: Json.Constant[String] = json.constant(p, value = "constant")

  val _: Json.Enumeration["foobar"] = json.enumeration(p, Mapping.constant("foobar"))
