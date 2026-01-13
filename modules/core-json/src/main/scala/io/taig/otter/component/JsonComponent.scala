package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent
    extends ConstantComponent[Json.Constant, Json.Primitive],
      ConstantComponent.Read[Json.Constant.Read, Json.Primitive.Read],
      ConstantComponent.Write[Json.Constant.Write, Json.Primitive.Write],
      FieldComponent[Json.Field, Json],
      FieldComponent.Read[Json.Field.Read, Json.Read],
      FieldComponent.Write[Json.Field.Write, Json.Write],
      PrimitiveComponent.Boolean[Json.Primitive.Boolean],
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

  object dictionary
      extends DictionaryComponent[Json.Dictionary, Json],
        DictionaryComponent.Read[Json.Dictionary.Read, Json.Read],
        DictionaryComponent.Write[Json.Dictionary.Write, Json.Write]

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
