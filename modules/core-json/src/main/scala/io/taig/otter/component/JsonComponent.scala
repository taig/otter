package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent
    extends TupleComponent[Json.Tuple, Json],
      PrimitiveComponent.Boolean[Json.Primitive.Boolean],
      PrimitiveComponent.Number[Json.Primitive.Number],
      PrimitiveComponent.Text[Json.Primitive.Text, Json.Primitive.Text.Read, Json.Primitive.Text.Write]

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

  // val _: Json.Tuple[(String, String, String)] = p :* p :* p
  // val _: Json.Tuple.Read[(String, Int, String)] = p :* pr :* p
  // val _: Json.Tuple.Write[(String, Long, String)] = p :* pw :* p

  val _: Json.Tuple[(String, String)] = t :* p
  val _: Json.Tuple.Read[(String, Int)] = t :* pr
  // val _: Json.Tuple.Read[(Int, Int)] = tr :* pr
  // val _: Json.Tuple.Read[(Int, String)] = tr :* p
  // val _: Json.Tuple.Write[(Long, Long)] = tw :* pw
  // val _: Json.Tuple.Write[(Long, String)] = tw :* p
