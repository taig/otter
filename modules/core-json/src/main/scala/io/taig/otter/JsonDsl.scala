package io.taig.otter

trait JsonDsl:
  val string = Json.Primitive.String.invariant.string()

  def field[A, B](name: A, key: => Json.Key[A], value: Json[B]): Json.Field[B] =
    Field.invariant[Json.Record, Json.Key, Json].apply(name, key, value)

  def field[A](name: String, value: Json[A]) =
    Field.invariant[Json.Record, Json.Key, Json].apply(name, key = string, value)

object JsonDsl extends JsonDsl
