package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.circe.Json

object Playground:
  @main
  def run: Unit = {
    println(JsonEncoder(string, "foobar"))
    println(JsonDecoder(int, Json.fromInt(42)))
    println(JsonDecoder(int, Json.fromString("foobar")))

    val myCollection: Collection.Of[Primitive[Option[String]], Vector[Option[String]]] = string.optional.collection

    println(JsonEncoder(myCollection, Vector(Some("foo"), None, Some("bar"))))
    println(
      JsonDecoder(
        myCollection,
        Json.arr(Json.fromString("foo"), Json.Null, Json.fromInt(3), Json.arr(Json.fromString("lol")))
      )
    )
    println(JsonDecoder(int, Json.fromString("foobar")))

  }
