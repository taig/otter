package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter.Plain.given
import io.circe.Json

object Playground:
  @main
  def run: Unit = {
    println(JsonEncoder(string, "foobar"))
    println(JsonDecoder(int, Json.fromInt(42)))
    println(JsonDecoder(int, Json.fromString("foobar")))

    println(JsonEncoder(string.collection, Vector("foo", "bar")))
    println(JsonDecoder(int, Json.fromString("foobar")))
  }
