package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base

object Playground:
  @main
  def run = {
    val t = string.toTuple
    println(JsonEncoder(collection(t), Vector("foo", "bar", "baz")))
  }
