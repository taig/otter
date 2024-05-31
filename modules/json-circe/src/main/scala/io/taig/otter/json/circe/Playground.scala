package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base

object Playground:
  @main
  def run = {
    println(JsonEncoder(collection(string), Vector("foo", "bar", "baz")))
  }
