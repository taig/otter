package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.Id

object Playground:
  @main
  def run = {
    val arr: Schema.Writer[Vector[String]] = Base.Schema.Required.Root(Base.Collection.Root(string))
    println(JsonEncoder(arr, Vector("asdf", "lol")))
  }
