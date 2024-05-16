package io.taig.otter.json.circe

import io.taig.otter.Type
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.taig.otter.Fix
import cats.Id
import io.circe.Json
import cats.data.Chain
import io.circe.syntax.*

object Playground:
  @main
  def run: Unit = {
    val string: Primitive[String] = Base.Schema.Root(Base.Primitive.Root(Type.String))
    val tuple: Tuple[String] = Base.Schema.Root(Base.Tuple.One(string))

    println(JsonEncoder(tuple, "hello"))
  }
