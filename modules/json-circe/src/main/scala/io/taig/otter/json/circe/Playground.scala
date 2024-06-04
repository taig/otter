package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter.Plain.given
import io.taig.otter as Base
import cats.syntax.all.*

object Playground:
  @main
  def run = {
    println(
      JsonEncoder(string.optional.collection.vector, (None +: Vector("yolo", "foo", "bar").map(_.some)) ++ Vector(None))
    )
  }
