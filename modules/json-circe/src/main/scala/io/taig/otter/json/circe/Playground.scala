package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter.Plain.given
import io.taig.otter as Base

object Playground:
  @main
  def run = {
    val myStr: Primitive[Option[String]] = string.optional
    val t = myStr.toTuple
    println(JsonEncoder(collection(t), Vector(Some("foo"), None, Some("bar"), Some("baz"))))
  }
