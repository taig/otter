package io.taig.otter.openapi

import io.taig.otter.http.Dsl.*
import io.taig.otter.http.Dsl.given

object Playground:
  val x: Schema[String] = ???
  val y: Primitive[String] = ???

  x.union
  val z: Union.Of[y.type, String] = y.union
  y.tpe
