package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.JsonDiscriminator
import io.taig.otter.component.JsonComponent.*

enum Tagged:
  case Circle(radius: Int)
  case Code(value: String)
  case Numbers(values: List[Int])
  case None

object Tagged:
  val nested: Json.Union[Tagged] = (
    branch.nested("circle", field("radius", int).toRecord.to[Tagged.Circle]) :+
      branch.nested("code", string).to[Tagged.Code] :+
      branch.nested("numbers", collection.list(int)).to[Tagged.Numbers] :+
      branch.nested("none", TNil.to[Tagged.None.type])
  ).to[Tagged]

  val merged: Json.Union[Tagged] = (
    branch.merged("circle", field("radius", int).toRecord.to[Tagged.Circle]) :+
      branch.merged("code", field("value", string).toRecord).to[Tagged.Code] :+
      branch.merged("numbers", field("values", collection.list(int)).toRecord).to[Tagged.Numbers] :+
      branch.merged("none", RNil.to[Tagged.None.type])
  ).to[Tagged]

  val custom: Json.Union[Either[String, Unit]] = (
    branch.nested("code", string, JsonDiscriminator.Nested("kind", "data")) :+
      branch.nested("none", TNil, JsonDiscriminator.Nested("kind", "data"))
  )

  // These compact documents preserve the legacy discriminator spelling and member order.
  val nestedDocuments: List[(Tagged, String)] = List(
    Tagged.Circle(1) -> """{"type":"circle","value":{"radius":1}}""",
    Tagged.Code("ABC") -> """{"type":"code","value":"ABC"}""",
    Tagged.Numbers(List(1, 2)) -> """{"type":"numbers","value":[1,2]}""",
    Tagged.None -> """{"type":"none"}"""
  )

  val mergedDocuments: List[(Tagged, String)] = List(
    Tagged.Circle(1) -> """{"type":"circle","radius":1}""",
    Tagged.Code("ABC") -> """{"type":"code","value":"ABC"}""",
    Tagged.Numbers(List(1, 2)) -> """{"type":"numbers","values":[1,2]}""",
    Tagged.None -> """{"type":"none"}"""
  )
