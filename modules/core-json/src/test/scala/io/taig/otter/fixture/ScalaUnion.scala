package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*

object ScalaUnion:
  final case class Text(value: String)
  final case class Number(value: Int)
  final case class Flag(value: Boolean)

  type Value = Text | Number | Flag

  val plain: Json.Union[Value] = (
    branch("text", string.to[Text]).toUnion :+
      branch("number", int.to[Number]) :+
      branch("flag", boolean.to[Flag])
  ).to[Value]

  val nested: Json.Union[Value] = (
    branch.nested("text", string.to[Text]) :+
      branch.nested("number", int.to[Number]) :+
      branch.nested("flag", boolean.to[Flag])
  ).to[Value]

  val merged: Json.Union[Value] = (
    branch.merged("text", field("value", string).toRecord.to[Text]) :+
      branch.merged("number", field("value", int).toRecord.to[Number]) :+
      branch.merged("flag", field("value", boolean).toRecord.to[Flag])
  ).to[Value]

  val documents: List[(Value, String)] = List(
    Text("x") -> "\"x\"",
    Number(2) -> "2",
    Flag(true) -> "true"
  )

  val nestedDocuments: List[(Value, String)] = List(
    Text("x") -> """{"type":"text","value":"x"}""",
    Number(2) -> """{"type":"number","value":2}""",
    Flag(true) -> """{"type":"flag","value":true}"""
  )

  val mergedDocuments: List[(Value, String)] = List(
    Text("x") -> """{"type":"text","value":"x"}""",
    Number(2) -> """{"type":"number","value":2}""",
    Flag(true) -> """{"type":"flag","value":true}"""
  )
