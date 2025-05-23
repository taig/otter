package io.taig.otter

import cats.data.NonEmptyChain
import cats.data.Chain
import cats.syntax.all.*

private[otter] def zodObject(fields: Chain[(String, String)]): String = fields match
  case Chain.nil           => "z.object({})"
  case Chain((key, value)) => s"z.object({ $key: $value })"
  case fields =>
    s"""z.object({
       |${indent(fields.map((key, value) => s"$key: $value").mkString_(",\n"))}
       |})""".stripMargin

private[otter] def zodUnion(values: NonEmptyChain[String]): String =
  if values.isEmpty then values.head else s"z.union([${values.mkString_(", ")}])"
