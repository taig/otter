package io.taig.otter

import cats.syntax.all.*

object TypescriptPrinter:
  def apply(codec: Codec[?, ?]): String = codec match
    case codec: Primitive[?, ?] => apply(codec)

  def apply(data: Data.Primitive): String = data match
    case Data.String(value)  => s"\"$value\""
    case Data.Boolean(true)  => "true"
    case Data.Boolean(false) => "false"
    case Data.Number(value)  => String.valueOf(value)

  def apply(codec: Primitive[?, ?]): String = codec.tpe match
    case Type.Boolean => "boolean"
    case Type.Number  => "number"
    case Type.String  => "string"

  def apply(enumeration: Enumeration[?, ?]): String =
    enumeration.data.map(apply).mkString_(" | ")
