package io.taig.otter

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.Id

object StringEncoder:
  def apply[A](schema: Value.Required.Writer[A], a: A) = schema match
    case schema: Primitive.Required.Writer[A]   => "xxx"
    case schema: Union.Value.Required.Writer[A] => "haha"
    case schema: Enumeration.Required.Writer[A] => "lol"

object Playground:
  @main
  def run = {
    val x: Base.Primitive.Required.Writer[String] = string
    val a = Base.Union.Value.Required.Writer.Root[container.Schema, container.Primitive[x.type], String](x)
    println(StringEncoder(a, "asdf"))
  }
