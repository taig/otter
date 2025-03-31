package io.taig.otter

import cats.Show

final case class Const(namespace: Option[String], name: String)

object Const:
  given Show[Const] =
    case Const(None, name)            => name
    case Const(Some(namespace), name) => s"$namespace.$name"
