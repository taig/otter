package io.taig.otter

import cats.Show
import cats.Eq
import cats.derived.*

final case class Const(namespace: Option[String], name: String) derives Eq

object Const:
  given Show[Const] =
    case Const(None, name)            => name
    case Const(Some(namespace), name) => s"$namespace.$name"
