package io.taig.otter

import cats.Show

final case class Reference(namespace: Option[String], name: String)

object Reference:
  given Show[Reference] =
    case Reference(None, name)            => name
    case Reference(Some(namespace), name) => s"$namespace.$name"
