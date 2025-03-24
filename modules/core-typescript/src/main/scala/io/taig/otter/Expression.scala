package io.taig.otter

import cats.Show

enum Expression:
  case Reference(namespace: Option[String], value: String)
  case Value(value: String)

object Expression:
  given Show[Expression] =
    case Reference(Some(namespace), value) => s"$namespace.$value"
    case Reference(None, value)            => value
    case Value(value)                      => value
