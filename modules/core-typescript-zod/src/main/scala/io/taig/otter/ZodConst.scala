package io.taig.otter

import cats.Eq
import cats.Show
import cats.syntax.all.*
import cats.derived.*

final case class ZodConst(namespace: Option[String], name: String) derives Eq

object ZodConst:
  def apply(name: String): ZodConst = ZodConst(namespace = none, name)

  given Show[ZodConst] =
    case ZodConst(None, name)            => name
    case ZodConst(Some(namespace), name) => s"$namespace.$name"
