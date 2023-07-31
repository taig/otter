package io.taig.openapi.schema

import cats.data.ValidatedNec

abstract class Decoder[F[_], A]:
  def decode[B](fa: F[B], a: A): ValidatedNec[String, B]
