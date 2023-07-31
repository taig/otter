package io.taig.openapi.validation

import io.taig.openapi.OpenApi

final case class Constraint(identifier: String, reference: Option[OpenApi.Primitive]):
  override def toString: String = reference.fold(identifier)(reference => s"$identifier(${reference.render})")
