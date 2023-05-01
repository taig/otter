package io.taig.openapi.schema

import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{validations, Validation}
import io.taig.openapi.validation

object validations:
  object openapi:
    def refine[A](tpe: String)(f: OpenApi => Option[A]): Validation[OpenApi, OpenApi, OpenApi, A] =
      validation.validations.refine(s"OpenApi.$tpe")(f).mapReference(OpenApi.fromString)
    val obj: Validation[OpenApi, OpenApi, OpenApi, OpenApi.Object] = refine("Object")(_.asObject)
    val primitive: Validation[OpenApi, OpenApi, OpenApi, OpenApi.Primitive] = refine("Primitive")(_.asPrimitive)
