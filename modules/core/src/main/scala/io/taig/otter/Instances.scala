package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait Instances extends Types:
  given schemaInvariant[A <: Schema[?]]: SchemaInvariant[Schema.Of[A, *]] with
    override def constraints[B](fa: Schema.Of[A, B]): Chain[Constraint[?]] = fa.constraints

    override def ivalidate[B, V1, V2, C](fa: Schema.Of[A, B])(validation: Validation[B, V1, V2, C])(
        f: C => B
    ): Schema.Of[A, C] = fa.ivalidate(validation)(f)
