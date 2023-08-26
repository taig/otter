package io.taig.otter.sample.schemas

import io.taig.otter.schema.Primitive
import io.taig.otter.schema.schemas.*
import io.taig.otter.sample.Isbn

val isbn: Primitive[Isbn] = long.ivalidate(Isbn.validation)(_.toLong)
