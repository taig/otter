package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema.Primitive
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.Isbn

val isbn: Primitive[Isbn] = long.ivalidate(Isbn.validation)(_.toLong)
