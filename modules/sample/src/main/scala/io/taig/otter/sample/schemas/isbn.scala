package io.taig.otter.sample.schemas

import io.taig.otter.Schema
import io.taig.otter.sample.Isbn
import io.taig.otter.schemas.*

val isbn: Schema.Primitive[Isbn] = long.ivalidate(Isbn.validation)(_.toLong)
