package io.taig.otter.sample.schemas

import io.taig.otter.Schema
import io.taig.otter.schemas.*
import io.taig.otter.sample.Isbn

val isbn: Schema.Primitive[Isbn] = long.ivalidate(Isbn.validation)(_.toLong)
