package io.taig.otter.sample.api.codecs

import io.taig.otter.Primitive
import io.taig.otter.dsl.*
import io.taig.otter.sample.data.Isbn

val isbn: Primitive[Isbn] = long.ivalidate(Isbn.validation)(_.toLong)
