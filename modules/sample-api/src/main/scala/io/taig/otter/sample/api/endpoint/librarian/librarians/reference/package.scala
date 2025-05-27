package io.taig.otter.sample.api.endpoint.librarian.librarians.reference

import io.taig.otter.http.Url
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.endpoint.librarian.librarians.url as parent

import java.util.UUID

val url: Url[UUID] = parent / parameter("librarian", parameter.uuid)
