package io.taig.otter.sample.api.endpoint.librarian.librarians

import io.taig.otter.http.Url
import io.taig.otter.sample.api.endpoint.librarian.url as parent

val url: Url[Unit] = parent / "librarians"
