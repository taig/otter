package io.taig.otter.http.headers

import io.taig.otter.http.Header
import io.taig.otter.http.syntax.*
import io.taig.otter.schema.schemas.*
import org.typelevel.ci.CIStringSyntax

val contentType: Header[ContentType] = header(ci"Content-Type", string.ivalidate(ContentType.validation)(_.print))
