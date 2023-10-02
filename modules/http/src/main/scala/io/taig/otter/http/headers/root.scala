package io.taig.otter.http.headers

import io.taig.otter.http.Header
import io.taig.otter.http.syntax.*
import io.taig.otter.codecs.*
import org.typelevel.ci.*

val contentLength: Header[Long] = header(ci"Content-Length", long)

val contentType: Header[ContentType] = header(ci"Content-Type", string.ivalidate(ContentType.validation)(_.print))
