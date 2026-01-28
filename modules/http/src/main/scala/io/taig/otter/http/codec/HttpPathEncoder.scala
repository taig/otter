package io.taig.otter.http.codec

import io.taig.otter.http.Http
import io.taig.otter.codec.Encoder
import cats.data.Chain

val HttpPathEncoder: Encoder[Http.Path.Write, Chain[String]] = PathEncoder.contramapK([_] => _.self.self)
