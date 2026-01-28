package io.taig.otter.codec

type Parser[-F[_]] = Decoder[F, String]
