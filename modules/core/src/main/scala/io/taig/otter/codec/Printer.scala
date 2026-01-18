package io.taig.otter.codec

type Printer[-F[_]] = Encoder[F, String]
