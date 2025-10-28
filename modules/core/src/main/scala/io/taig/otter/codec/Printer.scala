package io.taig.otter.codec

type Printer[-S[_]] = Encoder[S, String]
