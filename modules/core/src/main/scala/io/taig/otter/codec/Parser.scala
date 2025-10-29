package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

type Parser[-S[_]] = Decoder[S, String]
