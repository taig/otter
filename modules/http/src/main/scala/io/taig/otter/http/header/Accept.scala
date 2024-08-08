package io.taig.otter.http.header

import cats.data.NonEmptyList

opaque type Accept = NonEmptyList[Weighted[MediaRange]]
