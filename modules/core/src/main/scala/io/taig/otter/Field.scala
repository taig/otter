package io.taig.otter

final case class Field[+S[_], A](name: String, schema: Reference[S, A])
