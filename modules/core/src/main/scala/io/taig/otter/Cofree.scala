package io.taig.otter

final case class Cofree[+S[+_], A](value: S[Cofree[S, A]], a: A)
