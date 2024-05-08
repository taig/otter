package io.taig.otter

final case class Fix[S[_]](unfix: S[Fix[S]])

final case class Cofree[S[_], A](value: S[Cofree[S, A]], a: A)
