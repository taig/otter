package io.taig.otter

final case class Fix[+S[+_]](unfix: S[Fix[S]])
