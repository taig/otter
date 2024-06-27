package io.taig.otter

type ValidationWriter[F[+_], A] = (F[Schema.Writer[F, ?, A]], A)
