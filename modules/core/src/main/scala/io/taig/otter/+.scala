package io.taig.otter

type +[F[_], G[_]] = [a] =>> F[a] | G[a]
