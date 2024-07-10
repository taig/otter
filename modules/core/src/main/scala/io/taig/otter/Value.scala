package io.taig.otter

trait Value[+F[+_], +A, B] extends Codec[F, A, B], Value.Reader[F, A, B], Value.Writer[F, A, B]

object Value:
  trait Reader[+F[+_], +A, +B] extends Codec.Reader[F, A, B]

  trait Writer[+F[+_], +A, -B] extends Codec.Writer[F, A, B]
