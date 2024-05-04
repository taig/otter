package io.taig.otter

import io.taig.otter.validation.Validation

type Schema[+S, A] = Primitive[A]

object Schema:
  trait Operation[S[+_, _], +Of, A]:
    def ivalidate[B, C](constraint: Schema[Any, B])(validation: Validation[A, B, C])(g: C => A): S[Of, C]
    final def imap[B](f: A => B)(g: B => A): S[Of, B] = ???

  object Operation:
    trait Read[S[+_, _], +Of, A]:
      def map[B](f: A => B): S[Of, B]
      final def as[B](value: B): S[Of, B] = map(_ => value)

    trait Write[S[+_, _], +Of, A]:
      def contramap[B](f: B => A): S[Of, B]
