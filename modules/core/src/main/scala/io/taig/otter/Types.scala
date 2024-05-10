package io.taig.otter

trait Types:
  type Schema[A]

  trait Schemas:
    type Of[+Of, A]

    type Reader[A]

    type Writer[A]

  val Schema: Schemas

  type Collection[A]

  trait Collections:
    type Of[+Of, A]

    type Reader[A]

    trait Readers:
      type Of[+Of, A]

    val Reader: Readers

    type Writer[A]

    trait Writers:
      type Of[+Of, A]

    val Writer: Writers

  val Collection: Collections

  type Primitive[A]

  trait Primitives:
    type Reader[A]

    trait Readers:
      type Of[+Of, A]

    val Reader: Readers

    type Writer[A]

    trait Writers:
      type Of[+Of, A]

    val Writer: Writers

  val Primtivie: Primitives

  type Tuple[A]

  trait Tuples:
    type Of[+Of, A]

    type Reader[A]

    type Writer[A]

  val Tuple: Tuples
