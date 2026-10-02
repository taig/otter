package io.taig.otter.fixture

object OpaqueProducts:
  opaque type Isbn = String

  object Isbn:
    def apply(value: String): OpaqueProducts.Isbn = value

    def render(value: OpaqueProducts.Isbn): String = value

  final case class Book(isbn: OpaqueProducts.Isbn, title: String, edition: Int)

  final case class Reordered(title: String, isbn: OpaqueProducts.Isbn, edition: Int)

  final case class Identifier(isbn: OpaqueProducts.Isbn)

  opaque type Pair = (String, Int)

  object Pair:
    def apply(label: String, edition: Int): OpaqueProducts.Pair = (label, edition)

    def label(value: OpaqueProducts.Pair): String = value._1

  opaque type Marker = Unit

  object Marker:
    def apply(): OpaqueProducts.Marker = ()

  final case class Wrapped(pair: OpaqueProducts.Pair, marker: OpaqueProducts.Marker, isbn: OpaqueProducts.Isbn)
