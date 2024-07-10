package io.taig.otter

object SchemaOps:
  trait Isomorphic[Self[_, _, _], Optional[_, _, _], Plain[_, _]]:
    extension [A, B, C](self: Self[A, B, C])
      def optional: Optional[A, B, Option[C]]
      def plain: Plain[A, C]

  trait Reader[Self[_, _, _], Optional[_, _, _], Plain[_, _]]:
    extension [A, B, C](self: Self[A, B, C])
      def optional: Optional[A, B, Option[C]]
      def plain: Plain[A, C]

  trait Writer[Self[_, _, _], Optional[_, _, _], Plain[_, _]]:
    extension [A, B, C](self: Self[A, B, C])
      def optional: Optional[A, B, Option[C]]
      def plain: Plain[A, C]
