package io.taig.otter

sealed abstract class Record[+S[_], A] extends Codec[S, A]:
  def codecs: Vector[Reference[S, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Record[S, A]
  final override def imap[B](f: A => B)(g: B => A): Record[S, B] = Record.Modify(self = this, f, g)

object Record:
  final private[otter] case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
    override def codecs: Vector[Nothing] = Vector.empty
    override def modifyMetadata(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Record[S, B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[S[_], A](field: Field[S, A], metadata: Metadata) extends Record[S, A]:
    override def codecs: Vector[Reference[S, ?]] = Vector(field.codec)
    override def modifyMetadata(f: Metadata => Metadata): Record[S, A] = copy(metadata = f(metadata))

  final private[otter] case class Zip[S[_], A, B](
      left: Record[S, A],
      right: Record[S, B],
      metadata: Metadata
  ) extends Record[S, (A, B)]:
    override def codecs: Vector[Reference[S, ?]] = left.codecs ++ right.codecs
    override def modifyMetadata(f: Metadata => Metadata): Record[S, (A, B)] = copy(metadata = f(metadata))

  given [S[_]]: CodecInvariant[Record[S, *]] with
    override def imap[A, B](fa: Record[S, A])(f: A => B)(g: B => A): Record[S, B] = fa.imap(f)(g)
