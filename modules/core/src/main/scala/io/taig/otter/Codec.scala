package io.taig.otter

abstract class Codec[+S[_], A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[S, A]
  def imap[B](f: A => B)(g: B => A): Codec[S, B]

  final def nullable: Optional[S, Option[A]] = ??? // TODO Optional.Nullable(codec = Reference.now(this), metadata)
  final def nullable(default: A): Optional[S, A] = ???
  // TODO   Optional.Default(codec = Eval.now(this), value = default, metadata)

object Codec:
  given [S[_]]: CodecInvariant[Codec[S, *]] with
    override def imap[A, B](fa: Codec[S, A])(f: A => B)(g: B => A): Codec[S, B] = fa.imap(f)(g)
