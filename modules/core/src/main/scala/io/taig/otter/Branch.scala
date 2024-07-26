// package io.taig.otter

// sealed abstract class Branch[+F[+_], +O, A]:
//   def name: String

//   def metadata: Metadata
//   final def modifyMetadata(f: Metadata => Metadata): Branch[F, O, A] = ???

//   def codec: Codec[?, ?, ?]

//   final def imap[B](f: A => B): Branch[F, O, B] = ???

//   def decode(data: Data): Codec.Result[A]

//   def encode(a: A): F[O]

// object Branch:
//   def apply[F[+_], O, A](name: String, codec: Codec[F, O, A]): Branch[F, O, A] =
//     val _name = name
//     val _codec = codec

//     new Branch[F, O, A]:
//       override def name: String = _name
//       override def metadata: Metadata = Metadata.Empty
//       override def codec: Codec[F, O, A] = _codec
//       override def decode(data: Data): Codec.Result[A] = codec.decode(data)
//       override def encode(a: A): F[O] = codec.encode(a)
