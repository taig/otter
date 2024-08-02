// package io.taig.otter

// import io.taig.otter.Codec.Result

// sealed abstract class Branch[+O <: Data, A]:
//   self =>

//   def name: String

//   def codec: Codec[?]

//   def metadata: Metadata

//   final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
//     export self.{codec, decode, encode, name}
//     override def metadata: Metadata = f(self.metadata)

//   final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
//     export self.{codec, metadata, name}
//     override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
//     override def encode(b: B): O = self.encode(g(b))

//   final def :+[P <: Data, B](branch: Branch[P, B]): Branches[O | P, Either[A, B]] = toBranches :+ branch

//   final def +:[P <: Data, B](branch: Branch[P, B]): Branches[P | O, Either[B, A]] = branch +: toBranches

//   final def toBranches: Branches[O, A] = Branches(this)

//   def decode(data: Data): Codec.Result[A]

//   def encode(a: A): O

// object Branch:
//   def apply[A](name: String, of: Codec[A]): Branch[of.Out, A] =
//     val _name = name

//     new Branch[of.Out, A]:
//       override def name: String = _name
//       override def codec: Codec[?] = of
//       override def metadata: Metadata = Metadata.Empty
//       override def decode(data: Data): Codec.Result[A] = of.decode(data)
//       override def encode(a: A): of.Out = of.encode(a)

//   extension [O <: Data, A <: Matchable](self: Branch[O, A])
//     inline def |[P <: Data, B <: Matchable](branch: Branch[P, B]): Branches[O | P, A | B] =
//       self.toBranches | branch

//   given [O <: Data, A]: Metadata.Ops[Branch[O, A]] with
//     extension (self: Branch[O, A])
//       override def metadata: Metadata = self.metadata
//       override def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = self.modifyMetadata(f)
