package io.taig.otter

// trait SchemaReaderOps[R1[_], R2[_]]:
//   extension [A](self: R1[A])
//     def collection: R1[Vector[A]]
//     def optional: R2[Option[A]]

// trait SchemaWriterOps[W1[_], W2[_]]:
//   extension [A](self: W1[A])
//     def collection: W1[Vector[A]]
//     def optional: W2[Option[A]]
//     def contramap[B](f: B => A): W1[B]

// trait SchemaOps[I1[_], I2[_], R1[_], R2[_]] extends SchemaReaderOps[F, G], SchemaWriterOps[F, G]
