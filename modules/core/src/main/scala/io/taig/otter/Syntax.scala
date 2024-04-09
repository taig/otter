// package io.taig.otter

// trait Syntax extends Types:
//   extension [A](self: Schema[A]) def toTuple: Tuple.Of[self.type, A] = self.toTupleWith(_ => Metadata.tuple)

//   extension [A](self: Tuple[A])
//     final def product[B](tuple: Tuple[B]): Tuple[(A, B)] = self.productWith((_, _) => Metadata.tuple)(tuple)
//     final def zip[B](tuple: Tuple[B])(using merge: Evidence.Merge[A, B]): Tuple[merge.Out] =
//       product(tuple).imap(merge.apply)(merge.unapply)
//     final def :*[B](schema: Schema[B])(using merge: Evidence.Merge[A, B]): Tuple[merge.Out] = zip(schema.toTuple)
//     final def *:[B](schema: Schema[B])(using merge: Evidence.Merge[B, A]): Tuple[merge.Out] = schema.toTuple.zip(self)
