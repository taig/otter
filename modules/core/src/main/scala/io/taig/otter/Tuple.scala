// package io.taig.otter

// import cats.data.Chain

// sealed abstract class Tuple[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Codec[F, Data.Array[O], A] {
//   def codecs: Chain[Codec[?, ?, ?]]

//   override def decode(data: Data): Codec.Result[A] = ???
// }
