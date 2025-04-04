// package io.taig.otter.http

// import cats.data.Validated
// import cats.data.Validated.Invalid
// import cats.data.Validated.Valid
// import cats.syntax.all.*
// import io.taig.otter.Codec
// import io.taig.otter.Merge

// sealed abstract class Headers[A]:
//   self =>

//   def toVector: Vector[Header[?]]

//   final def imap[B](f: A => B)(g: B => A): Headers[B] = new Headers[B]:
//     export self.toVector
//     override def decode(headers: Http.Headers): (Http.Headers, Codec.Result[B]) = self.decode(headers).map(_.map(f))
//     override def encode(b: B): Http.Headers = self.encode(g(b))

//   final def zip[B](headers: Headers[B]): Headers[(A, B)] = new Headers[(A, B)]:
//     override def toVector: Vector[Header[?]] = self.toVector ++ headers.toVector
//     override def decode(values: Http.Headers): (Http.Headers, Codec.Result[(A, B)]) =
//       self.decode(values) match
//         case (values, Validated.Valid(a)) =>
//           headers.decode(values) match
//             case (values, Validated.Valid(b))           => (values, (a, b).valid)
//             case (values, right @ Validated.Invalid(_)) => (values, right)
//         case (values, Validated.Invalid(left)) =>
//           headers.decode(values) match
//             case (values, Validated.Valid(_))       => (values, left.invalid)
//             case (values, Validated.Invalid(right)) => (values, (left |+| right).invalid)
//     override def encode(ab: (A, B)): Http.Headers = self.encode(ab._1) ++ headers.encode(ab._2)

//   final def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] =
//     zip(header.toHeaders).imap(merge.apply)(merge.unapply)

//   final def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] =
//     header.toHeaders.zip(this).imap(merge.apply)(merge.unapply)

//   def encode(a: A): Http.Headers

//   def decode(headers: Http.Headers): (Http.Headers, Codec.Result[A])

// object Headers:
//   val Empty: Headers[Unit] = new Headers[Unit]:
//     override def toVector: Vector[Header[?]] = Vector.empty
//     override def encode(a: Unit): Http.Headers = Vector.empty
//     override def decode(headers: Http.Headers): (Http.Headers, Codec.Result[Unit]) = (headers, ().valid)

//   def apply[A](header: Header[A]): Headers[A] = new Headers[A]:
//     override def toVector: Vector[Header[?]] = Vector(header)
//     override def encode(a: A): Http.Headers = header.encode(a)
//     override def decode(headers: Http.Headers): (Http.Headers, Codec.Result[A]) = header.decode(headers)
