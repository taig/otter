// package io.taig.otter.http

// import io.taig.otter.Violations
// import io.taig.otter.http.HttpExport.*

// final case class Response[+S[_], A](results: S[A], validation: S[Violations], failure: S[Option[String]]):
//   def modifyResults[S1[a] >: S[a], B](f: S[A] => S1[B]): Response[S1, B] = copy(results = f(results))

//   def modifyValidation[S1[a] >: S[a]](f: S[Violations] => S1[Violations]): Response[S1, A] = copy(validation = f(validation))

//   def modifyFailure[S1[a] >: S[a]](f: S[Option[String]] => S1[Option[String]]): Response[S1, A] = copy(failure = f(failure))

// object Response:
//   final case class Data(code: Code, headers: Headers.Data, body: Array[Byte]):
//     def modifyHeaders(f: Headers.Data => Headers.Data): Response.Data = copy(headers = f(headers))

//     def modifyBody(f: Array[Byte] => Array[Byte]): Response.Data = copy(body = f(body))
//     def withBody(body: Array[Byte]): Response.Data = modifyBody(_ => body)

//   // given ResponseSchema[Response] with
//   //   override def schema[S[_], T[_]]: Schema[Response[S, T, *]] = new Schema[Response[S, T, *]]:
//   //     override def imap[A, B](fa: Response[S, T, A])(f: A => B)(g: B => A): Response[S, T, B] = fa.imap(f)(g)

//   //   extension [S[_], T[_], A](self: Response[S, T, A])
//   //     override def modifyResults[U[a] >: S[a], B](f: Results[S, A] => Results[U, B]): Response[U, T, B] =
//   //       self.modifyResults(f)
