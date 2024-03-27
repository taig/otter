// package io.taig.otter.http

// import cats.data.Chain

// final case class Endpoint[I, O](
//     request: Request[I],
//     response: Response[O],
//     deprecated: Boolean,
//     description: Option[String],
//     hidden: Boolean,
//     operationId: Option[String],
//     summary: Option[String],
//     tags: Chain[String]
// ):
//   def request[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
//   def request[T](value: Request[T]): Endpoint[T, O] = request(_ => value)

//   def response[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))
//   def response[T](value: Response[T]): Endpoint[I, T] = response(_ => value)

//   def deprecated(f: Boolean => Boolean): Endpoint[I, O] = copy(deprecated = f(deprecated))
//   def deprecated(value: Boolean): Endpoint[I, O] = deprecated(_ => value)

//   def description(f: Option[String] => Option[String]): Endpoint[I, O] = copy(description = f(description))
//   def description(value: Option[String]): Endpoint[I, O] = description(_ => value)
//   def description(value: String): Endpoint[I, O] = description(Some(value))

//   def hidden(f: Boolean => Boolean): Endpoint[I, O] = copy(hidden = f(hidden))
//   def hidden(value: Boolean): Endpoint[I, O] = hidden(_ => value)

//   def operationId(f: Option[String] => Option[String]): Endpoint[I, O] = copy(operationId = f(operationId))
//   def operationId(value: Option[String]): Endpoint[I, O] = operationId(_ => value)
//   def operationId(value: String): Endpoint[I, O] = operationId(Some(value))

//   def summary(f: Option[String] => Option[String]): Endpoint[I, O] = copy(summary = f(summary))
//   def summary(value: Option[String]): Endpoint[I, O] = summary(_ => value)
//   def summary(value: String): Endpoint[I, O] = summary(Some(value))

//   def tags(f: Chain[String] => Chain[String]): Endpoint[I, O] = copy(tags = f(tags))
//   def tags(values: Chain[String]): Endpoint[I, O] = tags(_ => values)
//   def tags(values: String*): Endpoint[I, O] = tags(Chain.fromSeq(values))

// object Endpoint:
//   def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
//     Endpoint(request, response, false, None, false, None, None, Chain.empty)
