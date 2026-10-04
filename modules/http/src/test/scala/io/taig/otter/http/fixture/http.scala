package io.taig.otter.http.fixture

import io.taig.otter.http.Headers
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.http.component.HttpComponent.*

object http:
  /** `/users/{id}`. The two literals contribute nothing, so the path holds an `Int` and not a `(Unit, Int, Unit)`. */
  val user: Path[Int] = __ :* segment("users") :* segment("id", int)

  /** The same path with no root named, which is what two segments beside each other already are. */
  val rootless: Path[Int] = segment("users") :* segment("id", int)

  /** The same path again, spelled the way Scala spells a cons. */
  val consed: Path[Int] = segment("users") *: segment("id", int) *: __

  /** The same path once more, spelled the way a URL is. */
  val sliced: Path[Int] = __ / segment("users") / segment("id", int)

  /** And with no root named, which `/` allows for the reason `:*` does. */
  val slicedRootless: Path[Int] = segment("users") / segment("id", int)

  /** And with the literal written as the text it is, which is all `segment` had left to say about it. */
  val named: Path[Int] = __ / "users" / segment("id", int)

  /** And once more with the root named rather than spelled, which is the same value under its other name. */
  val spelled: Path[Int] = Path.Root / "users" / segment("id", int)

  /** `/users/{id}/posts`, to show that a literal after a placeholder drops out just the same. */
  val posts: Path[Int] = __ :* segment("users") :* segment("id", int) :* segment("posts")

  /** `?page&tags`, where `page` may be left out and `tags` may be given more than once. */
  val listing: Queries[(Option[Int], List[String])] =
    query("page", int).optionalOrEmpty :* query("tags", collection.list(string))

  /** `?page` standing for the first page when it is not given. */
  val paged: Queries[Int] = query("page", int).defaultedOnMissingOrEmpty(1).toRecord

  /** A bare or empty flag means true; omission means false, and explicit Boolean values remain accepted. */
  val verbose: Queries[Boolean] = query.flag("verbose").toRecord

  /** One header that has to be there and one list valued one that need not be. */
  val request: Headers[(String, Option[List[String]])] =
    header("X-Request-Id", string) :* header("Accept-Language", collection.list(string)).optionalOrEmpty

  /** A required list-valued header: a missing name fails, but a present empty line means no elements. */
  val languages: Headers[List[String]] =
    header("Accept-Language", collection.list(string)).toRecord

  /** Ascribed to say that every segment is a primitive, which is the ordinary shape of a path and a compile error for
    * anything that would need more than one piece of text.
    */
  val flat: Path.Of[io.taig.otter.http.Segment.Schema[Parameter.Primitive.Node, *, *], Int] =
    __ :* segment("users") :* segment("id", int)

  /** The same claim about a path built with `/`: a bare literal is a primitive segment and widens nothing. */
  val slicedFlat: Path.Of[io.taig.otter.http.Segment.Schema[Parameter.Primitive.Node, *, *], Int] =
    __ / "users" / segment("id", int)

  /** And the same claim about the root written out, which is what says the two names are one value. */
  val spelledFlat: Path.Of[io.taig.otter.http.Segment.Schema[Parameter.Primitive.Node, *, *], Int] =
    Path.Root / "users" / segment("id", int)
