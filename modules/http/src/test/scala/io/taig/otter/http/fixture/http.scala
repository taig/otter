package io.taig.otter.http.fixture

import io.taig.otter.http.Headers
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.http.component.HttpComponent.*

object http:
  /** `/users/{id}`. The two literals contribute nothing, so the path holds an `Int` and not a `(Unit, Int, Unit)`. */
  val user: Path[Int] = PNil :* segment("users") :* segment("id", int)

  /** The same path with no root named, which is what two segments beside each other already are. */
  val rootless: Path[Int] = segment("users") :* segment("id", int)

  /** The same path again, spelled the way Scala spells a cons. */
  val consed: Path[Int] = segment("users") *: segment("id", int) *: PNil

  /** `/users/{id}/posts`, to show that a literal after a placeholder drops out just the same. */
  val posts: Path[Int] = PNil :* segment("users") :* segment("id", int) :* segment("posts")

  /** `?page&tags`, where `page` may be left out and `tags` may be given more than once. */
  val listing: Queries[(Option[Int], List[String])] =
    query("page", int).optional :* query("tags", collection.list(string))

  /** `?page` standing for the first page when it is not given. */
  val paged: Queries[Int] = query("page", int).optional(1).toRecord

  /** A flag, which is what a bare `?verbose` is: a name given with no value at all.
    *
    * `strict` is what makes it one. A lenient parameter reads a name carrying no text as absence before the value is
    * looked at, which is right for `?page=` and wrong here, where giving the name is the assertion.
    */
  val verbose: Queries[Boolean] = query("verbose", coerce(boolean)).strict.optional(false).toRecord

  /** One header that has to be there and one list valued one that need not be. */
  val request: Headers[(String, Option[List[String]])] =
    header("X-Request-Id", string) :* header("Accept-Language", collection.list(string)).optional

  /** Ascribed to say that every segment is a primitive, which is the ordinary shape of a path and a compile error for
    * anything that would need more than one piece of text.
    */
  val flat: Path.Of[[w, r] =>> io.taig.otter.http.Segment.Schema[Parameter.Primitive.Node, w, r], Int] =
    PNil :* segment("users") :* segment("id", int)
