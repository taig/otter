package io.taig.otter.fixture

import io.taig.otter.Csv
import io.taig.otter.component.CsvComponent.*

object csv:
  val book: Csv[Book] = (field("title", string) :* field("pages", int) :* field("read", boolean)).to

  /** The same row with its names taken away, which is what a file without a header is. */
  val positional: Csv.Tuple[Book] = (TNil :* string :* int :* boolean).to

  /** The same note twice: once keeping the column and leaving the cell empty, once dropping the column. Keeping it is
    * the default, because a row owes its header a cell.
    */
  val blankTag: Csv.Record[Note] = (field("title", string) :* field("tag", int).optional).to

  val omittedTag: Csv.Record[Note] = (field("title", string) :* field("tag", int).optional.omitted).to

  /** Two layers of absence, which only a strict column can tell apart: a missing column is the outer one, an empty cell
    * the inner one.
    */
  val nestedTag: Csv[Option[Option[Int]]] = field("tag", int.optional).optional.omitted.strict.toRecord

  val genre: Csv.Enumeration[Genre] = enumeration(string):
    case Genre.Fiction => "fiction"
    case Genre.History => "history"
    case Genre.Poetry  => "poetry"

  /** The same three columns, ascribed to say that every one of them is a primitive. For CSV this is not a special case
    * to be asked for but the ordinary shape of a row, and it is a compile error to write it down for a schema that
    * nests.
    */
  val flatBook: Csv.Record.Of[Csv.Primitive.Node, Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to

  /** Can be written but not read: there is no way back from a title to a book. */
  val title: Csv.Primitive.Text.Writer[Book] = printer("title", _.title)

  /** Can be read but not written. */
  val isbn: Csv.Primitive.Text.Reader[Isbn] = parser("isbn", value => Right(Isbn(value)))
