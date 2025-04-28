package io.taig.otter

trait DataDsl[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Union[a] <: Value[a],
    Key[_],
    Value[_]
](using
    Codec.Collection[Collection, Value],
    Codec.Dictionary[Dictionary, Key, Value],
    Codec.Nullable[Nullable, Value],
    Codec.Union[Union, Value]
) extends CollectionDsl[Collection, Value],
      DictionaryDsl[Dictionary, Key, Value],
      NullableDsl[Nullable, Value],
      PrimitiveDsl[Primitive],
      UnionDsl[Union, Value]:
  def key: PrimitiveDsl.String[Key]

  object data:
    val any: Nullable[Data.Any] = nullable(value).imap(_.getOrElse(Data.Null)) {
      case Data.Null        => None
      case data: Data.Value => Some(data)
    }

    val number: Union[Data.Number] =
      branch("bigDecimal", jBigDecimal) |
        branch("bigInterger", jBigInteger) |
        branch("long", long) |
        branch("int", int) |
        branch("float", float) |
        branch("double", double)

    val primitive: Union[Data.Primitive] = number | branch("boolean", boolean) | branch("string", string)

    val value: Union[Data.Value] = primitive | branch("object", obj) | branch("array", array)

    def obj[A <: Data.Any](codec: => Value[A]): Dictionary[Data.Object[A]] =
      dictionary.list(key.string, codec).imap(Data.Object[A])(_.values)

    val obj: Dictionary[Data.Object[Data.Any]] = obj(any)

    def array[A <: Data.Any](codec: => Value[A]): Collection[Data.Array[A]] =
      collection.vector(codec).imap(Data.Array[A])(_.values)

    val array: Collection[Data.Array[Data.Any]] = array(any)
