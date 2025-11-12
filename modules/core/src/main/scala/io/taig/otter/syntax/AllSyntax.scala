package io.taig.otter.syntax

trait AllSyntax
    extends CatsSyntax,
      CoerceableSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      EnumerationSyntax,
      FieldSyntax,
      NullableSyntax,
      NullishSyntax,
      RecordSyntax,
      RecordeableSyntax,
      TupleableSyntax,
      TupleSyntax,
      UnionSyntax

object AllSyntax extends AllSyntax
