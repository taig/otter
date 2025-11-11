package io.taig.otter.syntax

trait AllSyntax
    extends CatsSyntax,
      CoerceableSyntax,
      CoerceSyntax,
      CollectionSyntax,
      ConstantSyntax,
      DictionarySyntax,
      EnumerationSyntax,
      NullableSyntax,
      NullishSyntax,
      RecordSyntax,
      TupleableSyntax,
      TupleSyntax,
      UnionSyntax

object AllSyntax extends AllSyntax
