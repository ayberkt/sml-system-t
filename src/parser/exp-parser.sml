structure ExpParser =
  Join(structure LrParser = LrParser
      structure ParserData = ExpLrVals.ParserData
      structure Lex = ExpLex)
