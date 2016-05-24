structure ExpLrVals =
  ExpLrValsFun(structure Token = LrParser.Token)

structure ExpLex =
  ExpLexFun(structure Tokens = ExpLrVals.Tokens)
