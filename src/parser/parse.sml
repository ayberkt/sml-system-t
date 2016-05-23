structure Parse  =
  struct 

    structure ExpLrVals = ExpLrValsFun(structure Token = LrParser.Token)
    structure Lex = Exp_LexFun (structure Tokens = ExpLrVals.Tokens)
    structure ExpP = Join (structure ParserData = ExpLrVals.ParserData
                           structure Lex = Lex
                           structure LrParser = LrParser)

    fun stringreader s =
	let val pos = ref 0
  	    val remainder = ref (String.size s)
	    fun min(a, b) = if a < b then a else b 
	in
	  fn n =>
	    let
	      val m = min(n, !remainder)
	      val s = String.substring(s, !pos, m)
	      val () = pos := !pos + m
	      val () = remainder := !remainder - m
	    in
	      s
	    end
	end

    fun error (s, pos, pos') = (raise ParserState.Parse(s ^ "\n"))

    fun init symbols = 
        (ParserState.symtable := symbols;
        ParserState.stack := nil)
            
    fun parse symbols text = 
      let 
        val _ = init symbols
        val lexer =  ExpP.makeLexer (stringreader text)         
        val (res,_) = ExpP.parse(1, lexer, error, ())
        (* parse is complete -- skip the next token, either ; or eof *)
      in
        res
      end

end
