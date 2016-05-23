(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)
(* Modified by Kevin Watkins *)

signature INPUT =
sig

  (* Each element of an input stream provides a character
     together with a list of all characters from that point
     to the end of the line (for error reporting) *)
  val readFile  : string -> (char * char list) Stream.stream
  val readKeybd : unit   -> (char * char list) Stream.stream

  (* The type of this one is slightly subtle in order to handle
     different prompts for the start and the continuation of a term. *)
  val promptKeybd : string -> string ->
          ((char * char list) Stream.stream -> 'a Stream.stream) ->
          'a Stream.stream

end;  (* signature INPUT *)

structure Input :> INPUT =
struct

  structure T = TextIO
  structure S = Stream

  (* Reading a stream of lines from some input source *)

  fun isBlank ln = (String.tokens Char.isSpace ln = nil)

  fun keybdLines file prompt1 prompt2 ducer =
      let val pr = ref prompt1
          fun get () =
              ((!pr) ();
	       case T.inputLine file
		 of NONE => S.Nil
		  | SOME(ln) =>
                    (if not (isBlank ln) then pr := prompt2 else ();
		     S.Cons (ln, S.delay get)))
          fun put s =
              case (pr := prompt1; S.force s)
                of S.Cons (x, s) => S.Cons (x, S.delay (fn () => put s))
                 | S.Nil => S.Nil
      in
        S.delay (fn () => put (ducer (S.delay get)))
      end

  fun fileLines filename =
      let fun get file =
	      case T.inputLine file
	        of NONE => (T.closeIn file; S.Nil)
		 | SOME(ln) => S.Cons (ln, S.delay (fn() => get file))
      in
        S.delay (fn () => get (T.openIn filename))
      end

  (* Splitting a stream of lines into a stream of chars
     (with rest-of-lines for error reporting) *)

  fun line (nil, s) = S.force s
    | line (l as (h::t), s) = S.Cons ((h, l), S.delay (fn () => line (t, s)))

  fun linesToChars s =
      let fun get s =
              case S.force s
                of S.Cons (x, s) => line (explode x, S.delay (fn () => get s))
                 | S.Nil => S.Nil
      in
        S.delay (fn () => get s)
      end

  (* Putting it together *)

  fun readFile filename = linesToChars (fileLines filename)

  fun readKeybd () = keybdLines T.stdIn (fn () => ()) (fn () => ()) linesToChars

  fun doprompt s = (T.output (T.stdOut, s); T.flushOut T.stdOut)

  fun promptKeybd prompt1 prompt2 ducer =
        keybdLines T.stdIn (fn () => doprompt prompt1)
                           (fn () => doprompt prompt2)
                           (fn s => ducer (linesToChars s))

end;  (* structure Input *)
