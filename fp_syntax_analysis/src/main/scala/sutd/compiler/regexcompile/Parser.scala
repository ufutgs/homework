package sutd.compiler.regexcompile

import sutd.compiler.regexcompile.RegExp.*
import sutd.compiler.regexcompile.Parsec.*
import sutd.compiler.regexcompile.Functor.*
import sutd.compiler.regexcompile.Applicative.*
import sutd.compiler.regexcompile.Monad.*
import sutd.compiler.regexcompile.Lexer.*


/** regex external syntax
  *
  * r ::= [c-c] | [cc] | [] | c | r|r | r* | r? | rr | (r) c ::= a | ... | z | A
  * | ... | Z | 1 | ... | 0
  *
  * left recursion elimination
  *
  * step 1. remove left recursion in r 

  * TODO
  *
  * step 2. left factorization
  *
  * TODO
  */


object Parser {
    import RegExp.*

    case class PEnv(toks: List[LToken])

    given penvParserEnv: ParserEnv[PEnv, LToken] = new ParserEnv[PEnv, LToken] {
        override def getTokens(env: PEnv): List[LToken] = env match {
            case PEnv(toks) => toks
        }
        override def getCol(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => col
                }
        }
        override def getLine(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => ln
                }
        }
        override def setTokens(ts: List[LToken])(env: PEnv): PEnv = env match {
            case PEnv(_) => PEnv(ts)
        }

    }


    def parse_regex(toks:List[LToken]):Either[String,RE] = { // TODO fixme
        Left("parser not yet implemented.")
    }


}
