package sutd.compiler.regexcompile

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.regexcompile.Lexer.*
import sutd.compiler.regexcompile.Parsec.*


class TestLexer extends funsuite.AnyFunSuite {
    import LToken.*
    import Progress.*
    import Result.*
    test("test lexing 1") {
        val  s = "[a-z]"
        val expected = List(LBracket(SrcLoc(0,1)), AlphaNum(SrcLoc(0,2),'a'), Hyphen(SrcLoc(0,3)), AlphaNum(SrcLoc(0,4),'z'), RBracket(SrcLoc(0,5)))

        Parsec.run(lex)(LEnv(s.toList,0,0)) match {
            case Consumed(Ok(toks,LEnv(List(),_,_))) => assert(toks == expected)
            case _ => assert(false)
        }
    }


    test("test lexing 2") {
        val  s = "[a-z]*"
        val expected = List(LBracket(SrcLoc(0,1)), AlphaNum(SrcLoc(0,2),'a'), Hyphen(SrcLoc(0,3)), AlphaNum(SrcLoc(0,4),'z'), 
                        RBracket(SrcLoc(0,5)), Asterix(SrcLoc(0,6)))

        Parsec.run(lex)(LEnv(s.toList,0,0)) match {
            case Consumed(Ok(toks,LEnv(List(),_,_))) => assert(toks == expected)
            case _ => assert(false)
        }
    }


    test("test lexing 3") {
        val  s = "[ab][AC]*"
        val expected = List(LBracket(SrcLoc(0,1)), AlphaNum(SrcLoc(0,2),'a'), AlphaNum(SrcLoc(0,3),'b'), 
                        RBracket(SrcLoc(0,4)), LBracket(SrcLoc(0,5)), AlphaNum(SrcLoc(0,6),'A'), 
                        AlphaNum(SrcLoc(0,7),'C'), RBracket(SrcLoc(0,8)), Asterix(SrcLoc(0,9))) 
        Parsec.run(lex)(LEnv(s.toList,0,0)) match {
            case Consumed(Ok(toks,LEnv(List(),_,_))) => { assert(toks == expected) }
            case _ => assert(false)
        }
    }
}