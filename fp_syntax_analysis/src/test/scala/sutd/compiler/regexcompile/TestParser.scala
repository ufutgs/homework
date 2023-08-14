package sutd.compiler.regexcompile

import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import sutd.compiler.regexcompile.RegExp.*
import sutd.compiler.regexcompile.Parsec.*
import sutd.compiler.regexcompile.Lexer.*
import sutd.compiler.regexcompile.Parser.*
import javax.xml.stream.events.StartElement



class TestParser extends funsuite.AnyFunSuite {
    import LToken.*
    import Progress.*
    import Result.*
    import RE.*
    
    test("test parsing 1") {
        // s = [a-z]*
        val  toks:List[LToken] = List(LBracket(SrcLoc(0,1)), AlphaNum(SrcLoc(0,2),'a'), Hyphen(SrcLoc(0,3)), AlphaNum(SrcLoc(0,4),'z'), RBracket(SrcLoc(0,5)), Asterix(SrcLoc(0,6)))

        val expected = Star(Choice(Letter('a'),Choice(Letter('b'),Choice(Letter('c'),Choice(Letter('d'),Choice(Letter('e'),Choice(Letter('f'),Choice(Letter('g'),Choice(Letter('h'),Choice(Letter('i'),Choice(Letter('j'),Choice(Letter('k'),Choice(Letter('l'),Choice(Letter('m'),Choice(Letter('n'),Choice(Letter('o'),Choice(Letter('p'),Choice(Letter('q'),Choice(Letter('r'),Choice(Letter('s'),Choice(Letter('t'),Choice(Letter('u'),Choice(Letter('v'),Choice(Letter('w'),Choice(Letter('x'),Choice(Letter('y'),Letter('z')))))))))))))))))))))))))))

        parse_regex(toks) match {
            case Right(err) => assert(false)
            case Left(r) => assert( r == expected)
        }
    }
    
    test("test parsing 2") {
        // s = [ab][AC]*
        val  toks:List[LToken] = List(LBracket(SrcLoc(0,1)), AlphaNum(SrcLoc(0,2),'a'), AlphaNum(SrcLoc(0,3),'b'), RBracket(SrcLoc(0,4)), LBracket(SrcLoc(0,5)), AlphaNum(SrcLoc(0,6),'A'), AlphaNum(SrcLoc(0,7),'C'), RBracket(SrcLoc(0,8)), Asterix(SrcLoc(0,9))) 


        val expected = Seq(Choice(Letter('a'),Letter('b')),Star(Choice(Letter('A'),Letter('C')))) 
        parse_regex(toks) match {
            case Right(err) => assert(false)
            case Left(r) => assert( r == expected)
        }
    }
    

    
}