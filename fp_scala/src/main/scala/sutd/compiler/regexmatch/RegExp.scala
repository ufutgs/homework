package sutd.compiler.regexmatch

object RegExp {
    enum RE {
        case Choice(r1:RE, r2:RE)
        case Seq(r1:RE, r2:RE)
        case Star(r:RE)
        case Epsilon
        case Letter(l:Char)
        case Phi
    }

    import RE.* 

    def eps(r:RE):Boolean = r match
    {
       case Letter(l) => false
       case Epsilon => true
       case Phi => false
       case Star(r1) => true
       case Seq(r1,r2) => (eps(r1) && eps(r2)) 
       case Choice(r1,r2) => (eps(r1) || eps(r2))
    }

    def deriv(r:RE, l:Char):RE = r match
    {
      case Phi => Phi
      case Epsilon => Phi
      case Letter(l2) =>
      {
        if (l==l2) Epsilon else Phi
      }
      case Choice(r1,r2) => Choice(deriv(r1,l),deriv(r2,l))
      case Seq(r1, r2) => 
        {
            if(eps(r1)) Choice(Seq(deriv(r1,l),r2) , deriv(r2,l)) 
            else Seq(deriv(r1,l),r2)
        }
      case Star(r1) => Seq(deriv(r1,l),r)
    }

    def wordMatch(w:List[Char], r:RE):Boolean = w match
    {
        case Nil => eps(r)
        case l::rest => wordMatch(rest,deriv(r,l))
    }
}