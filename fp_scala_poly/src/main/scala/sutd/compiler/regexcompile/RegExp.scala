package sutd.compiler.regexcompile 

import sutd.compiler.regexcompile.Automata.*

object RegExp {
    enum RE{
        case Choice(r1:RE, r2:RE)
        case Seq(r1:RE, r2:RE)
        case Star(r:RE)
        case Epsilon
        case Letter(l:Char)
        case Phi
    }

    import RE.* 

    def eps(r:RE):Boolean = r match {
        case Choice(r1,r2) => eps(r1) || eps(r2) 
        case Seq(r1,r2)    => eps(r1) && eps(r2)
        case Star(_)       => true
        case Epsilon       => true
        case Letter(_)     => false
        case Phi           => false
    }

    def deriv(r:RE, l:Char):RE = r match {
        case Choice(r1,r2)          => Choice(deriv(r1, l), deriv(r2,l))
        case Seq(r1,r2) if eps(r1)  => Choice(Seq(deriv(r1,l), r2), deriv(r2,l))
        case Seq(r1,r2)             => Seq(deriv(r1,l), r2)
        case Star(r1)               => Seq(deriv(r1,l), Star(r1))
        case Epsilon                => Phi
        case Letter(l1) if l1 == l  => Epsilon
        case Letter(l1)             => Phi
        case Phi                    => Phi
    }

    def wordMatch(w:List[Char], r:RE):Boolean = w match {
        case Nil => eps(r)
        case (l::w1) => wordMatch(w1, deriv(r,l))
    }

    // task 1.
    given derivFSA:StateMachine[RE, Char] = new StateMachine[RE,Char] {
        def step(r:RE, l:Char):Option[RE] = Some(deriv(r,l))
        def isFinal(r:RE):Boolean = eps(r)
    }



    // task 2.1
    def isPhi(r:RE):Boolean = r match{
        case Choice(r1,r2) => isPhi(r1) && isPhi(r2)
        case Seq(r1,r2)    => isPhi(r1) || isPhi(r2)
        case Phi => true
        case _ => false
    }

    def isEps(r:RE):Boolean = r match { // it is given to you. you don't need to change.
        case Choice(r1,r2) => isEps(r1) && isEps(r2)
        case Seq(r1,r2)    => isEps(r1) && isEps(r2)
        case Star(r1)      => isPhi(r1) || isEps(r1)
        case Epsilon       => true
        case _             => false
    }

    // task 2.2

    given reOrdering:Ordering[RE] = new Ordering[RE] { // Phi < Eps < Letter < Choice < Seq < Star
        def compare(r1:RE,r2:RE):Int = (r1,r2) match {
            case (Phi, Phi) => 0 // EQ
            case (Phi, _) => -1  // Less Than
            case (Epsilon, Phi) => 1 // Greater Than
            case (Epsilon, Epsilon) => 0
            case (Epsilon, _ ) => -1
            case (Letter(_), Phi) => 1
            case (Letter(_), Epsilon) => 1
            case (Letter(l1), Letter(l2)) => l1.compare(l2)
            case (Letter(_), _) => -1
            // Choice
            case (Choice(r1,r2),Seq(r3,r4)) => -1
            case (Choice(r1,r2),Star(r3)) => -1
            case (Choice(r1,r2),_) => 1
            case (Choice(r1,r2), Choice(r3,r4)) => compare(r1,r3) match {
                case 0 => compare(r2,r4)
                case 1 => 1
                case -1 => -1
            }
            //Seq
            case (Seq(r1,r2),Star(r3)) => -1
            case (Seq(r1,r2),_) => 1
            case (Seq(r1,r2),Seq(r3,r4)) => compare(r1,r3) match {
                case 0 => compare(r2,r4)
                case 1 => 1
                case -1 => -1
            }
            case (Star(r1),Star(r2)) => compare(r1,r2)
            case (Star(r1),_) => 1
            // TODO: Fix me. 
            // implement the remaining cases.
        }
    }
    // given
    def norm(r:RE):RE = {
        val rs = normChoice(r)
        mkChoice(rs)
    }

    // given
    def normChoice(r:RE):List[RE] = r match {
        case Choice(r1,r2) => {
            val nr2 = normChoice(r2)
            val nr1 = normChoice(r1)
            rmdup((nr1 ++ nr2).sortBy(r => r))
        }
        case _ => List(normSeq(r))
    }
    // task 2.3
    def rmdup(rs:List[RE]):List[RE] = rs.distinct
    // given
    def mkChoice(rs:List[RE]):RE = rs match {
        case Nil => Phi
        case List(r) => r
        case (r::rs1) => Choice(r, mkChoice(rs1))
    }
    // given
    def normSeq(r:RE):RE = r match {
        case Seq(r1,r2) => r1 match {
            case Seq(r11,r12) => normSeq(Seq(r11, Seq(r12, r2)))
            case _ => Seq(r1, normSeq(r2))
        }
        case _ => r 
    }

    // task 2.4
    def simp1(r:RE):RE = r match {
        case Choice(r1,r2) if (isPhi(r1) && isPhi(r2)) => Phi
        case Choice(r1,r2) if (isPhi(r1)) => simp1(r2)
        case Choice(r1,r2) if (isPhi(r2)) => simp1(r1)
        case Choice(r1,r2) => norm(Choice(simp1(r1), simp1(r2)))
        case Seq(r1,r2)    if (isPhi(r1)) => Phi
        case Seq(r1, r2) if (isEps(r1)) => simp1(r2)
        case Seq(r1, r2) => normSeq(Seq(simp1(r1),simp1(r2)))
        case Star(r1) if (isPhi(r1)) => Phi
        case Star(r1) => Star(simp1(r1))
        case _             => r
    }

    def simp(r:RE):RE = {
        val r1 = simp1(r)
        if (r1 == r) { r1 } else simp(r1)
    }


    
    def sigma(r:RE):Set[Char] = r match {
        case Choice(r1, r2) => sigma(r1).union(sigma(r2))
        case Seq(r1, r2)    => sigma(r1).union(sigma(r2))
        case Star(r1)       => sigma(r1)
        case Letter(l)      => Set(l)
        case Epsilon        => Set()
        case Phi            => Set()
    }


    def build(r:RE, sig:Set[Char]):List[(RE, Char, RE)] = {
        def go(newDs:Set[RE], seenDs:Set[RE], delta:List[(RE,Char,RE)]):List[(RE,Char,RE)] = newDs.toList match {
            case Nil => delta
            case _::_ => { 
                val newDelta = newDs.toList.flatMap ( r => {
                    sig.toList.map( l => (r, l, simp(deriv(r,l))))
                })
                val nextNewDs = newDelta.map( (x:(RE,Char,RE)) => x match { 
                    case (r,l,d) => d
                }).filter( r => !seenDs.contains(r)).toSet
                val nextSeenDs = seenDs.union(newDs)
                go(nextNewDs, nextSeenDs, delta ++ newDelta)
            }
        }
        go(Set(r), Set(), List())
    }

    // task 2.5
    def compile(r:RE):StateMachine[Int, Char] = { 
        val allSymbs  = sigma(r)
        val delta     = build(r, allSymbs) // assumption, the first item in the list should contain the init state as the first item
        val allStates:List[RE] = delta.map(_._1)
        val allStatesExceptR   = allStates.filter( x => x != r )
        val table:Map[RE,Int]  = allStatesExceptR.foldLeft(Map(r -> 0))(
            (acc,s) => 
                if (!acc.contains(s)) { 
                    acc + (s -> (acc.values.max + 1) )
                } else { acc }
            )
        val delta_numeric:List[(Int, Char, Int)] = delta.map((re1,char,re2)=> {
            table.get(re1) match {
                case Some(i1) => table.get(re2) match{
                    case Some(i2) => (i1,char,i2)
                }
            }
        })
        val delta_numeric_map:Map[(Int, Char), Int] = 
            delta_numeric.foldLeft(Map():Map[(Int, Char), Int])(
                (acc, t:(Int, Char, Int)) => t match {
                    case (rn, l, dn) => acc + ((rn, l) -> dn)
                }
            )
        val final_numeric = table.toList.flatMap( p => p match { 
            case (r, i) if eps(r) => List(i)
            case (r, i)           => Nil
        }).toSet

        new StateMachine[Int, Char] {
            def step(s:Int, l:Char):Option[Int] = delta_numeric_map.get((s,l)) 
            def isFinal(state: Int): Boolean = final_numeric.contains(state)
        }
    }

}