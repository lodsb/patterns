package pattern.util

import annotation.tailrec

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-11-27 :: 17:57
    >>  Origin: pattern
    >>
  +3>>
    >>  Copyright (c) 2013:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */


trait Input[+T] {}

case class  Element[T](value: T) extends Input[T]
case object Empty extends Input[Nothing]
case object EOF extends Input[Nothing]


trait Iteratee[In, Out] {
  def run : Option[Out] = {
    println("run")
    this match {
      case Done(result, _) => Some(result)
      case c@Cont(k) => c(EOF) match {
        case Done(result, _) => Some(result)
        case _ => None
      }
    }
  }

  def map[NewOut](f: Out => NewOut) : Iteratee[In, NewOut]
  def flatMap[NewOut](f: Out => Iteratee[In,NewOut]) : Iteratee[In, NewOut]
  /*
  def flatten: Iteratee[In, Out] = {
    this match {
      case Done(x: Out , rI: In) => Done(x, rI)
      case Cont(x) => Cont(x)

      case Done(Done(x:Out, _), rI:In) => Done(x,rI)
      case Done(Cont(k: (Input[Int] => Iteratee[In,Out])), rI:In) => k(rI)
    }
  }
    */
 // def compose[NewOut](f: (Out) => Iteratee[Out, NewOut])

}

case class Done[In, Out](result: Out, remainingInput: Input[In]) extends Iteratee[In, Out] {

  def map[NewOut](f: (Out) => NewOut): Iteratee[In, NewOut] = Done(f(result), remainingInput)

  def flatMap[NewOut](f: (Out) => Iteratee[In, NewOut]): Iteratee[In, NewOut] = f(result) match {
    case Done(newResult,_) => Done(newResult, remainingInput)
    case Cont(k) => Cont(x => k(x))
  }

  /*def compose[NewOut](f: (Out) => Iteratee[Out, NewOut]) = {
    f(result) match {
      case Done(n,s) => Done(n,s)
      case Cont(k) => Cont(x => k(x))
    }
  } */

}

case class Cont[In, Out](k: Input[In] => Iteratee[In, Out]) extends Iteratee[In, Out] {
  def apply[EIn >: In](i: Input[In]) = k(i)
  def map[NewOut](f: (Out) => NewOut): Iteratee[In, NewOut] = Cont(x => k(x).map(f))
  def flatMap[NewOut](f: (Out) => Iteratee[In, NewOut]): Iteratee[In, NewOut] = Cont(x => k(x).flatMap(f))
  /*
  def compose[NewOut](f: (Out) => Iteratee[Out, NewOut]) = {
    Cont(x => k(x)).compose(f)
  }  */
}

trait Enumerator[A] {
  def enum[Out](iter: Iteratee[A, Out]) : Iteratee[A, Out]
}

object Enumerator {
  @tailrec
  def apply[In, Out](seq: Seq[In], iter: Iteratee[In, Out]) : Iteratee[In, Out] = {
    seq match {
      case _ if seq.isEmpty => iter
      case head::tail => iter match {
        case Done(_,_) => iter
        case c@Cont(_) => this.apply(tail, c(Element(head)))
      }
    }
  }

}

object Test extends App {

  def enumerator[In, Out](seq: Seq[In], iter: Iteratee[In, Out]) : Iteratee[In, Out] = {
    Enumerator(seq, iter)
  }

  def print[T]: Iteratee[T, Unit] = {
    def step(in: Input[T]) : Iteratee[T, Unit] = {
      in match {
        case EOF => Done((), EOF)
        case Empty => Cont(step)
        case Element(x) => println(x); Cont(step)
      }
    }

    Cont(step)
  }

  def size[T] : Iteratee[T, Int] = {
    def step(akk: Int)(in: Input[T]) : Iteratee[T, Int] = {
      println(in)
      in match {
        case EOF => Done(akk, EOF)
        case Empty=>Cont(step(akk))
        case Element(x) => println(x);Cont(step(akk+1))
      }
    }
    Cont(step(0))
  }

  def head[T] : Iteratee[T, Option[T]] = {
    def step(in: Input[T]) : Iteratee[T, Option[T]] = {
      in match {
        case EOF => Done(None, EOF)
        case Empty=>Cont(step)
        case Element(x)=>Done(Some(x), Empty)
      }
    }

    Cont(step)
  }

  def drop[T](n: Int) : Iteratee[T, Unit]= {
    def step(toGo: Int)(in: Input[T]) : Iteratee[T,Unit] ={
      in match {
        case EOF => Done((), EOF)
        case Empty=> Cont(step(toGo))
        case Element(x) => if (toGo == 0) {
            Done((), EOF)
          } else {
            Cont(step(toGo-1))
          }
        }
      }

    if (n == 0) {
      Done((), EOF)
    } else {
      Cont(step(n-1))
    }

  }

  def take[T](n: Int) : Iteratee[T, Seq[T]]= {

    def step(toGo: Int, seq: Seq[T])(in: Input[T]) : Iteratee[T,Seq[T]] ={
      in match {
        case Element(x) => if(toGo == 0) {
          Done(x +: seq, EOF)
        } else {
          Cont(step(toGo-1,x +: seq))
        }

        case EOF => Done(seq, EOF)
        case Empty=> Cont(step(toGo,seq))
      }
    }


      if (n == 0) {
        Done(Seq.empty[T], EOF)
      } else {
        Cont(step(n-1, Seq.empty[T]))
      }


  }


  println(enumerator(List(12,3,23,2,31,23,12,31,231,2), print[Int]).run)
  println(enumerator(List(12,3,23,2,31,23,12,31,231,2), size[Int]).run)

  val drop3Size = for {
    _ <- drop[Int](3)
    acc <- size[Int]
  } yield acc

  println("DROP")
    println(enumerator(List(12,3,23,2,31,23,12,31,231,2), drop[Int](2).flatMap(x => size[Int])).run)

  //println(enumerator(List(12,3,23,2,31,23,12,31,231,2), drop3Size).run)

  //println(enumerator(List( 111,2,3,4), head[Int]).run)
  //println("TAKE")
  //println(enumerator(List(12,3,23,2,31,23,12,31,231,2), take[Int](3)).run)


}






