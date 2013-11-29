package pattern

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-10-29 :: 20:37
    >>  Origin: patterns
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

//FIXME: disgusting!!!

//TODO: round to clock accuracy

class MusicalTime(protected val rep: Double) extends Ordered[MusicalTime] {
  //def d256 = new MusicalTime(rep/256)
  def d128 = new MusicalTime(rep/128)
  def d64 = new MusicalTime(rep/64)
  def d32 = new MusicalTime(rep/32)
  def d16 = new MusicalTime(rep/16)
  def d8 = new MusicalTime(rep/8)
  def d4 = new MusicalTime(rep/4)
  def d2 = new MusicalTime(rep/2)

  def + (that: MusicalTime) = new MusicalTime(this.rep + that.rep)
  def - (that: MusicalTime) = new MusicalTime(this.rep - that.rep)
  def compare(that: MusicalTime): Int = this.rep.compareTo(that.rep)

  def toDouble = rep

  override def toString: String = "MusicalTime: "+rep.toString

    // ??
  //def toTicks
  //stretch
  //toTime
}

class MusicalDuration(rep: Double) extends MusicalTime(rep) {
  override def d128 = new MusicalDuration(rep/128)
  override def d64 = new MusicalDuration(rep/64)
  override def d32 = new MusicalDuration(rep/32)
  override def d16 = new MusicalDuration(rep/16)
  override def d8 = new MusicalDuration(rep/8)
  override def d4 = new MusicalDuration(rep/4)
  override def d2 = new MusicalDuration(rep/2)
  def stretch(factor: Double) = new MusicalDuration(factor*this.rep)
}

object MusicalDuration {
  def apply(v: Int) = new MusicalDuration(v)
  def apply(v: Double) = new MusicalDuration(v)
  def apply(v: Float) = new MusicalDuration(v)

  def min(l: MusicalDuration,r:MusicalDuration) : MusicalDuration= {
    if (l >= r) {
      r
    } else {
      l
    }
  }


  // not sure whether this wont lead to double triggers?
  def happens(ref: MusicalTime, pt: MusicalTime, accuracy: MusicalTime) : Boolean = {
    scala.math.abs((ref-pt).toDouble) < accuracy.toDouble
  }

  def happensRepeatedly(ref: MusicalTime, pt: MusicalTime, accuracy: MusicalTime) : Boolean = {
    scala.math.abs(ref.toDouble % pt.toDouble) < accuracy.toDouble
  }

}

//musical point in time?


//object Time {
  /*def every(time: Timing)(block: => ()) = {
    Context.getContext()
  } */

  //logical time player and for clock?
//}

/*
class Timing {

}

class Timed[T](val value:T) {
  def unapply() : T = value
  //set get time
  //sollte eigene clock haben, sodass man stretch events machen kann?
}
  */
