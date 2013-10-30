package pattern

import clock.{AppClock, Clock}

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

case class PVal[T](value: T)

class Environment extends Bindable {
  private val monitor = new Object

  val clock = new AppClock

  clock.start()

  def dispatchBinding[T](s: SymbolBinding[T]) = {
    this.dispatch(s.symbol, PVal(s.specMapping(s.value)))
  }

  private var bindings = Map[Symbol, MethodDispatch[PVal[_], Unit]]()

  //function names for deletion?
  def bind(sym: Symbol)(func: PartialFunction[PVal[_], Unit]): Unit = {
    val mdisp = bindings.get(sym).getOrElse(new MethodDispatch[PVal[_], Unit])

    mdisp += func

    bindings = bindings + (sym -> mdisp)
  }

  def clearBindings(sym: Symbol) = {
    monitor.synchronized {
      val mdisp = bindings.get(sym)
      if (mdisp.isDefined) {
        mdisp.get.clear()
      }
    }
  }

  private def dispatch[T](sym: Symbol, t: PVal[T]) = {
    monitor.synchronized {
      val mdisp = bindings.get(sym)
      if (mdisp.isDefined) {
        mdisp.get.apply(t)
      }
    }
  }

  def player[T](pattern: P0[Seq[SymbolBinding[T]]], scheduleAt: MusicalDuration) : Player[T] = {
    val ret = new Player(pattern, this, clock)

    ret
  }
}
