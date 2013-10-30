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

import clock.Clock

//jeder player bekommt einen stream von einen bestimmten typen (binding)
//  jedes binding sollte eine timestamp haben (logical/abs time)
// was ist mit collections von bindings?
//jedes value aht einen zeitpunkt????
// was ist mit parallelen ereignissen???
// global value map?
// ist es egal ob values zeit haben oder bindings?? naja tupel ist tupel und nur die sind relevant?
// dann doch eehr timed tuple?
// fingertree?
// aber timed value hat mehr flexibilität?
// dann kann man aber auch automationskurven so beschreiben indem die interpolation im player ausgeführt wird
// dann sollte man aber auch bindings flexibler gestalten, also
// bind(pitch,bind(curveType1,val))) aber das sollte ja auch gehen? tuples von tuples
// case classes für binding, timing, etc?aber wrapper object ist netter? obwohl case classes can man matchen und dann
// die timestamp verändern...
// d.h. wir brauchen ein "flatmap" auf die case classes?
// wrapper class ist aber flexibler für unbekannte typen
// lists von case classes?
//
// sollte auch ohne binding funktionieren, sonst gehen keine getimten
// prozesse, die sozusagen unit zurückgeben
// oder vllt doch? also dann ist z.b. progress gebunden
// an timed[Empty]
// und dann pro player mapping

// mapping classe, symbol auf funktion1
// wie ist es mit realtime?
// fake: player mit kleinen durations in timed bindings
// oder ... andere playerclasse, die clock nicht bedient sondern
// direkt das mapping ausführt?
// oder: direkt receive channel in mapping?

// clock:
// ruft player auf, führt mappings aus, wartet, ruft player auf
// akka empfängt msg, wartet, führt mapping aus, sendet ack zurück
// akka schlecht, verstopft system

// base class dafür! next ist abstract! rest nischt...

abstract class BasePlayer[T](protected val pattern: P0[Seq[SymbolBinding[T]]],
                             protected val bindable: Bindable,
                             protected val clock: Clock) {


  private val stopped = false

  def stop() = {}

  def pause() = {}

  def play() = {
    clock.schedulePlayer(this)
  }

  def next(): Context = {
    // will be modified by execution


    val ctx = Context(clock)

    if (!stopped) {
      // execute chain
      val bindValue = pattern(ctx)
      // distribute
      bindValue.foreach(x => bindable.dispatchBinding(x))
    } else {
      ctx.invalidate
    }
    ctx
  }
}


class Player[T](pattern: P0[Seq[SymbolBinding[T]]], bindable: Bindable, clock: Clock)
  extends BasePlayer(pattern, bindable, clock) {

}
