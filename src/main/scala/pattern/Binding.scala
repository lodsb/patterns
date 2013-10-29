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

class BaseBinding

case class SymbolBinding(symbol: Symbol) extends BaseBinding

trait Bindable[A] {
  // TODO: set lower/upper bounds
  def apply(a: A)
}


object Binding {
  def apply[B <: BaseBinding, V](binding: P0[B], value: P0[V]) : P0[Seq[(B,V)]] =
    Pattern[Seq[(B, V)]]((ctx: Context) => Seq((binding(ctx) -> value(ctx))))

  //TODO: clear up naming .. map?
  def map[B <: BaseBinding, V](bindings: P0[Seq[B]], value: P0[V]) : P0[Seq[(B,V)]] =
    Pattern[Seq[(B, V)]](
  {
    (ctx: Context) =>
      val v = value(ctx);
      val seq = bindings(ctx) map {
        x => x -> v
      }
      seq
  }
  )

  //TODO: clear up naming .. zip?
  def zip[B <: BaseBinding, V](bindings: P0[Seq[B]], value: P0[Seq[V]]) : P0[Seq[(B,V)]] =
    Pattern[Seq[(B, V)]](
    {
      (ctx: Context) => bindings(ctx).zip(value(ctx))
    }
    )
}