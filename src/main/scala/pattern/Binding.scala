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

// identity mapped
case class SymbolBinding[T](symbol: Symbol, value: T, specMapping: (T => T) = {x:T => x} ) extends BaseBinding

trait Bindable {
  // TODO: set lower/upper bounds
  def dispatchBinding[T](a: SymbolBinding[T])
}

// TODO: FIX THIS according to symbol binding, also how to concatenate seq-bindings?
object Binding {

  def apply[V](sym: P0[Symbol], value: P0[V], specMapping:(V => V) = {x: V => x}) : P0[Seq[SymbolBinding[V]]] = {
    Pattern[Seq[SymbolBinding[V]]]( (ctx: Context) => Seq(SymbolBinding(sym(ctx), value(ctx), specMapping)))
  }

  def apply[V](syms: P0[Seq[Symbol]], value: P0[V], specMappings:Seq[(V => V)] = Nil) : P0[Seq[SymbolBinding[V]]] = {
    if (specMappings.isEmpty) {
      Pattern[Seq[SymbolBinding[V]]](
          {
            (ctx: Context) =>

            val v = value(ctx)
            val s = syms(ctx)
            s.map {x => SymbolBinding(x,v)}
          })
    } else {
      Pattern[Seq[SymbolBinding[V]]](
      {
        (ctx: Context) =>

        val v = value(ctx)
        val s = syms(ctx)

         s.zip(specMappings).map {x=> SymbolBinding(x._1, v, x._2)}
      })
    }
  }

  /*

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
    )*/
}
