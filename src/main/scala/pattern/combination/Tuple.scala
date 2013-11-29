package pattern.combination

import pattern.{Context, P2, P1, P0}

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

object Tuple {
  def apply[X,Y](x:P0[X],y:P0[Y]) : P0[(X,Y)] = {
    val func = (ctx: Context) => {
      val v1 = x(ctx)
      val v2 = y(ctx)

      (v1, v2)
    }

    new P0(func)
  }

  def apply[X,Y,Z](x:P0[X],y:P0[Y], z:P0[Z]) : P0[(X,Y,Z)] = {
    val func = (ctx: Context) => {
      val v1 = x(ctx)
      val v2 = y(ctx)
      val v3 = z(ctx)

      (v1, v2, v3)
    }

    new P0(func)
  }

  def apply[X,Y,Z,A](x:P0[X],y:P0[Y], z:P0[Z], a: P0[A]) : P0[(X,Y,Z,A)] = {
    val func = (ctx: Context) => {
      val v1 = x(ctx)
      val v2 = y(ctx)
      val v3 = z(ctx)
      val v4 = a(ctx)

      (v1, v2,v3,v4)
    }

    new P0(func)
  }

  def apply[X,Y,Z,A,B](x:P0[X],y:P0[Y], z:P0[Z], a: P0[A], b: P0[B]) : P0[(X,Y,Z,A,B)] = {
    val func = (ctx: Context) => {
      val v1 = x(ctx)
      val v2 = y(ctx)
      val v3 = z(ctx)
      val v4 = a(ctx)
      val v5 = b(ctx)

      (v1, v2,v3,v4,v5)

    }

    new P0(func)
  }

  def apply[X,Y,Z,A,B,C](x:P0[X],y:P0[Y], z:P0[Z], a: P0[A], b: P0[B], c: P0[C]) : P0[(X,Y,Z,A,B,C)] = {
    val func = (ctx: Context) => {
      val v1 = x(ctx)
      val v2 = y(ctx)
      val v3 = z(ctx)
      val v4 = a(ctx)
      val v5 = b(ctx)
      val v6 = c(ctx)

      (v1, v2,v3,v4,v5, v6)

    }

    new P0(func)
  }
}