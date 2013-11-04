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

// types
trait BasePattern {
  def getContext() : Context = {
    Context.defaultContext
  }
}

// P*W patterns act as "proxy"!

class P0[T](protected[pattern] var func: Context => T)
  extends (() => T) with BasePattern {
  protected val monitor = new Object()

  def apply() : T = {
      this.apply(getContext())
  }

  def apply(ctx:Context) : T = {
    monitor.synchronized {
      ctx.setCurrentPattern(this)
      func(ctx)
    }
  }

  //binding
  def bind(sym: Symbol, specMapping:(T => T) = {x: T => x}) = Binding(Pattern(() => sym),this, specMapping)
  def bindSeq(syms: Seq[Symbol], specMappings:Seq[(T => T)] = Nil) = Binding(Pattern(() => syms),this, specMappings)


  //map
  def map[B](f: T=>B): P0[B] = {
    new P0[B]((ctx:Context) => f(this.apply(ctx)))
  }
}

class P0W[T](myFunc: Context => T) extends P0[T](myFunc) {
  def :=(f: P0[T]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}

class P1[T, U](protected[pattern] var func: Function2[Context, T, U])
  extends Function1[T, U] with BasePattern {
  protected val monitor = new Object()

  def apply(x: T): U = {
      this(getContext(),x)
  }

  def apply(ctx: Context,x: T): U = {
      monitor.synchronized {
        ctx.setCurrentPattern(this)
        func(ctx,x)
      }
    }

  //map
  def map[B](f: U=>B): P1[T,B] = {new P1[T,B]((ctx: Context, x:T) => f(this.apply(ctx, x)))}

  //composition
  def °(p: P0[T]) : P0[U] = {

    new P0[U]((ctx:Context) => this.apply(ctx,p.apply(ctx)))
  }
}

class P1W[T,U](myFunc: Function2[Context,T, U]) extends P1(myFunc) {
  def :=(f: P1[T, U]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}

class P2[T, U, V](protected[pattern] var func: Function3[Context, T, U, V])
  extends Function2[T, U, V] with BasePattern {
  protected val monitor = new Object()

  def apply(x: T, y: U): V = {
      this(getContext, x, y)
  }

  def apply(ctx: Context, x: T, y: U): V = {
      monitor.synchronized {
        ctx.setCurrentPattern(this)
        func(ctx, x, y)
      }
  }

  //map
  def map[B](f: V=>B): P2[T,U,B] = {new P2[T,U,B]((ctx:Context, x:T,y:U) => f(this.apply(ctx,x,y)))}

  //composition
  def °(p: P0[T]) : P1[U,V] = { new P1[U,V]((ctx: Context, x:U) => this.apply(ctx,p.apply(ctx),x))}

}

class P2W[T, U, V](myFunc: Function3[Context,T, U, V]) extends P2[T,U,V](myFunc) {
  def :=(f: P2[T, U, V]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}


class P3[T, U, V, W](protected[pattern] var func: Function4[Context, T, U, V, W])
  extends Function3[T, U, V, W] with BasePattern {

  protected val monitor = new Object()

  def apply(x: T, y: U, z: V): W = {
      this(getContext, x, y, z)
  }

  def apply(ctx: Context, x: T, y: U, z: V): W = {
    monitor.synchronized {
      ctx.setCurrentPattern(this)
      func(ctx, x, y, z)
    }
  }


  //map
  def map[B](f: W=>B): P3[T,U,V,B] = {new P3[T,U,V,B]((ctx: Context, x:T,y:U,z:V) => f(this.apply(ctx,x,y,z)))}

  //composition
  def °(p: P0[T]) : P2[U,V,W] = { new P2[U,V,W]((ctx: Context, x:U,y:V) => this.apply(ctx, p.apply(ctx),x,y))}

}

class P3W[T, U, V, W](myFunc: Function4[Context,T, U, V, W]) extends P3[T, U, V, W](myFunc) {
  def :=(f: P3[T, U, V, W]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}

class P4[T, U, V, W, X](protected[pattern] var func: Function5[Context, T, U, V, W, X])
  extends Function4[T, U, V, W, X] with BasePattern {
  protected val monitor = new Object()

  def apply(x: T, y: U, z: V, a: W): X = {
      this(getContext, x, y, z, a)
  }

  def apply(ctx: Context, x: T, y: U, z: V, a: W): X = {
    monitor.synchronized {
      ctx.setCurrentPattern(this)
      func(ctx, x, y, z, a)
    }
  }


  //map
  def map[B](f: X=>B): P4[T,U,V,W,B] = {new P4[T,U,V,W,B]((ctx: Context, x:T,y:U,z:V,a:W) => f(this.apply(ctx,x,y,z,a)))}

  //composition
  def °(p: P0[T]) : P3[U,V,W,X] = { new P3[U,V,W,X]((ctx: Context,x:U,y:V,z:W) => this.apply(ctx, p.apply(ctx),x,y,z))}

}

class P4W[T, U, V, W, X](myFunc: Function5[Context,T, U, V, W, X]) extends P4[T, U, V, W, X](myFunc) {
  def :=(f: P4[T, U, V, W, X]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}

class P5[T, U, V, W, X, Y](protected[pattern] var func: Function6[Context, T, U, V, W, X, Y])
  extends Function5[T, U, V, W, X, Y] with BasePattern {
  protected val monitor = new Object()

  def apply(x: T, y: U, z: V, a: W, b: X): Y = {
      this(getContext, x, y, z, a, b)
  }

  def apply(ctx: Context, x: T, y: U, z: V, a: W, b: X): Y = {
    monitor.synchronized {
      ctx.setCurrentPattern(this)
      func(ctx, x, y, z, a, b)
    }
  }


  //map
  def map[B](f: Y=>B): P5[T,U,V,W,X,B] = {new P5[T,U,V,W,X,B]((ctx: Context,x:T,y:U,z:V,a:W,b:X) => f(this.apply(ctx,x,y,z,a,b)))}

  //composition
  def °(p: P0[T]) : P4[U,V,W,X,Y] = { new P4[U,V,W,X,Y]((ctx: Context,x:U,y:V,z:W,a:X) => this.apply(ctx,p.apply(ctx),x,y,z,a))}

}

class P5W[T, U, V, W, X, Y](myFunc: Function6[Context,T, U, V, W, X, Y])
  extends P5[T, U, V, W, X, Y](myFunc) {

  def :=(f: P5[T, U, V, W, X, Y]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}

class P6[T, U, V, W, X, Y, Z](protected[pattern] var func: Function7[Context, T, U, V, W, X, Y, Z])
  extends Function6[T, U, V, W, X, Y, Z] with BasePattern {
  protected val monitor = new Object()

  def apply(x: T, y: U, z: V, a: W, b: X, c: Y): Z = {
      this(getContext(),x, y, z, a, b, c)
  }

  def apply(ctx: Context, x: T, y: U, z: V, a: W, b: X, c: Y): Z = {
    monitor.synchronized {
      ctx.setCurrentPattern(this)
      func(ctx,x, y, z, a, b, c)
    }
  }


  //map
  def map[B](f: Z=>B): P6[T,U,V,W,X,Y,B] = {new P6[T,U,V,W,X,Y,B]((ctx: Context, x:T,y:U,z:V,a:W,b:X,c:Y) => f(this.apply(ctx,x,y,z,a,b,c)))}

  //composition
  def °(p: P0[T]) : P5[U,V,W,X,Y,Z] = { new P5[U,V,W,X,Y,Z]((ctx: Context, x:U,y:V,z:W,a:X,b:Y) => this.apply(ctx,p.apply(ctx),x,y,z,a,b))}

}

class P6W[T, U, V, W, X, Y, Z](myFunc: Function7[Context,T, U, V, W, X, Y, Z])
  extends P6[T, U, V, W, X, Y, Z](myFunc) {

  def :=(f: P6[T, U, V, W, X, Y, Z]) = {
    monitor.synchronized {
      this.func = f.func
    }
  }
}


object Pattern {
  // implicits
  implicit def val2p0[T](v: T) = new P0((ctx:Context) => v)
  implicit def fC02p0[T](func: Function1[Context, T]) = new P0W(func)
  implicit def fC12p1[T,U](func: Function2[Context, T,U]) = new P1W(func)
  implicit def fC22p2[T,U,V](func: Function3[Context, T,U,V]) = new P2W(func)
  implicit def fC32p3[T,U,V,W](func: Function4[Context, T,U,V,W]) = new P3W(func)
  implicit def fC42p4[T,U,V,W,X](func: Function5[Context, T,U,V,W,X]) = new P4W(func)
  implicit def fC52p5[T,U,V,W,X,Y](func: Function6[Context, T,U,V,W,X,Y]) = new P5W(func)
  implicit def fC62p6[T,U,V,W,X,Y,Z](func: Function7[Context,T,U,V,W,X,Y,Z]) = new P6W(func)

  def apply[T](func: Function1[Context,T]) = new P0W[T](func)

  def apply[T, U](func: Function2[Context,T, U]) = new P1W[T, U](func)

  def apply[T, U, V](func: Function3[Context,T, U, V]) = new P2W[T, U, V](func)

  def apply[T, U, V, W](func: Function4[Context,T, U, V, W]) = new P3W[T, U, V, W](func)

  def apply[T, U, V, W, X](func: Function5[Context,T, U, V, W, X]) = new P4W[T, U, V, W, X](func)

  def apply[T, U, V, W, X, Y](func: Function6[Context,T, U, V, W, X, Y]) = new P5W[T, U, V, W, X, Y](func)

  def apply[T, U, V, W, X, Y, Z](func: Function7[Context,T, U, V, W, X, Y, Z]) = new P6W[T, U, V, W, X, Y, Z](func)

  //def apply[T](v: T) = new P0((ctx:Context) => v)

  def apply[T](func: Function0[T]) =
    new P0[T]((ctx: Context) => func())

  def apply[T, U](func: Function1[T, U]) =
    new P1[T, U]((ctx: Context, x:T) => func(x))

  def apply[T, U, V](func: Function2[T, U, V]) =
    new P2[T, U, V]((ctx: Context, x:T, y: U) => func(x,y))

  def apply[T, U, V, W](func: Function3[T, U, V, W]) =
    new P3[T, U, V, W]((ctx: Context, x:T, y: U, z:V) => func(x,y,z))

  def apply[T, U, V, W, X](func: Function4[T, U, V, W, X]) =
    new P4[T, U, V, W, X]((ctx: Context, x:T, y: U,z:V, a:W) =>func(x,y,z,a))

  def apply[T, U, V, W, X, Y](func: Function5[T, U, V, W, X, Y]) =
    new P5[T, U, V, W, X, Y]((ctx: Context, x:T, y: U,z:V, a:W,b:X) =>func(x,y,z,a,b))

  def apply[T, U, V, W, X, Y, Z](func: Function6[T, U, V, W, X, Y, Z]) =
    new P6[T, U, V, W, X, Y, Z]((ctx: Context, x:T, y: U,z:V, a:W,b:X,c:Y) =>func(x,y,z,a,b,c))
}
