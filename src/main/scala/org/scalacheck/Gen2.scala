/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2013 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import util.Buildable

sealed trait Gen2[+T] {

  import Gen2.{P, R, r, gen}

  def labels = apply(P()).labels

  //// Private interface ////

  private[scalacheck] def apply(prms: P): R[T]

  private[scalacheck] def mapR[U](f: R[T] => R[U]): Gen2[U] = gen { prms =>
    f(Gen2.this.apply(prms))
  }


  //// Public interface ////

  def map[U](f: T => U): Gen2[U] = mapR(r => r.map(f andThen Some[U]))

  def flatMap[U](f: T => Gen2[U]): Gen2[U] = gen { p =>
    apply(p).flatMap(t => f(t).apply(p))
  }

  def filter(p: T => Boolean): Gen2[T] = suchThat(p)

  def withFilter(p: T => Boolean): GenWithFilter = new GenWithFilter(p)

  final class GenWithFilter(p: T => Boolean) {
    def map[U](f: T => U): Gen2[U] = Gen2.this.suchThat(p).map(f)
    def flatMap[U](f: T => Gen2[U]): Gen2[U] = Gen2.this.suchThat(p).flatMap(f)
    def withFilter(q: T => Boolean): GenWithFilter = Gen2.this.withFilter(x => p(x) && q(x))
  }

  def suchThat(p: T => Boolean): Gen2[T] = mapR { r =>
    r.copy(s = _ match { case t:T => p(t) })
  }

  def retryUntil(p: T => Boolean): Gen2[T] = flatMap { t =>
    if (p(t)) Gen2.value(t).suchThat(p) else retryUntil(p)
  }

  def sample: Option[T] = apply(P()).retrieve

  /** Put a label on the generator to make test reports clearer */
  def label(label: String) = mapR { r => r.copy(l = r.labels + label) }

  /** Put a label on the generator to make test reports clearer */
  def :|(l: String) = label(l)

  /** Put a label on the generator to make test reports clearer */
  def |:(l: String) = label(l)

  /** Put a label on the generator to make test reports clearer */
  def :|(l: Symbol) = label(l.toString.drop(1))

  /** Put a label on the generator to make test reports clearer */
  def |:(l: Symbol) = label(l.toString.drop(1))

}

object Gen2 {

  //// Private interface ////

  private[scalacheck] case class P (
    size: Int = 100,
    rng: java.util.Random = util.StdRand
  ) {
    def resize(newSize: Int) = copy(size = newSize)

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Long. */
    def choose(l: Long, h: Long): Long = {
      if (h < l) throw new IllegalArgumentException("Invalid range")
      val d = h - l + 1
      if (d <= 0) {
        var n = rng.nextLong
        while (n < l || n > h) {
          n = rng.nextLong
        }
        n
      } else {
        l + math.abs(rng.nextLong % d)
      }
    }

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Double. */
    def choose(l: Double, h: Double) = {
      val d = h-l
      if (d < 0 || d > Double.MaxValue)
        throw new IllegalArgumentException("Invalid range")
      else if (d == 0) l
      else rng.nextDouble * (h-l) + l
    }
  }

  private[scalacheck] trait R[+T] {
    def labels: Set[String] = Set()
    def sieve[U >: T]: U => Boolean = _ => true
    def result: Option[T]

    def retrieve = result.filter(sieve)

    def copy[U >: T](
      l: Set[String] = this.labels,
      s: Any => Boolean = this.sieve,
      r: Option[U] = this.result
    ): R[U] = new R[U] {
      override def labels = l
      override def sieve[V >: U] = s
      def result = r
    }

    def map[U](f: T => Option[U]): R[U] = new R[U] {
      override def labels = R.this.labels
      def result = R.this.retrieve.flatMap(f)
    }

    def flatMap[U](f: T => R[U]): R[U] = retrieve match {
      case None => map(_ => None)
      case Some(t) => f(t).copy(l = labels ++ f(t).labels)
    }
  }

  private[scalacheck] def r[T](r: Option[T]): R[T] = new R[T] {
    def result = r
  }

  /** Generator factory method */
  private[scalacheck] def gen[T](f: P => R[T]): Gen2[T] = new Gen2[T] {
    def apply(p: P) = f(p)
  }

  //// Public interface ////

  trait Choose[T] { def choose(min: T, max: T): Gen2[T] }

  object Choose {
    implicit val chooseLong: Choose[Long] = new Choose[Long] {
      def choose(low: Long, high: Long) = if (low > high) fail else gen { p =>
        const(p.choose(low,high)).suchThat(x => x >= low && x <= high)(p)
      }
    }
    implicit val chooseInt: Choose[Int] = new Choose[Int] {
      def choose(low: Int, high: Int) = if (low > high) fail else gen { p =>
        const(p.choose(low,high).toInt).suchThat(x => x >= low && x <= high)(p)
      }
    }
    implicit val chooseByte: Choose[Byte] = new Choose[Byte] {
      def choose(low: Byte, high: Byte) = if (low > high) fail else gen { p =>
        const(p.choose(low,high).toByte).suchThat(x => x >= low && x <= high)(p)
      }
    }
    implicit val chooseShort: Choose[Short] = new Choose[Short] {
      def choose(low: Short, high: Short) = if (low > high) fail else gen { p =>
        const(p.choose(low,high).toShort).suchThat(x => x >= low && x <= high)(p)
      }
    }
    implicit val chooseChar: Choose[Char] = new Choose[Char] {
      def choose(low: Char, high: Char) = if (low > high) fail else gen { p =>
        const(p.choose(low,high).toChar).suchThat(x => x >= low && x <= high)(p)
      }
    }
    implicit val chooseFloat: Choose[Float] = new Choose[Float] {
      def choose(low: Float, high: Float) = if (low > high) fail else gen { p =>
        const(p.choose(low,high).toFloat).suchThat(x => x >= low && x <= high)(p)
      }
    }
  }

  /** A generator that always generates the given value */
  def value[T](x: T): Gen2[T] = const(x)

  /** A generator that always generates the given value */
  def const[T](x: T): Gen2[T] = gen(_ => r(Some(x))).suchThat(_ == x)

  /** A generator that never generates a value */
  def fail[T]: Gen2[T] = gen(_ => r(None)).suchThat(_ => false)

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate
   *  any value. */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen2[T] =
    c.choose(min, max)

  /** Sequences generators. If any of the given generators fails, the
   *  resulting generator will also fail. */
  def sequence[C[_],T](gs: Iterable[Gen2[T]])(implicit b: Buildable[T,C]): Gen2[C[T]] = 
    if(gs.isEmpty) fail
    else gen { p =>
      val builder = b.builder
      val xs = gs.iterator
      var none = false
      var r = xs.next.apply(p)
      while(xs.hasNext && !none) r.retrieve match {
        case Some(x) => 
          builder += x
          val r1 = xs.next.apply(p)
          r = r.flatMap(_ => r1)
        case None => none = true
      }
      r.retrieve match {
        case Some(x) if !none =>
          builder += x
          r.map(_ => Some(builder.result()))
        case _ => fail(p)
      }
    }

  /** Wraps a generator lazily. The given parameter is only evalutated once,
   *  and not until the wrapper generator is evaluated. */
  def lzy[T](g: => Gen2[T]): Gen2[T] = {
    lazy val h = g
    gen(h.apply)
  }

  /** Wraps a generator for later evaluation. The given parameter is
   *  evaluated each time the wrapper generator is evaluated. */
  def wrap[T](g: => Gen2[T]) = gen(g.apply)

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen2[T]) = gen(p => f(p.size).apply(p))

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen2[T]) = gen(p => g.apply(p.resize(s)))

  /** Picks a random value from a list */
  def oneOf[T](xs: Seq[T]): Gen2[T] =
    choose(0, xs.size-1).map(xs(_)).suchThat(xs.contains)

  /** Picks a random value from a list */
  def oneOf[T](t0: T, t1: T, tn: T*): Gen2[T] = oneOf(t0 +: t1 +: tn)

  /** Picks a random generator from a list */
  def oneOf[T](g0: Gen2[T], g1: Gen2[T], gn: Gen2[T]*): Gen2[T] =
    choose(0, gn.size+1).flatMap((g0 +: g1 +: gn).apply(_))

}
