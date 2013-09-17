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


  //// Private interface ////

  private[scalacheck] def apply(prms: P): R[T]

  private[scalacheck] def mapR[U](f: R[T] => R[U]): Gen2[U] = gen { prms =>
    f(Gen2.this.apply(prms))
  }


  //// Public interface ////

  /** A class supporting filtered operations. */
  final class WithFilter(p: T => Boolean) {
    def map[U](f: T => U): Gen2[U] = Gen2.this.suchThat(p).map(f)
    def flatMap[U](f: T => Gen2[U]): Gen2[U] = Gen2.this.suchThat(p).flatMap(f)
    def withFilter(q: T => Boolean): WithFilter = Gen2.this.withFilter(x => p(x) && q(x))
  }

  def map[U](f: T => U): Gen2[U] = mapR(r => r.map(f andThen Some[U]))

  def flatMap[U](f: T => Gen2[U]): Gen2[U] = gen { p =>
    apply(p).flatMap(t => f(t).apply(p))
  }

  def filter(p: T => Boolean): Gen2[T] = suchThat(p)

  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  def suchThat(p: T => Boolean): Gen2[T] = mapR(_.copy(s = p))

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
      s: U => Boolean = this.sieve,
      r: Option[U] = this.result
    ): R[U] = new R[U] {
      override def labels = l
      override def sieve[V >: U] = { x:Any => x match { case u:U => s(u) } }
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


  //// Various Generator Combinators ////

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
          r = r.flatMap(_ => r1).copy(s = t => r.sieve(t) || r1.sieve(t))
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

  /** Chooses one of the given generators with a weighted random distribution */
  def frequency[T](gs: (Int,Gen2[T])*): Gen2[T] = {
    def tot = (gs.map(_._1) :\ 0) (_+_)

    def pick(n: Int, l: List[(Int,Gen2[T])]): Gen2[T] = l match {
      case Nil => fail
      case (k,g)::gs => if(n <= k) g else pick(n-k, gs)
    }

    choose(1,tot).flatMap(n => pick(n, gs.toList)).suchThat(_ => true)
  }


  //// List Generators ////

  /** Generates a container of any type for which there exists an implicit
   *  [[org.scalacheck.util.Buildable]] instance. The elements in the container will
   *  be generated by the given generator. The size of the generated container
   *  is given by `n`. If the given generator fails generating a value, the
   *  complete container gnerator will also fail. */
  def containerOfN[C[_],T](n: Int, g: Gen2[T])(implicit b: Buildable[T,C]
  ): Gen2[C[T]] = gen { p =>
    val builder = b.builder
    builder.sizeHint(n)
    var failed = false
    var i = 1
    while(!failed && i < n) g(p).retrieve match {
      case None => failed = true
      case Some(x) => builder += x 
    }
    if(failed) fail(p)
    else if(n <= 0) r(Some(builder.result()))
    else {
      val r = g(p)
      r.map(_ => Some(builder.result())) //.copy(s = c => c.forall(r.sieve))
    }
  }

  /** Generates a container of any type for which there exists an implicit
   *  [[org.scalacheck.util.Buildable]] instance. The elements in the container
   *  will be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf[C[_],T](g: Gen2[T])(implicit b: Buildable[T,C]): Gen2[C[T]] =
    sized(size => for(n <- choose(0,size); c <- containerOfN[C,T](n,g)) yield c)

  /** Generates a non-empty container of any type for which there exists an
   *  implicit [[org.scalacheck.util.Buildable]] instance. The elements in the container
   *  will be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf1[C[_],T](g: Gen2[T])(implicit b: Buildable[T,C]): Gen2[C[T]] =
    sized(size => for(n <- choose(1,size); c <- containerOfN[C,T](n,g)) yield c)

  /** Generates a list of random length. The maximum length depends on the
   *  size parameter. This method is equal to calling
   *  `containerOf[List,T](g)`. */
  def listOf[T](g: => Gen2[T]) = containerOf[List,T](g)

  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter. This method is equal to calling
   *  `containerOf1[List,T](g)`. */
  def listOf1[T](g: => Gen2[T]) = containerOf1[List,T](g)

  /** Generates a list of the given length. This method is equal to calling
   *  `containerOfN[List,T](n,g)`. */
  def listOfN[T](n: Int, g: Gen2[T]) = containerOfN[List,T](n,g)

  /** A generator that picks a random number of elements from a list */
  def someOf[T](l: Iterable[T]) = choose(0,l.size) flatMap (pick(_,l))

  /** A generator that picks a random number of elements from a list */
  def someOf[T](g1: Gen2[T], g2: Gen2[T], gs: Gen2[T]*) = for {
    n <- choose(0, gs.length+2)
    x <- pick(n, g1, g2, gs: _*)
  } yield x

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, l: Iterable[T]): Gen2[Seq[T]] =
    if(n > l.size || n < 0) fail
    else (gen { prms => 
      val buf = new collection.mutable.ListBuffer[T]
      buf ++= l
      while(buf.length > n) {
        val g = choose(0, buf.length-1)
        buf.remove(g(prms).result.get)
      }
      r(Some(buf))
    }).suchThat(x => l.exists(_ == x))

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, g1: Gen2[T], g2: Gen2[T], gs: Gen2[T]*): Gen2[Seq[T]] = for {
    // TODO suchThat?
    is <- pick(n, 0 until (gs.size+2))
    allGs = gs ++ (g1::g2::Nil)
    xs <- sequence[List,T](is.toList.map(allGs(_)))
  } yield xs
}
