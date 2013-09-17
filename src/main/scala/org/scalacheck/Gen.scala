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

sealed trait Gen[+T] {

  import Gen.{P, R, r, gen}


  //// Private interface ////

  protected def doApply(p: P): R[T]

  private[scalacheck] def sieve(x: Any): Boolean = true

  private[scalacheck] final def apply(p: P): R[T] = {
    val r = doApply(p)
    r.copy(s = x => r.sieve(x) && sieve(x))
  }

  private[scalacheck] final def mapR[U >: T](f: R[T] => R[U]): Gen[U] =
    mapP(p => f(this.doApply(p)))

  private[scalacheck] final def mapP[U >: T](f: P => R[U]): Gen[U] = new Gen[U] {
    def doApply(p: P) = f(p)
    override def sieve(x: Any) = Gen.this.sieve(x)
  }


  //// Public interface ////

  /** A class supporting filtered operations. */
  final class WithFilter(p: T => Boolean) {
    def map[U](f: T => U): Gen[U] = Gen.this.suchThat(p).map(f)
    def flatMap[U](f: T => Gen[U]): Gen[U] = Gen.this.suchThat(p).flatMap(f)
    def withFilter(q: T => Boolean): WithFilter = Gen.this.withFilter(x => p(x) && q(x))
  }

  def map[U](f: T => U): Gen[U] = gen { p => apply(p).map(f andThen Some[U]) }

  def flatMap[U](f: T => Gen[U]): Gen[U] = gen { p =>
    apply(p).flatMap(t => f(t).apply(p))
  }

  def filter(p: T => Boolean): Gen[T] = suchThat(p)

  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  def suchThat(p: T => Boolean): Gen[T] = new Gen[T] {
    def doApply(p: P) = Gen.this.doApply(p)
    override def sieve(x: Any) = x match { case t:T => p(t) }
  }

  def retryUntil(p: T => Boolean): Gen[T] = flatMap { t =>
    if (p(t)) Gen.const(t).suchThat(p) else retryUntil(p)
  }

  def sample: Option[T] = apply(P()).retrieve

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result, or both
   *  generators generate no result.  */
  def ==[U](g: Gen[U]) = Prop { prms =>
    (apply(prms.genPrms).retrieve, g(prms.genPrms).retrieve) match {
      case (None,None) => Prop.proved(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => Prop.proved(prms)
      case _ => Prop.falsified(prms)
    }
  }

  def !=[U](g: Gen[U]) = Prop.forAll(this)(r => Prop.forAll(g)(_ != r))

  def !==[U](g: Gen[U]) = Prop { prms =>
    (apply(prms.genPrms).retrieve, g(prms.genPrms).retrieve) match {
      case (None,None) => Prop.falsified(prms)
      case (Some(r1),Some(r2)) if r1 == r2 => Prop.falsified(prms)
      case _ => Prop.proved(prms)
    }
  }

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

object Gen {

  //// Private interface ////

  private[scalacheck] case class P (
    size: Int = 100,
    rng: java.util.Random = util.StdRand
  ) {
    def resize(newSize: Int) = copy(size = newSize)

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Long. */
    def choose(l: Long, h: Long): Option[Long] = {
      if (h < l) None
      val d = h - l + 1
      if (d <= 0) {
        var n = rng.nextLong
        while (n < l || n > h) {
          n = rng.nextLong
        }
        Some(n)
      } else {
        Some(l + math.abs(rng.nextLong % d))
      }
    }

    /** @throws IllegalArgumentException if l is greater than h, or if
     *  the range between l and h doesn't fit in a Double. */
    def choose(l: Double, h: Double): Option[Double] = {
      val d = h-l
      if (d < 0 || d > Double.MaxValue) None
      else if (d == 0) Some(l)
      else Some(rng.nextDouble * (h-l) + l)
    }
  }

  private[scalacheck] trait R[+T] {
    def labels: Set[String] = Set()
    def sieve[U >: T]: U => Boolean = _ => true
    protected def result: Option[T]

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
  private[scalacheck] def gen[T](f: P => R[T]): Gen[T] = new Gen[T] {
    def doApply(p: P) = f(p)
  }

  //// Public interface ////

  trait Choose[T] { def choose(min: T, max: T): Gen[T] }

  object Choose {
    implicit val chooseLong: Choose[Long] = new Choose[Long] {
      def choose(low: Long, high: Long) =
        gen(p => r(p.choose(low,high))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseInt: Choose[Int] = new Choose[Int] {
      def choose(low: Int, high: Int) =
        gen(p => r(p.choose(low,high).map(_.toInt))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseByte: Choose[Byte] = new Choose[Byte] {
      def choose(low: Byte, high: Byte) =
        gen(p => r(p.choose(low,high).map(_.toByte))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseShort: Choose[Short] = new Choose[Short] {
      def choose(low: Short, high: Short) =
        gen(p => r(p.choose(low,high).map(_.toShort))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseChar: Choose[Char] = new Choose[Char] {
      def choose(low: Char, high: Char) =
        gen(p => r(p.choose(low,high).map(_.toChar))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseDouble: Choose[Double] = new Choose[Double] {
      def choose(low: Double, high: Double) =
        gen(p => r(p.choose(low,high))).suchThat(x => x >= low && x <= high)
    }
    implicit val chooseFloat: Choose[Float] = new Choose[Float] {
      def choose(low: Float, high: Float) =
        gen(p => r(p.choose(low,high).map(_.toFloat))).suchThat(x => x >= low && x <= high)
    }
  }


  //// Various Generator Combinators ////

  /** A generator that always generates the given value */
  implicit def value[T](x: T): Gen[T] = const(x)

  /** A generator that always generates the given value */
  def const[T](x: T): Gen[T] = gen(_ => r(Some(x))).suchThat(_ == x)

  /** A generator that never generates a value */
  def fail[T]: Gen[T] = gen(_ => r(None)).suchThat(_ => false)

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate
   *  any value. */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen[T] =
    c.choose(min, max)

  /** Sequences generators. If any of the given generators fails, the
   *  resulting generator will also fail. */
  def sequence[C[_],T](gs: Iterable[Gen[T]])(implicit b: Buildable[T,C]): Gen[C[T]] =
    if(gs.isEmpty) fail
    else {
      val g = gen { p =>
        val builder = b.builder
        val git = gs.iterator
        var failed = false
        var r = git.next.apply(p)
        while(git.hasNext && !failed) r.retrieve match {
          case Some(x) =>
            builder += x
            r = r.flatMap(_ => git.next.apply(p))
          case None => failed = true
        }
        r.retrieve match {
          case Some(x) if !failed =>
            builder += x
            r.map(_ => Some(builder.result()))
          case _ => fail(p)
        }
      }
      val sieve = gs.map(_.sieve _).fold((_:T) => false) { case (f1,f2) =>
        x:T => f1(x) || f2(x)
      }
      //g.suchThat(_.forall(sieve))
      g
    }

  /** Wraps a generator lazily. The given parameter is only evalutated once,
   *  and not until the wrapper generator is evaluated. */
  def lzy[T](g: => Gen[T]): Gen[T] = {
    lazy val h = g
    gen(h.apply)
  }

  /** Wraps a generator for later evaluation. The given parameter is
   *  evaluated each time the wrapper generator is evaluated. */
  def wrap[T](g: => Gen[T]) = gen(g.apply)

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen[T]) = gen(p => f(p.size).apply(p))

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen[T]) = gen(p => g.apply(p.resize(s)))

  /** Picks a random value from a list */
  def oneOf[T](xs: Seq[T]): Gen[T] =
    choose(0, xs.size-1).map(xs(_)).suchThat(xs.contains)

  /** Picks a random value from a list */
  def oneOf[T](t0: T, t1: T, tn: T*): Gen[T] = oneOf(t0 +: t1 +: tn)

  /** Picks a random generator from a list */
  def oneOf[T](g0: Gen[T], g1: Gen[T], gn: Gen[T]*): Gen[T] = {
    val gs = g0 +: g1 +: gn
    choose(0,gs.size-1).flatMap(gs.apply(_)).suchThat(x => gs.exists(_.sieve(x)))
  }

  /** Chooses one of the given generators with a weighted random distribution */
  def frequency[T](gs: (Int,Gen[T])*): Gen[T] = {
    def tot = (gs.map(_._1) :\ 0) (_+_)

    def pick(n: Int, l: List[(Int,Gen[T])]): Gen[T] = l match {
      case Nil => fail
      case (k,g)::gs => if(n <= k) g else pick(n-k, gs)
    }

    choose(1,tot).flatMap(pick(_, gs.toList)).suchThat { x =>
      gs.exists(_._2.sieve(x))
    }
  }


  //// List Generators ////

  /** Generates a container of any type for which there exists an implicit
   *  [[org.scalacheck.util.Buildable]] instance. The elements in the container will
   *  be generated by the given generator. The size of the generated container
   *  is given by `n`. If the given generator fails generating a value, the
   *  complete container gnerator will also fail. */
  def containerOfN[C[_],T](n: Int, g: Gen[T])(implicit b: Buildable[T,C]
  ): Gen[C[T]] = {
    val g2 = gen { p =>
      val builder = b.builder
      builder.sizeHint(n)
      var failed = false
      var i = 1
      while(!failed && i < n) g.apply(p).retrieve match {
        case None => failed = true
        case Some(x) => builder += x
      }
      if(failed) fail(p)
      else if(n <= 0) r(Some(builder.result()))
      else {
        val r = g.apply(p)
        r.map(_ => Some(builder.result()))
      }
    }
    // TODO g2.suchThat(_.forall(g.sieve))
    g2
  }

  /** Generates a container of any type for which there exists an implicit
   *  [[org.scalacheck.util.Buildable]] instance. The elements in the container
   *  will be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf[C[_],T](g: Gen[T])(implicit b: Buildable[T,C]): Gen[C[T]] =
    sized(size => choose(0,size).flatMap(containerOfN[C,T](_,g)))

  /** Generates a non-empty container of any type for which there exists an
   *  implicit [[org.scalacheck.util.Buildable]] instance. The elements in the container
   *  will be generated by the given generator. The size of the container is
   *  bounded by the size parameter used when generating values. */
  def containerOf1[C[_],T](g: Gen[T])(implicit b: Buildable[T,C]): Gen[C[T]] =
    sized(size => choose(1,size).flatMap(containerOfN[C,T](_,g)))

  /** Generates a list of random length. The maximum length depends on the
   *  size parameter. This method is equal to calling
   *  `containerOf[List,T](g)`. */
  def listOf[T](g: => Gen[T]) = containerOf[List,T](g)

  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter. This method is equal to calling
   *  `containerOf1[List,T](g)`. */
  def listOf1[T](g: => Gen[T]) = containerOf1[List,T](g)

  /** Generates a list of the given length. This method is equal to calling
   *  `containerOfN[List,T](n,g)`. */
  def listOfN[T](n: Int, g: Gen[T]) = containerOfN[List,T](n,g)

  /** A generator that picks a random number of elements from a list */
  def someOf[T](l: Iterable[T]) = choose(0,l.size).flatMap(pick(_,l))

  /** A generator that picks a random number of elements from a list */
  def someOf[T](g1: Gen[T], g2: Gen[T], gs: Gen[T]*) =
    choose(0, gs.length+2).flatMap(pick(_, g1, g2, gs: _*))

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, l: Iterable[T]): Gen[Seq[T]] =
    if(n > l.size || n < 0) fail
    else (gen { p =>
      val b = new collection.mutable.ListBuffer[T]
      b ++= l
      while(b.length > n) b.remove(choose(0, b.length-1).apply(p).retrieve.get)
      r(Some(b))
    }).suchThat(_.forall(x => l.exists(x == _)))

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, g1: Gen[T], g2: Gen[T], gn: Gen[T]*): Gen[Seq[T]] = {
    val gs = g1 +: g2 +: gn
    pick(n, 0 until gs.size).flatMap(idxs =>
      sequence[List,T](idxs.toList.map(gs(_)))
    ).suchThat(_.forall(x => gs.exists(_.sieve(x))))
  }


  //// Character Generators ////

  /* Generates a numerical character */
  def numChar: Gen[Char] = choose(48.toChar, 57.toChar)

  /* Generates an upper-case alpha character */
  def alphaUpperChar: Gen[Char] = choose(65.toChar, 90.toChar)

  /* Generates a lower-case alpha character */
  def alphaLowerChar: Gen[Char] = choose(97.toChar, 122.toChar)

  /* Generates an alpha character */
  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))

  /* Generates an alphanumerical character */
  def alphaNumChar = frequency((1,numChar), (9,alphaChar))


  //// String Generators ////

  /* Generates a string that starts with a lower-case alpha character,
   * and only contains alphanumerical characters */
  def identifier: Gen[String] = for {
    c <- alphaLowerChar
    cs <- listOf(alphaNumChar)
  } yield (c::cs).mkString // TODO suchThat

  /* Generates a string of alpha characters */
  def alphaStr: Gen[String] = listOf(alphaChar).map(_.mkString) // TODO suchThat

  /* Generates a string of digits */
  def numStr: Gen[String] = listOf(numChar).map(_.mkString) // TODO suchThat


  //// Number Generators ////

  /** Generates positive numbers of uniform distribution, with an
   *  upper bound of the generation size parameter. */
  def posNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(one, fromInt(max)))
  }

  /** Generates negative numbers of uniform distribution, with an
   *  lower bound of the negated generation size parameter. */
  def negNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(-fromInt(max), -one))
  }

  /** Generates numbers within the given inclusive range, with
   *  extra weight on zero, +/- unity, both extremities, and any special
   *  numbers provided. The special numbers must lie within the given range,
   *  otherwise they won't be included. */
  def chooseNum[T](minT: T, maxT: T, specials: T*)(
    implicit num: Numeric[T], c: Choose[T]
  ): Gen[T] = {
    import num._
    val basics = List(minT, maxT, zero, one, -one)
    val basicsAndSpecials = for {
      t <- specials ++ basics if t >= minT && t <= maxT
    } yield (1, value(t))
    val allGens = basicsAndSpecials ++ List(
      (basicsAndSpecials.length, c.choose(minT, maxT))
    )
    frequency(allGens: _*)
  }
}
