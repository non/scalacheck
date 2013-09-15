package org.scalacheck

sealed trait Gen2[+T] {

  import Gen2.{Params, R, r, gen}

  //// Private interface ////

  private[scalacheck] def apply(prms: Params): R[T]

  private[scalacheck] def mapR[U](f: R[T] => R[U]): Gen2[U] = gen { prms =>
    f(Gen2.this.apply(prms))
  }


  //// Public interface ////

  def map[U](f: T => U): Gen2[U] = mapR(r => r.map(f))

  def flatMap[U](f: T => Gen2[U]): Gen2[U] = gen { prms =>
    val r0 = apply(prms)
    r0.retrieve match {
      case Some(t) =>
        val r1 = f(t).apply(prms)
        r1.copy(l = r0.labels ++ r1.labels)
      case None => r0.flatMap(_ => None)
    }
  }

  def suchThat(p: T => Boolean): Gen2[T] = mapR { r =>
    r.copy(s = _ match { case t:T => r.sieve(t) && p(t) })
  }

  def sample: Option[T] = apply(Params()).retrieve

  /** Put a label on the generator to make test reports clearer */
  def label(label: String) = mapR { r => r.copy(l = label::r.labels) }

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

  private[scalacheck] case class Params (
    size: Int = 100,
    rng: java.util.Random = util.StdRand
  ) {
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
  }

  private[scalacheck] trait R[+T] {
    def labels: List[String] = Nil
    def sieve[U >: T]: U => Boolean = _ => true
    def result: Option[T]

    def retrieve = result.filter(sieve)

    def copy[U >: T](
      l: List[String] = this.labels,
      s: Any => Boolean = this.sieve,
      r: Option[U] = this.result
    ): R[U] = new R[U] {
      override def labels = l
      override def sieve[V >: U] = s
      def result = r
    }

    def map[U](f: T => U): R[U] = new R[U] {
      override def labels = R.this.labels
      def result = R.this.retrieve.map(f)
    }

    def flatMap[U](f: T => Option[U]): R[U] = new R[U] {
      override def labels = R.this.labels
      def result = R.this.result.flatMap(f)
    }
  }

  private[scalacheck] def r[T](r: Option[T]): R[T] = new R[T] {
    def result = r
  }

  private[scalacheck] def gen[T](f: Params => R[T]) = new Gen2[T] {
    def apply(prms: Params) = f(prms)
  }

  /** Creates a generator that can access its generation parameters */
  private[scalacheck] def parameterized[T](f: Params => Gen2[T]): Gen2[T] =
    gen(prms => f(prms)(prms))


  //// Public interface ////

  trait Choose[T] {
    def choose(min: T, max: T): Gen2[T]
  }

  object Choose {
    implicit val chooseLong: Choose[Long] = new Choose[Long] {
      def choose(low: Long, high: Long) = {
        val g =
          if (low > high) fail
          else parameterized(prms => value(prms.choose(low,high)))
        g.suchThat(x => x >= low && x <= high)
      }
    }
  }

  def value[T](x: T): Gen2[T] = gen(_ => r(Some(x)))

  /** A generator that never generates a value */
  def fail[T]: Gen2[T] = gen(_ => r(None))

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate any value.
   */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen2[T] = {
    c.choose(min, max)
  }

  def check[T](t: T, g: Gen2[T]): Boolean = {
    g.apply(Params()).sieve(t)
  }

  def labels[T](g: Gen2[T]): List[String] = {
    g.apply(Params()).labels
  }
}
