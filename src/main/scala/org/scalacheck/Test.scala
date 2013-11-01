/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2013 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Prop.Arg

object Test {

  import util.{FreqMap, CmdLineParser, ConsoleReporter}

  /** Test parameters used by the [[Test.check]] method. */
  trait Parameters {
    /** The minimum number of tests that must succeed for ScalaCheck to
     *  consider a property passed. */
    val minSuccessfulTests: Int

    /** The starting size given as parameter to the generators. */
    val minSize: Int

    /** The maximum size given as parameter to the generators. */
    val maxSize: Int

    /** The initial random seed. */
    val seed: Long

    /** The random number generator used. */
    val rng: Long => Long

    /** The number of tests run in parallell. */
    val workers: Int

    /** A callback that ScalaCheck calls each time a test is executed. */
    val testCallback: TestCallback

    /** The maximum ratio between discarded and passed tests allowed before
     *  ScalaCheck gives up and discards the property. At least
     *  `minSuccesfulTests` will always be run, though. */
    val maxDiscardRatio: Float

    /** A custom class loader that should be used during test execution. */
    val customClassLoader: Option[ClassLoader]

    // private since we can't guarantee binary compatibility for this one
    private[scalacheck] def copy(
      _minSuccessfulTests: Int = Parameters.this.minSuccessfulTests,
      _minSize: Int = Parameters.this.minSize,
      _maxSize: Int = Parameters.this.maxSize,
      _seed: Long = Parameters.this.seed,
      _rng: Long => Long = Parameters.this.rng,
      _workers: Int = Parameters.this.workers,
      _testCallback: TestCallback = Parameters.this.testCallback,
      _maxDiscardRatio: Float = Parameters.this.maxDiscardRatio,
      _customClassLoader: Option[ClassLoader] = Parameters.this.customClassLoader
    ): Parameters = new Parameters {
      val minSuccessfulTests: Int = _minSuccessfulTests
      val minSize: Int = _minSize
      val maxSize: Int = _maxSize
      val seed: Long = _seed
      val rng: Long => Long = _rng
      val workers: Int = _workers
      val testCallback: TestCallback = _testCallback
      val maxDiscardRatio: Float = _maxDiscardRatio
      val customClassLoader: Option[ClassLoader] = _customClassLoader
    }
  }

  /** Test parameters used by the [[Test.check]] method.
   *
   *  To override default values, extend the
   *  [[org.scalacheck.Test.Parameters.Default]] trait:
   *
   *  {{{
   *  val myParams = new Parameters.Default {
   *    override val minSuccesfulTests = 600
   *    override val maxDiscardRatio = 8
   *  }
   *  }}}
   */
  object Parameters {
    /** Default test parameters trait. This can be overriden if you need to
     *  tweak the parameters. */
    trait Default extends Parameters {
      val minSuccessfulTests: Int = 100
      val minSize: Int = 0
      val maxSize: Int = Gen.Parameters.default.size
      val seed: Long = Gen.Parameters.default.seed
      val rng: Long => Long = Gen.Parameters.default.rng
      val workers: Int = 1
      val testCallback: TestCallback = new TestCallback {}
      val maxDiscardRatio: Float = 5
      val customClassLoader: Option[ClassLoader] = None
    }

    /** Default test parameters instance. */
    val default: Parameters = new Default {}

    /** Verbose console reporter test parameters instance. */
    val defaultVerbose: Parameters = new Default {
      override val testCallback = ConsoleReporter(2)
    }
  }

  /** Test statistics */
  case class Result(
    status: Status,
    succeeded: Int,
    discarded: Int,
    freqMap: FreqMap[Set[Any]],
    time: Long,
    seed: Long
  ) {
    def passed = status match {
      case Passed => true
      case Proved(_) => true
      case _ => false
    }
  }

  /** Test status */
  sealed trait Status

  /** ScalaCheck found enough cases for which the property holds, so the
   *  property is considered correct. (It is not proved correct, though). */
  case object Passed extends Status

  /** ScalaCheck managed to prove the property correct */
  sealed case class Proved(args: List[Arg[Any]]) extends Status

  /** The property was proved wrong with the given concrete arguments.  */
  sealed case class Failed(args: List[Arg[Any]], labels: Set[String]) extends Status

  /** The property test was exhausted, it wasn't possible to generate enough
   *  concrete arguments satisfying the preconditions to get enough passing
   *  property evaluations. */
  case object Exhausted extends Status

  /** An exception was raised when trying to evaluate the property with the
   *  given concrete arguments. */
  sealed case class PropException(args: List[Arg[Any]], e: Throwable,
    labels: Set[String]) extends Status

  /** An exception was raised when trying to generate concrete arguments
   *  for evaluating the property. */
  sealed case class GenException(e: Throwable) extends Status

  trait TestCallback { self =>
    /** Called each time a property is evaluated */
    def onPropEval(name: String, threadIdx: Int, succeeded: Int,
      discarded: Int): Unit = ()

    /** Called whenever a property has finished testing */
    def onTestResult(name: String, result: Result): Unit = ()

    def chain(testCallback: TestCallback) = new TestCallback {
      override def onPropEval(name: String, threadIdx: Int,
        succeeded: Int, discarded: Int
      ): Unit = {
        self.onPropEval(name,threadIdx,succeeded,discarded)
        testCallback.onPropEval(name,threadIdx,succeeded,discarded)
      }

      override def onTestResult(name: String, result: Result): Unit = {
        self.onTestResult(name,result)
        testCallback.onTestResult(name,result)
      }
    }
  }

  private def assertParams(prms: Parameters) = {
    import prms._
    if(
      minSuccessfulTests <= 0 ||
      maxDiscardRatio <= 0 ||
      minSize < 0 ||
      maxSize < minSize ||
      workers <= 0
    ) throw new IllegalArgumentException("Invalid test parameters")
  }

  private def secure[T](x: => T): Either[T,Throwable] =
    try { Left(x) } catch { case e: Throwable => Right(e) }

  private[scalacheck] lazy val cmdLineParser = new CmdLineParser {
    object OptMinSuccess extends IntOpt {
      val default = Parameters.default.minSuccessfulTests
      val names = Set("minSuccessfulTests", "s")
      val help = "Number of tests that must succeed in order to pass a property"
    }
    object OptMaxDiscardRatio extends FloatOpt {
      val default = Parameters.default.maxDiscardRatio
      val names = Set("maxDiscardRatio", "r")
      val help =
        "The maximum ratio between discarded and succeeded tests " +
        "allowed before ScalaCheck stops testing a property. At " +
        "least minSuccessfulTests will always be tested, though."
    }
    object OptMinSize extends IntOpt {
      val default = Parameters.default.minSize
      val names = Set("minSize", "n")
      val help = "Minimum data generation size"
    }
    object OptMaxSize extends IntOpt {
      val default = Parameters.default.maxSize
      val names = Set("maxSize", "x")
      val help = "Maximum data generation size"
    }
    object OptWorkers extends IntOpt {
      val default = Parameters.default.workers
      val names = Set("workers", "w")
      val help = "Number of threads to execute in parallel for testing"
    }
    object OptVerbosity extends IntOpt {
      val default = 1
      val names = Set("verbosity", "v")
      val help = "Verbosity level"
    }

    val opts = Set[Opt[_]](
      OptMinSuccess, OptMaxDiscardRatio, OptMinSize,
      OptMaxSize, OptWorkers, OptVerbosity
    )

    def parseParams(args: Array[String]) = parseArgs(args) {
      optMap => Parameters.default.copy(
        _minSuccessfulTests = optMap(OptMinSuccess),
        _maxDiscardRatio = optMap(OptMaxDiscardRatio),
        _minSize = optMap(OptMinSize),
        _maxSize = optMap(OptMaxSize),
        _workers = optMap(OptWorkers),
        _testCallback = ConsoleReporter(optMap(OptVerbosity))
      )
    }
  }

  /** Tests a property with the given testing parameters, and returns
   *  the test results. */
  def check(params: Parameters, p: Prop): Result = {
    import params._

    assertParams(params)
    if(workers > 1) {
      assert(!p.isInstanceOf[Commands], "Commands cannot be checked multi-threaded")
    }

    // Intermediate result
    case class R(s: Option[Status], n: Int, d: Int, fm: FreqMap[Set[Any]])

    val iterations = math.ceil(minSuccessfulTests / (workers: Double))
    val sizeStep = (maxSize-minSize) / (iterations*workers)
    val seed0 = params.seed
    var stop = false
    val genPrms = new Gen.Parameters.Default {
      override val seed = seed0
      override val rng = params.rng
    }

    def worker(workerIdx: Int): () => R =
      if (workers < 2) () => workerFun(workerIdx) 
      else spawn {
        params.customClassLoader.map(Thread.currentThread.setContextClassLoader(_))
        workerFun(workerIdx)
      }

    def spawn[A](body: => A): () => A = {
      import scala.concurrent._, ExecutionContext.Implicits.global, duration.Duration
      val future = Future(body)
      () => Await.result(future, Duration.Inf)
    }

    def workerFun(workerIdx: Int) = {
      var res = R(None, 0, 0, FreqMap.empty[Set[Any]])
      while(!stop && !res.s.isDefined && res.n < iterations) {
        val size =
          (minSize: Double) + (sizeStep * (workerIdx + (workers*(res.n+res.d))))
        val propPrms = Prop.Params(genPrms.resize(size.round.toInt), res.fm)
        secure(p(propPrms)) match {
          case Right(e) =>
            res.copy(s = Some(GenException(e)), fm = FreqMap.empty[Set[Any]])
          case Left(propRes) =>
            res = res.copy(fm =
              if(propRes.collected.isEmpty) res.fm
              else res.fm + propRes.collected
            )
            propRes.status match {
              case Prop.Undecided =>
                res = res.copy(d = res.d + 1)
                testCallback.onPropEval("", workerIdx, res.n, res.d)
                // The below condition is kind of hacky. We have to have
                // some margin, otherwise workers might stop testing too
                // early because they have been exhausted, but the overall
                // test has not.
                if (res.n+res.d > minSuccessfulTests && 
                    1+workers*maxDiscardRatio*res.n < res.d)
                  res = res.copy(s = Some(Exhausted))
              case Prop.True =>
                res = res.copy(n = res.n + 1)
                testCallback.onPropEval("", workerIdx, res.n, res.d)
              case Prop.Proof =>
                res = res.copy(s = Some(Proved(propRes.args)), n = res.n + 1)
                stop = true
              case Prop.False =>
                res = res.copy(s = Some(Failed(propRes.args,propRes.labels)))
                stop = true
              case Prop.Exception(e) =>
                res = res.copy(s = Some(PropException(propRes.args,e,propRes.labels)))
                stop = true
            }
        }
      }
      if (!res.s.isDefined) {
        if (maxDiscardRatio*res.n > res.d) res.copy(s = Some(Passed))
        else res.copy(s = Some(Exhausted))
      } else res
    }

    def mergeResults(r1: () => R, r2: () => R) = {
      val R(Some(st1), s1, d1, fm1) = r1()
      val R(Some(st2), s2, d2, fm2) = r2()
      if (st1 != Passed && st1 != Exhausted)
        () => R(Some(st1), s1+s2, d1+d2, fm1++fm2)
      else if (st2 != Passed && st2 != Exhausted)
        () => R(Some(st2), s1+s2, d1+d2, fm1++fm2)
      else {
        if (s1+s2 >= minSuccessfulTests && maxDiscardRatio*(s1+s2) >= (d1+d2))
          () => R(Some(Passed), s1+s2, d1+d2, fm1++fm2)
        else
          () => R(Some(Exhausted), s1+s2, d1+d2, fm1++fm2)
      }
    }

    val start = System.currentTimeMillis
    val results = for(i <- 0 until workers) yield worker(i)
    val R(Some(s),n,d,fm) = results.reduceLeft(mergeResults)()
    stop = true
    results foreach (_.apply())
    val res = Result(s,n,d,fm,System.currentTimeMillis-start,seed0)
    params.testCallback.onTestResult("", res)
    res
  }

  /** Check a set of properties. */
  def checkProperties(prms: Parameters, ps: Properties): Seq[(String,Result)] =
    ps.properties.map { case (name,p) =>
      val testCallback = new TestCallback {
        override def onPropEval(n: String, t: Int, s: Int, d: Int) =
          prms.testCallback.onPropEval(name,t,s,d)
        override def onTestResult(n: String, r: Result) =
          prms.testCallback.onTestResult(name,r)
      }
      val res = check(prms copy (_testCallback = testCallback), p)
      (name,res)
    }
}
