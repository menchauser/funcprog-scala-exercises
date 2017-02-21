
import java.util.concurrent._

package object parallel {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)


    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))


    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = pa(es)
        val bf = pb(es)
        Map2Future(af, bf, f)
      }


    def map[A, B](a: Par[A])(f: A => B): Par[B] =
      map2(a, unit(()))((a, _) => f(a))


    def fork[A](a: => Par[A]): Par[A] =
      (es: ExecutorService) => {
        es.submit(new Callable[A] {
          override def call: A = a(es).get
        })
      }


    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)


    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(Nil: List[A]))((pa, z) => map2(pa, z)(_ :: _))


    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }


    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      asyncF((l: List[A]) => l.filter(f))(as)


    def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars = as.map(asyncF((a: A) => if (f(a)) List(a) else Nil))
      map(sequence(pars))(_.flatten)
    }


    def forkJoin[A, B](is: IndexedSeq[A])(default: A)(f: (A, A) => A): Par[A] = {
      if (is.size <= 1)
        unit(is.headOption.getOrElse(default))
      else {
        val (l, r) = is.splitAt(is.size / 2)
        map2(
          fork(forkJoin(l)(default)(f)),
          fork(forkJoin(r)(default)(f))
        )(f)
      }
    }


    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


    def equal[A](e: ExecutorService)(lp: Par[A], rp: Par[A]): Boolean =
      lp(e).get() == rp(e).get()


    private case class UnitFuture[A](get: A) extends Future[A] {
      override def get(timeout: Long, unit: TimeUnit): A = get

      override def cancel(evenIfRunning: Boolean): Boolean = false

      override def isDone: Boolean = true

      override def isCancelled: Boolean = false
    }


    private case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                           f: (A, B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None

      override def isDone: Boolean = cache.isDefined

      override def isCancelled: Boolean = a.isCancelled || b.isCancelled

      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

      override def get(): C = compute(Long.MaxValue)

      override def get(timeout: Long, unit: TimeUnit): C =
        compute(unit.toNanos(timeout))

      private def compute(timeoutNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime()
          val ar = a.get(timeoutNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime()
          val aTime = stop - start
          val br = b.get(timeoutNanos - aTime, TimeUnit.NANOSECONDS)
          val res = f(ar, br)
          cache = Some(res)
          res
      }
    }

  }


  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l) + sum(r)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum2(l), sum2(r))(_ + _)
    }
  }

  def sum3(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))(_ + _)
    }
  }


  def sum4(ints: IndexedSeq[Int]): Par[Int] =
    Par.forkJoin(ints)(0)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    Par.forkJoin(ints)(Int.MinValue)(_ max _)

}
