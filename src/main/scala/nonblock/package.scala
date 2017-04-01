import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

package object nonblock {

  sealed trait Future[A] {
    private[nonblock] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown() }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] = {
    _ => new Future[A] {
      override private[nonblock] def apply(k: (A) => Unit) =
        k(a)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es => new Future[A] {
      override private[nonblock] def apply(k: (A) => Unit) =
        eval(es)(a(es)(k))
    }
  }

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

}
