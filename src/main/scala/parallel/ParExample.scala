package parallel

import java.util.concurrent.{Executors, TimeUnit}

object ParExample extends App {
  val is = Array(3, 2, 1, 4)

  println(sum(is))

  val service = Executors.newFixedThreadPool(10)

  println(sum2(is)(service).get())
  println(sum3(is)(service).get())
  println(sum4(is)(service).get())
  println(max(is)(service).get())

  service.shutdown()
  service.awaitTermination(5, TimeUnit.SECONDS)
}
