package evidence

import java.util.concurrent.atomic.AtomicLong

//An abstract prompt marker
case class Marker[A](val value: Long) extends AnyVal

object Marker:
  private val counter = new AtomicLong(0)

  def fresh[A](f: Marker[A] => Ctl[A]): Ctl[A] = f(
    Marker(counter.getAndIncrement())
  )
