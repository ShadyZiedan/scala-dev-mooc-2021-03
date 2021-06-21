package module1


import java.util.UUID
import scala.annotation.tailrec


/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification
  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
  }

  trait AbiturientService{

    def registerAbiturient(uuid: String, abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(uuid: String, abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }
    def registerAbiturient2(abiturientDTO: AbiturientDTO): (Abiturient, Notification) = {
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      (abiturient, Notification.Email(abiturient.email, "Some message"))
    }

  }
}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n = _n *  i
      i = i + 1
    }
    _n
  }

  def fact2(n: Int): Int = {
    if(n <= 1) 1
    else n * fact2(n - 1)
  }

  def fact3(n: Int): Int = {

    @tailrec
    def loop(n1: Int, acc: Int): Int ={
      if(n1 <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }
    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

   def fib(n: Int): Int = fib(n -1) + fib(n - 2)

}

object hof{

  def printFactorialResult(r: Int) = println(s"Factorial result is ${r}")

  def printFibonacciResult(r: Int) = println(s"Fibonacci result is ${r}")

  def printResult(r: Int, name: String) = println(s"$name result is ${r}")


  def printFuncResult[A, B](f: A => B, v: A, name: String) =
    println(s"$name result is ${f(v)}")





  // Follow type implementation
  def partial[A,B,C](a: A, f: (A, B) => C): B => C = (b : B) => f(a, b) // B => C

  def sum(x: Int, y: Int): Int = ???

  val r: Int => Int = partial(1, sum)

  r(2) // sum(1, 2)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   // Animal
   // Dog extend Animal
  // Option[Dog] Option[Animal]

   sealed trait Option[+A]{
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    def get: A = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty list")
    }


    def getOrElse[B >: A](b: B): B = this match {
      case Option.Some(v) => v
      case Option.None => b
    }

    def map[B](f: A => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    // val i : Option[Int]  i.map(v => v + 1)


    def f(x: Int, y: Int): Option[Int] =
      if(y == 0) Option.None
      else Option.Some(x / y)

    def printIfAny(): Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None =>
    }

    def orElse[B >: A](b: Option[B]): Option[B] = this match {
      case Option.Some(_) => this
      case Option.None => b
    }

    def zip[B](b: Option[B]): Option[(A, B)] = (this, b) match {
      case (Option.Some(v1), Option.Some(v2)) => Option.Some((v1, v2))
      case _ => Option.None
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Option.Some(v) if f(v) => this
      case _ => Option.None
    }
  }

   object Option{
     case class Some[A](v: A) extends Option[A]
     case object None extends Option[Nothing]
   }


  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */

  /**
   *
   * реализовать метод orElse который будет возвращать другой Option, если данный пустой
   */


  /**
   *
   * Реализовать метод isEmpty, который будет возвращать true если Option не пуст и false в противном случае
   */


  /**
   *
   * Реализовать метод get, который будет возвращать значение
   */

  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }