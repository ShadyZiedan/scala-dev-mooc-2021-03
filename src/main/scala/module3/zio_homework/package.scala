package module3

import module3.zioConcurrency.printEffectRunningTime
import zio.{Has, Task, ULayer, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */

  lazy val readInt: Task[Int] = ZIO.effect(StdIn.readInt())
  lazy val readIntOrRetry: ZIO[Console, Nothing, Int] = readInt.orElse(putStrLn("Invalid number") *> readIntOrRetry)
  lazy val guessProgram: ZIO[Console with Random, IOException, Unit] =
    for {
      _ <- putStrLn("Guess Number:")
      num <- readIntOrRetry.map(n => if (n > 3) putStrLn("Number should be between 0 and 3") *> readIntOrRetry)
      rnd <- nextIntBetween(1, 3)
      _ <- if (num == rnd) putStrLn("Good job!") else putStrLn(s"No, that wasn't correct. It was $rnd.")
    } yield ()


  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */

  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    body.repeatWhile(condition)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: ZIO[Console, Nothing, config.AppConfig] = config.load.foldM(
    ex => putStrLn(ex.toString) *> ZIO.succeed(config.AppConfig("MyApp", "www.my-app.ru")),
    data => ZIO.succeed(data))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = sleep(1.second) *> nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(effects.fold(ZIO.succeed(0))((acc, el) => acc.zipWith(el)(_ + _))).flatMap(v => putStrLn(s"Result: $v"))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(effects.fold(ZIO.succeed(0))((acc, el) => acc.zipWithPar(el)(_ + _))).flatMap(v => putStrLn(s"Result: $v"))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  import module3.logger.LoggerService

  val loggerServiceEnv: ULayer[LoggerService] =
    Clock.live ++ Console.live >>> LoggerService.live

  lazy val appWithLogger: ZIO[LoggerService with Random with Clock with Console, Nothing, Unit] = for {
    res <- LoggerService.printEffectRunningTime(eff)
    _ <- putStrLn(s"Result: $res")
  } yield ()

  lazy val runApp: ZIO[Clock with Random with Console, Nothing, Unit] = appWithLogger.provideSomeLayer[Clock with Random with Console](loggerServiceEnv)


  
}

package object logger {
  type LoggerService = Has[LoggerService.Service]

  @accessible
  object LoggerService {
    trait Service {
      def printEffectRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A]
    }

    class LoggerServiceImpl(clock: Clock.Service, console: Console.Service) extends Service {
      override def printEffectRunningTime[R, E, A](eff: ZIO[R, E, A]): ZIO[R, E, A] = for{
        start <- clock.currentTime(TimeUnit.SECONDS)
        r <- eff
        finish <- clock.currentTime(TimeUnit.SECONDS)
        _ <- console.putStrLn(s"Running time ${finish - start}")
      } yield r
    }
    val live: ZLayer[Has[Clock.Service] with Has[Console.Service], Nothing, LoggerService] =
      ZLayer.fromServices[Clock.Service, Console.Service, LoggerService.Service]((clock, console) => new LoggerServiceImpl(clock, console))
  }
}
