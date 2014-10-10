package support


import java.util.regex.Matcher

import recorder.MyFunSuite.TestFailedException
import recorder._

import language.experimental.macros


case class TestFailed(throwable: Option[Throwable], suiteName: String, testName: String)

trait HandsOnSuite extends MyFunSuite {
  def __ : EMatcher[Any] = {
    throw new NotImplementedError("__")
  }

  implicit val suite: MyFunSuite = this


  import scala.language.implicitConversions



  trait AMatcher[+T] {
    def check[B >: T](b:B):Option[Exception]
  }

  case class EMatcher[+T](o: T) extends AMatcher[T] {
    override def check[B >: T](b: B): Option[Exception] = {
      if (b != o) {
        Option(new TestFailedException("expected " + o + " , found " + b))
      } else {
        None
      }
    }
  }

  case class CMatcher[+T](o: T) extends AMatcher[T] {
    override def check[B >: T](b: B): Option[Exception] = {
      b match {
        case b:Iterable[_] if b.toList.contains(o) => None
        case _ => Option(new TestFailedException(" " + b + " does not contain " + o))
      }

    }
  }


  object be {
    def apply[T](o: T) = EMatcher[T](o)
  }

  val equal = be


  def fail(s:String):Unit = {
    throw  new TestFailedException(s)
  }


  object contain {
    //TODO real matchers
    def apply[T](o: T) = CMatcher[T](o)
  }

  case class AnyShouldWrapper[+T](o: T) {
    def should[B >: T](m: AMatcher[B]): Unit = {
      val check: Option[Exception] = m.check(o)

      if(check.isDefined) {
        throw  check.get
      }
    }
  }


  implicit def convertToAnyShouldWrapper[T](o: T): AnyShouldWrapper[T] = new AnyShouldWrapper(o)


  def anchor[A](a: A): Unit = macro RecorderMacro.anchor[A]

  def exercice(testName: String)(testFun: Unit)(implicit suite: MyFunSuite, anchorRecorder: AnchorRecorder): Unit = macro RecorderMacro.apply


  /*override protected def test(testName: String, tags: Tag*)(testFun: => Unit):Unit


  = macro RecorderMacro.apply  */




  object reportToTheStopper {
    def info(s: String): Unit = {
      println(s)
    }

    def headerFail = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n               TEST FAILED                 \n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

    def footerFail = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

    def headerPending = "*******************************************\n               TEST PENDING                \n*******************************************"

    def footerPending = "*******************************************"

    def sendInfo(header: String, suite: String, test: String, location: Option[String], message: Option[String], context: Option[String], footer: String) {


      header.split("\n").foreach(info)

      info("Suite    : " + suite.replace("\n", ""))

      info("Test     : " + test.replace("\n", ""))

      location.collect({ case f =>
        info("fichier  : " + f.replace("\n", ""))
      })
      message.collect({ case m =>
        info("")
        m.split("\n").foreach(info)
      })
      context.collect({ case c =>
        info("")
        c.split("\n").foreach(info)
      })
      info("")
      footer.split("\n").foreach(info)
    }

    def sendFail(e: MyException, suite: String, test: String) = {
      sendInfo(headerFail
        , suite
        , test
        , e.fileNameAndLineNumber
        , Option(e.getMessage)
        , e.context
        , footerFail
      )
    }

    def sendPending(e: MyException, suite: String, test: String, mess: Option[String]) = {
      sendInfo(headerPending
        , suite
        , test
        , e.fileNameAndLineNumber
        , mess
        , e.context
        , footerPending
      )
    }



    def apply(e: TestFailed) {

      e.throwable match {
        //pour les erreurs d'assertions => sans stacktrace
        case Some(failure: MyTestFailedException) =>
          sendFail(failure, e.suiteName, e.testName)
        //pour les __ => avec context
        case Some(pending: MyTestPendingException) =>
          sendPending(pending, e.suiteName, e.testName, Some("Vous devez remplacer les __ par les valeurs correctes"))
        //pour les ??? => sans context
        case Some(pending: MyNotImplException) =>
          sendPending(pending, e.suiteName, e.testName, Some("Vous devez remplacer les ??? par les implémentations correctes"))
        //pour les autres erreurs => avec stacktrace
        case Some(failure: MyException) =>
          sendFail(failure, e.suiteName, e.testName)

        //ça non plus, un TestFailed a normalement une excepetion attachée
        case _ =>
          sendInfo(headerFail
            , e.suiteName
            , e.testName
            , None
            , None
            , None
            ,
            footerFail
          )
      }


    }
  }


}
