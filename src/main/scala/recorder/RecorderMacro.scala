package recorder


import reflect.macros.whitebox.Context
import org.scalatest.exceptions._
import reflect.internal.Chars
import collection.mutable.ArrayBuffer
import annotation.switch

class RecorderMacro[C <: Context](val context: C) {
  import context.universe._

  def apply(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite], anchorRecorder:context.Expr[PPrintRecorder]): context.Expr[Unit] = {

    val texts = getTexts(testFun.tree)


    val tree = q"""

        val testExpressionLineStart:Int = ${texts._2}
        val testExpressionLineEnd:Int  = ${texts._3}

        import recorder._
        MyFunSuite.testBody($testName, $suite, $anchorRecorder)($testFun)(new TestContext(
          ${texts._1}, testExpressionLineStart, testExpressionLineEnd))


     """



     context.Expr(tree)



    /*reify {



        val testExpressionLineStart:Int = context.literal(texts._2).splice

        val testExpressionLineEnd:Int  = context.literal(texts._3).splice

        MyFunSuite.testBody(testName.splice, suite.splice, anchorRecorder.splice)(testFun.splice)(new TestContext(
          context.literal(texts._1).splice, testExpressionLineStart, testExpressionLineEnd))
    }*/
  }


  def getTexts(recording:Tree):(String, Int,Int) = {
    def lines(rec : Tree):(Int,Int)  = {
      rec match {
        case Block(xs, y) => (rec.pos.line, y.pos.line)
        case _ => (rec.pos.line, rec.pos.line)
      }

    }
    val (lstart, lend) = lines(recording)

    val source = recording.pos.source


    val sourceContent:String =  source.content.mkString
    (sourceContent, lstart, lend)

  }

}


object RecorderMacro {



  def apply(context: Context)(testName: context.Expr[String])
           (testFun: context.Expr[Unit])
           (suite: context.Expr[MyFunSuite], anchorRecorder: context.Expr[PPrintRecorder]): context.Expr[Unit] = {

    new RecorderMacro[context.type](context).apply(testName)(testFun)(suite, anchorRecorder)
  }


  def pprint[T: context.WeakTypeTag](context: Context)(a : context.Expr[T]):context.Expr[Unit] = {
    import context.universe._



    val aCode = q"${show(a.tree)}"


    val line = q"${a.tree.pos.line}"

    val resultExp = reify {
      "" + a.splice
    }

    context.Expr[Unit](
      Apply(Select(Select(
        context.prefix.tree, TermName("pprintRecorder")), TermName("record")), List(aCode, line, resultExp.tree))

    )


  }
}
