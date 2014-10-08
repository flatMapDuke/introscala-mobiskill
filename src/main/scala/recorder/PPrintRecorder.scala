package recorder

/**
 * Created with IntelliJ IDEA.
 * User: jon
 * Date: 3/26/13
 * Time: 11:49 PM
 * To change this template use File | Settings | File Templates.
 */
class PPrintRecorder {

  var records:List[PPrintValue] = Nil


  def record(name:String, line:Int, value:String):Unit =  {
    records = PPrintValue(name, line, value) :: records
  }

  def reset() {
    records = Nil
  }
}

case class PPrintValue(name:String, line:Int, value:String)  {

  def toMessage():String = s" anchor line $line | $name => $value"
}
