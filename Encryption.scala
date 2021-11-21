import java.awt.AlphaComposite
import scala.util.Random

object Encryption {
  var fake_pas = "hello"
  var upper_store:List[Int]=List()
  var key_string= "!"
  var new_number = 0
  var new_letter:String = ""
  var cyphertext:String = ""
  var new_int:Int=0
  var norm_alpha: Map[String, Int] = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5, "f" -> 6, "g" -> 7, "h" -> 8, "i" -> 9, "j" -> 10, "k" -> 11, "l" -> 12, "m" -> 13, "n" -> 14, "o" -> 15, "p" -> 16, "q" -> 17, "r" -> 18, "s" -> 19, "t" -> 20, "u" -> 21, "v" -> 22, "w" -> 23, "x" -> 24, "y" -> 25, "z" -> 26, "po" -> 0, "?"->75)
  var back_alpha: Map[Int, String]  = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d", 5 -> "e", 6 -> "f", 7 -> "g", 8 -> "h", 9 -> "i", 10 -> "j", 11 -> "k", 12 -> "l", 13 -> "m", 14 -> "n", 15 -> "o", 16 -> "p", 17 -> "q", 18 -> "r", 19 -> "s", 20 -> "t", 21 -> "u", 22 -> "v", 23 -> "w", 24 -> "x", 25 -> "y", 26 -> "z", 0 -> "po", 47->"}")
  var new_numbers:Map[Int,Int] = Map(0->9,1->8,2->7,3->6,4->5,5->4,6->3,7->2,8->1,9->0)
  var symbols:Map[String,String] = Map("!"->")" , ")"->"!","@"->"(","("->"@","#"->"*","*"->"#","$"->"&","&"->"$","%"->"^","^"->"%","`"->"~","~"->"`","-"->"+","+"->"-","_"->"=","="->"_","["->";",";"->"[","{"->":",":"->"{","]"->"'","'"->"]","}"->"}","}"->"}","."->"/","/"->".",">"->"?","?"->">", "," -> "," , "<"->"|" , "|"->"<")

  def test_uppercase(phrase:String): String={
    var new_phrase = phrase.toList
    for(letter <- new_phrase){
      if(letter.isUpper){
        upper_store = upper_store :+ new_phrase.indexOf(letter)
      }
    }
    for(number <- upper_store){
      key_string = key_string.concat(number.toString+",")
    }
    key_string
  }
  def encrypt(fake_pass:String):String = {
    test_uppercase(fake_pass)
  for (letter <- fake_pass.toLowerCase.replaceAll("\\s","?").toList) {
    if(letter.toString == "a" ||letter.toString =="b"||letter.toString =="c"||letter.toString == "d"||letter.toString =="e"||letter.toString =="f"||letter.toString == "g"||letter.toString =="h"||letter.toString =="i"||letter.toString == "j"||letter.toString =="k"||letter.toString =="l"||letter.toString == "m"||letter.toString =="n"||letter.toString =="o"||letter.toString == "p"||letter.toString =="q"||letter.toString =="r"||letter.toString == "s"||letter.toString =="t"||letter.toString =="u"||letter.toString == "v"||letter.toString =="w"||letter.toString =="x"||letter.toString == "y"||letter.toString =="z"||letter.toString =="?"){
    new_number = Math.abs(27 - norm_alpha.getOrElse(letter.toString,0))-1//number to get encrypted letter
    new_letter = back_alpha.getOrElse(new_number,null).toString // new encrypted letter
    cyphertext = cyphertext.concat(new_letter)
  }
    else if(letter.toString == "`" || letter.toString == "~" || letter.toString == "!" || letter.toString == "@" || letter.toString == "#" || letter.toString == "$" || letter.toString == "_" || letter.toString == "" || letter.toString == "%" || letter.toString == "+" || letter.toString == "'" || letter.toString == "^" || letter.toString == "[" || letter.toString == "" || letter.toString == "&" || letter.toString == "{" || letter.toString == "," || letter.toString == "*" || letter.toString == "]" || letter.toString == "<" || letter.toString == "(" || letter.toString == "}" || letter.toString == "." || letter.toString == ")" || letter.toString == "|" || letter.toString == ">" || letter.toString == "-" || letter.toString == ";" || letter.toString == "/" || letter.toString == "=" || letter.toString == ":" || letter.toString == "?" || letter.toString == "" ){
      new_letter = letter.toString
      var store = symbols.getOrElse(new_letter,"WRONG").toString
      //var diff_sym = diff_symbols.getOrElse(new_letter,0).toString
      cyphertext = cyphertext.concat(store)//.concat(diff_sym)
    }
    else{
     new_letter = letter.toString
      var store = new_numbers.getOrElse(new_letter.toInt,0).toString
      //var diff_sym = diff_symbols.getOrElse(new_letter,0).toString
      cyphertext = cyphertext.concat(store)//.concat(diff_sym)
    }
  }
    cyphertext + key_string
}
  def main(args: Array[String]): Unit = {
    println(encrypt(fake_pas))
  }
}
