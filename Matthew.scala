import java.util._
/*
object CellType extends Enumeration {
  type CellType = Value
  val None, Black, White = Value
}
import CellType.CellType*/

object Matthew {
  def main(args: Array[String]){
    val sc = new Scanner(System.in)
    val map = new MatthewMap(sc.nextInt, sc.nextInt)
    var v = "aaa"
    while(v != "q"){
      val x = sc.nextInt
      val y = sc.nextInt
      v = sc.next
      v match {
        case "b" => map.setType(x, y, 1)
        case "w" => map.setType(x, y, 2)
        case "c" => map.allCheck
        case _ => println(map(x, y).possibility.mkString(","))
      }
      println(map)
    }
  }
}
class MatthewMap(width: Int,height: Int) {
  private val map = Array.ofDim[Cell](height, width)
  def allCheck {
    for(w <- 0 until width; h <- 0 until height)
    map(w)(h).check(this)
  }
  for(w <- 0 until width; h <- 0 until height)
    map(w)(h) = Cell(w, h, 0, Array(true, true, true, true, true, true, true))
  allCheck
  def apply(x: Int,y: Int) = {
    if(0 <= x && 0 <= y && x < width && y < height)
      map(x)(y)
    else
      Cell(x, y, 0, Array(true, false, false, false, false, false, false))
  }
  override def toString = {
    (for(h <- 0 until height)
      yield (for(w <- 0 until width) yield (map(w)(h))).mkString(",")).mkString("\n")
  }
  def setType(x: Int, y: Int, t: Int){
    map(x)(y) = Cell(x, y, t, map(x)(y).possibility)
    map(x)(y).check(this)
  }
}
case class Cell(x: Int, y: Int,t: Int = 0, possibility: Array[Boolean] = new Array[Boolean](7)){
  var route = -1
  override def toString = {
    val a = t match {
      case 0 => "  "
      case 1 => "B:"
      case 2 => "W:"
    }
    a + (if(route == -1) "?" else route.toString)
  }
  def canConnectRight =
    possibility(2) || possibility(3) || possibility(4)
  def canConnectLeft =
    possibility(2) || possibility(5) || possibility(6)
  def canConnectTop =
    possibility(1) || possibility(3) || possibility(6)
  def canConnectBottom =
    possibility(1) || possibility(4) || possibility(5)
  def connectedRight =//0 1 5 6
    !possibility(0) && !possibility(1) && !possibility(5) && !possibility(6)
  def connectedLeft = //0 1 3 4
    !possibility(0) && !possibility(1) && !possibility(3) && !possibility(4)
  def connectedTop = // 0 2 4 5
    !possibility(0) && !possibility(2) && !possibility(4) && !possibility(5)
  def connectedBottom = //0 2 3 6
    !possibility(0) && !possibility(2) && !possibility(3) && !possibility(6)
  def check(map: MatthewMap){
    val rc = map(x + 1, y)
    val lc = map(x - 1, y)
    val tc = map(x, y - 1)
    val bc = map(x, y + 1)
    //色の条件
    if(t != 0){
      possibility(0) = false
      t match {
        case 1 =>
          possibility(1) = false
          possibility(2) = false
        case 2 =>
          possibility(3) = false
          possibility(4) = false
          possibility(5) = false
          possibility(6) = false
      }
    }
    //繋がらない条件
    if(!rc.canConnectLeft){
      possibility(2) = false
      possibility(3) = false
      possibility(4) = false
    }
    if(!lc.canConnectRight){
      possibility(2) = false
      possibility(5) = false
      possibility(6) = false
    }
    if(!tc.canConnectBottom){
      possibility(1) = false
      possibility(3) = false
      possibility(6) = false
    }
    if(!bc.canConnectTop){
      possibility(1) = false
      possibility(4) = false
      possibility(5) = false
    }
    //繋がらない条件（色
    t match {
      case 0 =>
      case 1 => //black
        if(!rc.possibility(2)){
          possibility(3) = false
          possibility(4) = false
        }
        if(!lc.possibility(2)){
          possibility(5) = false
          possibility(6) = false
        }
        if(!tc.possibility(1)){
          possibility(3) = false
          possibility(6) = false
        }
        if(!bc.possibility(1)){
          possibility(4) = false
          possibility(5) = false
        }
      case 2 => // white
        if(!((rc.possibility(5) || rc.possibility(6)) || (lc.possibility(3) || lc.possibility(4))))
          possibility(2) = false
        if(!((tc.possibility(4) || tc.possibility(5)) || (bc.possibility(3) || bc.possibility(6))))
          possibility(1) = false
    }
    /*
    def connectedRight =//0 1 5 6
    !possibility(0) && !possibility(1) && !possibility(5) && !possibility(6)
  def connectedLeft = //0 1 3 4
    !possibility(0) && !possibility(1) && !possibility(3) && !possibility(4)
  def connectedTop = // 0 2 4 5
    !possibility(0) && !possibility(2) && !possibility(4) && !possibility(5)
  def connectedBottom = //0 2 3 6
    !possibility(0) && !possibility(2) && !possibility(3) && !possibility(6)
    */
    if(rc.connectedLeft){
      possibility(0) = false
      possibility(1) = false
      possibility(5) = false
      possibility(6) = false
      rc.t match {
        case 0 =>
        case 1 =>
          possibility(3) = false
          possibility(4) = false
        case 2 =>
          if(!map(x + 2, y).possibility(2))
            possibility(2) = false
      }
    }
    if(lc.connectedRight){
      possibility(0) = false
      possibility(1) = false
      possibility(3) = false
      possibility(4) = false
      lc.t match {
        case 0 =>
        case 1 =>
          possibility(5) = false
          possibility(6) = false
        case 2 =>
          if(!map(x - 2, y).possibility(2))
            possibility(2) = false
      }
    }
    if(tc.connectedBottom){
      possibility(0) = false
      possibility(2) = false
      possibility(4) = false
      possibility(5) = false
      tc.t match {
        case 0 =>
        case 1 =>
          possibility(3) = false
          possibility(6) = false
        case 2 =>
          if(!map(x , y - 2).possibility(1))
            possibility(1) = false
      }
    }
    if(bc.connectedTop){
      possibility(0) = false
      possibility(2) = false
      possibility(3) = false
      possibility(6) = false
      bc.t match {
        case 0 =>
        case 1 =>
          possibility(4) = false
          possibility(5) = false
        case 2 =>
          if(!map(x , y + 2).possibility(1))
            possibility(1) = false
      }
    }
    val c = possibility.filter(_ == true).length
    if(c == 1)
      for(number <- 0 to 6 if(possibility(number))) route = number
    if(c == 0)
     println(x+","+y+":候補がありません")
  }
}
