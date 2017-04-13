import math._
//1
class Complexe(val re: Double = 0, val img:Double = 0) {

  //2
  override def toString: String = (re,img,img <0) match {
    case(0,0,_) => s"$re"
    case(0,_,false) => s"i$img"
    case(0,_,true) => val mimg = -img
      s"-i$mimg"
    case(_,0,_) => s"$re"
    case(_,_,false) => s"$re+i*$img"
    case(_,_,true) => val mimg = -img
      s"$re-i*$mimg"
  }
  //4
  def module = hypot(img,re)
  def argument = {
    if(re > 0) 2*atan2(img,re+module)
    else if(re < 0) Math.PI + atan2(-1*img,re)
    else Math.PI / 2
  }

  def conjugue ={
    val denominateur = pow(re,2)+pow(img,2)
    Complexe(re/denominateur,-img/denominateur)
  }
  //5
  def +(autre: Complexe) = new Complexe(re+autre.re, img+autre.img)
  def -(autre: Complexe) = new Complexe(re-autre.re, img-autre.img)
  def *(autre: Complexe) = new Complexe((re*autre.re)-(img*autre.img), (re*autre.img)+(img*autre.re))
  def /(autre: Complexe) = this*autre.conjugue
  def unary_- = new Complexe(-re, -img)

  //7
  def <(autre: Complexe) = module < autre.module

  //8
  def ==(autre: Complexe) = (re == autre.re) && (img == autre.img)

  //9
  def max(c2: Complexe) = {
    if(module > c2.module) module
    else c2.module
  }

  //10 rotation
  def rotation(angle: Double) = {
    this * Complexe(cos(angle.toRadians), sin(angle.toRadians))
  }

}
//3
object Complexe{
  def apply(a: Double =0 , b: Double=0) = new Complexe(a,b)
  //6
  implicit def double2Complexe(d:Double) : Complexe = new Complexe(d,0)
}