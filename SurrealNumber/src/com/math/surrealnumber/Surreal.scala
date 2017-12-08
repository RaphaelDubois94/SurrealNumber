
package com.math.surrealnumber

object v extends Surreal  {}


class Surreal (l: Surreal, r: Surreal, name: String="NoName") {

  val left: Surreal=l
  val right: Surreal=r
  def this() = this( v, v,"Empty")
  val depht: Int = {
     if ((left!=v) || (right!=v)) {
      if (left==v) right.depht+1
        else if (right==v) left.depht+1
        else left.depht+1 // or l is the same...
      } else 0 
  } 
  /* Surreal exception left > right */
  if (Surreal.supEgal(l,r)) throw new SurrealException("left > right")

  override def toString=name
}

object Surreal {
  
  val zero : Surreal = { new Surreal(v,v,"0") }
  val one : Surreal = { new Surreal(zero,v,"1") }
  val mone : Surreal = { new Surreal(v,zero,"-1") }
  val mtwo : Surreal = { new Surreal(v,mone,"-2") }
  val mthree : Surreal = { new Surreal(v,mtwo,"-3") }
  val two : Surreal = { new Surreal(one,v,"2") }
  val three : Surreal = { new Surreal(two,v,"3") }
    /**
   * supEgal
   */
  def supEgal(x:  Surreal ,  y: Surreal) : Boolean = {
			/* page 8: Si XG ou XD est vide, la condition (XG non sup ou egal XD) est vraie quelque soit le
			contenu de l'autre ensemble. */
		  if ((x==v) ||  (y==v)) return false
		  (( ! supEgal(y,x.right)) && (! supEgal(y.left,x))) 
  }
	/**
	* Egalité de deux surreels
	* Voir: http://fr.wikipedia.org/wiki/Nombre_surr%C3%A9el_et_pseudo-r%C3%A9el#Ordre 
	*/ 
  def egal(x: Surreal, y: Surreal) : Boolean = {
				supEgal(x, y) && supEgal(y, x)
	}
    
  
  def max(a: Surreal, b:Surreal): Surreal = {
        if (a == v ) b
        else if ( b== v) a
        else if (supEgal(a, b)) a
			  else b
	}
  def min(a: Surreal, b:Surreal): Surreal = {
        if (a == v ) b
        else if ( b == v) a
        else if (supEgal(a, b))	 b
			  else a
	}
  /*
   * negatif
   */
  
  def neg(x: Surreal): Surreal = {
    if (x==v) v
    else
       new Surreal(neg(x.right),neg(x.left),"-"+x);		
	}
  
   
  /**
   * Sum
   */
  def add(x: Surreal, y:Surreal): Surreal = {
  	if ((x == v) || (y == v) ) 
  	  v
  	else {
	    val s1: Surreal = add(x.left  , y      )
	    val s2: Surreal = add(x       , y.left )
	     val s3: Surreal = add(x       , y.right)
	     val s4: Surreal = add(x.right , y      )
	    new Surreal(max(s1,s2),min(s3,s4),x+"+"+y)
  	}
  }
   /**
   * mult
   */
  def mult(x: Surreal, y:Surreal): Surreal = {
  	
  	
  	if ( egal(zero,x)) zero
  	else if ( egal(zero,y)) zero
  	else if ( egal(one,x)) y
  	else if ( egal(one,y)) x
  	else if ( egal(two,x)) add(y,y)
  	else if ( egal(two,y)) add(x,x)

  	else if ( egal(mone,x)) neg(y)
  	else if ( egal(mone,y)) neg(x)
  	else if ( egal(mtwo,x)) neg(add(y,y))
  	else if ( egal(mtwo,y)) neg(add(x,x))
  	
  	else if ( egal(three,x)) add(add(y,y),y)
  	else if ( egal(three,y)) add(add(x,x),x)
  	else if ( egal(mthree,x)) neg(add(add(y,y),y))
  	else if ( egal(mthree,y)) neg(add(add(x,x),x))
  	
  	else
    if ((x == v) || (y == v) ) v 
  	else {
       val m1= mult(x.left,y)
			 val m2= mult(x,y.left)
			 val m3= mult(x.left,y.left)
			
			 val n1= mult(x.right,y)
			 val n2= mult(x,y.right)
			 val n3= mult(x.right,y.right)
			
			 val o1= mult(x.left,y)
			 val o2= mult(x,y.right)
			 val o3= mult(x.left,y.right)
			
			 val p1= mult(x.right,y)
			 val p2= mult(x,y.left)
			 val p3= mult(x.right,y.left)

			 val s1= add(m1,add(m2,neg(m3)))
			 val s2= add(n1,add(n2,neg(n3)))
			 val s3= add(o1,add(o2,neg(o3)))
			 val s4= add(p1,add(p2,neg(p3)))
println("Construct result for operation:"+ x+"x"+y)
			new Surreal(max(s1,s2),min(s3,s4),x+"x"+y);	
  	}
  }	
  
  /**
   * print egal
   */
  def pegal(x: Surreal, y:Surreal) ={
      println(x+"?="+y+":"+egal(x,y))  
  }
  /**
   * form: string form surreal with the notation {|}
   */
  def form(x:Surreal):String = {
    var sleft="" 
		var sright="" 
		if (x == v) ""
		else {
		  if (x.left  != v) sleft =form(x.left)
		  else 
		    if (x.right != v) sright=form(x.right)
		}
    "{"+sleft+"|"+sright+"}"
  }
  /**
   * print the form classical {|} 
   */
  def pform(x:Surreal) = {
    println(x+"="+form(x)+" depht:"+x.depht)
  }

  /**
   * 
   */
  def main(args: Array[String]) {
     val zero= new Surreal(v,v,"0")
     val p1 = new Surreal(zero,v,"1")
     val p2 = new Surreal(p1,v,"2")
     val p3 = new Surreal(p2,v,"3")
     val p4 = new Surreal(p3,v,"4")
     val p5 = new Surreal(p4,v,"5")
     val p6 = new Surreal(p5,v,"6")
     val p7 = new Surreal(p6,v,"7")
     val p8 = new Surreal(p7,v,"8")
     val p9 = new Surreal(p8,v,"9")
     val p10 = new Surreal(p9,v,"10")
     val p11 = new Surreal(p10,v,"11")
     val p12 = new Surreal(p11,v,"12")
     val p13 = new Surreal(p12,v,"13")
     val p14 = new Surreal(p13,v,"14")
     val p15 = new Surreal(p14,v,"15")
     val p16 = new Surreal(p15,v,"16")
     val p17 = new Surreal(p16,v,"17")
     val p18 = new Surreal(p17,v,"18")
     val p19 = new Surreal(p18,v,"19")
     val p20 = new Surreal(p19,v,"20")
     
     
     val m1= new Surreal(v ,zero,"-1")
     val m2= new Surreal(v ,m1,"-2")
     val m3= new Surreal(v ,m2,"-3")
     val m4= new Surreal(v ,m3,"-4")

    
    
    
     val p1_2= new Surreal(zero,p1,"1/2")
     val p3_2= new Surreal(p1,p2,"3/2")

    
    pform(zero)
    pform(p1)
    pform(m1)
    pform(m4)
    pform(p6)
    pform(p1_2)
    pform(p3_2)
    pform(neg(p1_2))
    pform(neg(p3_2))

    
    // test exception:
    try {
       val illegal = new Surreal(p2,p1)
    } catch {
      case ex: SurrealException => {
            println("SurrealException catch...")
      }
    }
    
    
    println ("Test 1 : supEgal")
    println(supEgal(zero,zero))
    println(supEgal(m1,p1))
    println(supEgal(p1,m1))
    println(supEgal(p1,p1_2))
    //test max
    println("max(m1,p1)="+max(m1,p1))
    println("max(p1,m1)="+max(p1,m1))
    println("max(p2,p1)="+max(p2,p1))
    // test negation:
    val negp4=neg(p4)
    pegal(negp4,m4)
    pform(negp4)
    
    // test sum:
    val sump2p1=add(p2,p1)
    pegal(p3,sump2p1)
    
    val sump3p4=add(p3,p4)
    pegal(p7,sump3p4)
    
    val p1_2Pp3_2=add(p1_2,p3_2)
    pegal(p1_2Pp3_2,p2)
    
    pform(p1_2Pp3_2)
    // test mult
    val p2xp0=mult(p2,zero)
    pegal(p2xp0,zero)
        
    val p1xp2=mult(p1,p2)
    pegal(p1xp2,p2)

    pegal(p1xp2,p1_2Pp3_2)
    
    val p2xp1=mult(p2,p1)
    pegal(p2,p1xp2)

    pegal(p1xp2,p2xp1)
    
    val p2xp2=mult(p2,p2)
    pegal(p2xp2,p4)

    
    val p2xp3=mult(p2,p3)
    val p3xp2=mult(p3,p2)
    
    pegal(p2xp3,p6)
    pegal(p2xp3,p3xp2)
    pform(p2xp3)

    pform(p3xp2)
    
    
    println("Begin:2x4") 
    val p2xp4=mult(p2,p4)
    println("Begin:4x2")
    val p4xp2=mult(p4,p2)
    println("Begin:2x4 ?=p8")
    pegal(p2xp4,p8)
    println("Begin:2x4 ?=4x2")
    pegal(p2xp4,p4xp2)
    pform(p2xp4)
    pform(p4xp2)

    
    println("3x3:")
    val p3xp3=mult(p3,p3)
    println("p3xp3 Finish ")
    pegal(p3xp3,p9)
    pform(p3xp3)


    println("3x4:")
    val p3xp4=mult(p3,p4)
    println("p3xp4 Finish !")
    pegal(p3xp4,p12)
    pform(p3xp4)

    println("3x1/2:")
    val p3xp1_2=mult(p3,p1_2)
    println("p3xp1_2 Finish !")
    pegal(p3xp1_2,p3_2)
    pform(p3xp1_2)
  

    
    
    
    // Because the time  is to big:
    System.exit(0)    
    
    println("4x4:")
    val p4xp4=mult(p4,p4)
    println("Finish")
    pegal(p4xp4,p16)
    pform(p4xp4)
    
    println("Depht:"+p4xp4.depht)    
    

  }
    
  }




