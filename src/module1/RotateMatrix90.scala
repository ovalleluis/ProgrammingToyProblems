package module1

import Array._



object RotateMatrix90 {

    def matrix(n: Int): Array[Array[Int]] =  {
    	Array.ofDim[Int](n,n) ;   	
    } 
    
    
    
    def main(args: Array[String]) {
      
    	println( matrix(5).map(_.mkString(" ")).mkString("\n")
    	       )
    	var n = 5 
    	
    	var matrix = Array();
    	       
    	matrix :+ (
    			for (j <- 1 to n) yield List.tabulate(n)( i => (i + 1) + ((j-1)*n)))
    	
  
    }
  
}