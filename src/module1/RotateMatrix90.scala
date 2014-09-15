package module1

import Array._



object RotateMatrix90 {

    def createMatrix(n: Int): Array[Array[Int]] =  {
      
      var matrix = Array[Array[Int]](); 
      
      for (j <- 1 to n) 
    	  matrix = matrix :+ List.tabulate(n)( i => (i + 1) + ((j-1)*n)).toArray;
      
      matrix;   	
      
    } 
    
     def rotate90[T](matrix: Array[Array[Int]]) : Array[Array[Int]] = {       
     
    	for ( row <- 0 to matrix.size/2 - 1 ){
    	  var first_col  = row  
    	  var last_col = matrix.size - 1 - row    	   	 
    	  
    	  for ( col <- first_col to last_col - 1) {     		
    	    
    	    var offset = last_col  - col
    	    
    		//top
    	    var temp = matrix(row)(col)
    	    
    	    //left 2 top
    	    matrix(row)(col) = matrix(row + offset)(first_col);   	   
    	    
    	    //bottom 2 left
    	    matrix(row + offset)(first_col) = matrix(last_col)(first_col + offset)    	
    	    
    	    //right 2 bottom
    	    matrix(last_col)(first_col + offset) = matrix(col)(last_col) 
    	 
    	    //top 2 right
    	    matrix(col)(last_col) = temp    	    
        	    
    	  }
    	  
    	}
    	
    	matrix;   	  
    	
    }
    
    
    
    def printMatrix[T](matrix : Array[Array[T]]) = {
      
    	println( matrix.map(_.mkString(" ")).mkString("\n")  );
    	println()
      
    }
    
    
    
    def main(args: Array[String]) {
      
    	printMatrix(createMatrix(5));
    	printMatrix(rotate90(createMatrix(5)));    	
    	
    
  
    }
  
}