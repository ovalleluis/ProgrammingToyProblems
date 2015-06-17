package module1

object Problems99 {

  def last[A](myList: List[A]): A = myList match {

    case ele :: Nil => ele
    case ele :: theTail => last(theTail)
    

  }

  def penultimate[A](myList: List[A]): A = myList match {

    case ele :: last :: Nil => ele
    case ele :: theTail => penultimate(theTail)
    case _ => throw new NoSuchElementException

  }

  def nth[A](myList: List[A], n: Int): A = (myList, n) match {

    case (ele :: _, 0) => ele
    case (ele :: tail, n) => nth(tail, n - 1)
    case (Nil, _) => throw new NoSuchElementException

  }

  def length[A](myList: List[A], size: Int): Int = myList match {

    case ele :: Nil => size + 1
    case ele :: tail => length(tail, size + 1)
    case _ => size

  }

  def length[A](myList: List[A]): Int = {
    length(myList, 0)
  }

  def reverse[A](myList: List[A]): List[A] = myList match {

    case Nil => myList
    case ele :: tail => reverse(tail) ::: List(ele)

  }

  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[Any] => flatten(ms)
    case e => List(e)
  }

  def compress(ls: List[Any]): List[Any] = ls match {

    case a :: b :: tail => {
      if (a == b) compress(a :: tail)
      else a :: compress(b :: tail)
    }
    case _ => ls
  }

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {

    pack(ls).map(x => (length(x), x.head))
  }

  def encodeModified[A](ls: List[A]): List[Any] = {

    encode(ls) map { t => if (t._1 == 1) t._2 else t }

  }
  
   
  
  
  def main(args: Array[String]): Unit = {
    
    println("http://aperiodic.net/phil/scala/s-99/")
    
    println("P1:")
    println("last List(1,2,3,4,5,6,7) -> " +  last(List(1,2,3,4,5,6,7)))
    
    println("P2:")
    println("penultimate (1,2,3,4,5,6,7) -> " +  penultimate (List(1,2,3,4,5,6,7)))
    
    println("P3:")
    println("nth (1,2,3,4,5,6,7) ,3 -> " +  nth( (List(1,2,3,4,5,6,7)),3) )
    
    println("P4:")
    println("length (1,2,3,4,5,6,7) -> " +  length(List(1,2,3,4,5,6,7)) ) 
    
    println("P5:")
    println("reverse (1,2,3,4,5,6,7) -> " +  reverse(List(1,2,3,4,5,6,7)) ) 
    
    println("P6:")
    println("isPalindrome (1,2,3,4,3,2,1) -> " +  isPalindrome(List(1,2,3,4,3,2,1)) ) 
    
    println("P7:")
    println("flatten(List(List(1, 1), 2, List(3, List(5, 8))))  " +  flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    
    println("P8")
    println("compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))  " + compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) )
    
    println("P9")
    println("pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))" + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    
    println("P10")
    println("List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" + encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ) 
    
    
    println("P11")
    println("List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)" + encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ) 
    
    
    
    
  }

}