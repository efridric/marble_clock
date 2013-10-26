import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

object MarbleClock {
	def main(args: Array[String]) {
		println("Thank god this is not c++")
		
		//Bottom tray(5th tray) is a queue this is where the marbles wait to be processed
		
		//Top tray has 4 marbles this is a stack and when its full the stack dumps back down to the bottom queue tray (Stack) (Represents 1min intervals)
		
		//Second tray holds 2 marbles emptied same way as the first (Stack) (Represents 5min intervals)
		
		//Third tray holds 4 marbles (Stack) (Represents 15min intervals)
		
		//Fourth tray holds 11 marbles (Stack) (Represents 1hr intervals)
						
		val stackTrays = List[StackTray](new StackTray("Top-Tray", 4), new StackTray("Second-Tray", 2), new StackTray("Third-Tray", 4), new StackTray("Fourth-Tray", 11))
		val queueTray = QueueTray.sizePrompt()
				
		//For debugging 
		for(i <- 0 until 25){
		  println("<--------------- Iteration "+i+" --------------------->")
		  incrementByOne
		  stackTrays.foreach(x => println(x.name + ": "+x.theTray.mkString(", ")))
		  println("Bottom-Tray: "+queueTray.mkString(", "))
		  println()
		}
		
		def incrementByOne = {
		  val temp = check
		  stackTrays(temp).theTray.push(queueTray.dequeue)
		  for(i <- 0 until temp){
		    for(j <- 0 until stackTrays(i).maxLength)
		      queueTray.enqueue(stackTrays(i).theTray.pop)	      
		  }
		}
		
		def check: Int = {
			for( i <- 0 until 4 ){
			  if(!stackTrays(i).isFull)
			    return i
			}
		  return 5;
		}
		
	}
}

case class StackTray(name: String, maxLength: Int) {
	val theTray = new Stack[Int];
	
	def isFull : Boolean = {
	  if (theTray.size == maxLength)
	    return true;
	  return false;
	}
}

object QueueTray {
	
	def init(size: Int) : Queue[Int] = {
	  val queueTray = new Queue[Int]
	  for(i <- 1 to size)
	  	queueTray.enqueue(i: Int)
	  return queueTray
	}
	
	def sizePrompt() = {
	  init(readLine("Enter the starting number of marbles: ").toInt)
	}
}

