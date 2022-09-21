package CS2.hello.CS2Project
import io.StdIn._

object newmovielibrary extends App{
//my movie library has been updated to use singly linked lists as its data collection method rather than lists
trait ListADT[A]{

  def apply(index:Int):A
  def update(index:Int, data:A):Unit
  def insert(index:Int, data:A):Unit
  def remove(index:Int):A

}

class SinglyLinkedList[A] extends ListADT[A]{
private class Node(var data: A, var next: Node)//A is a data type meant to restrict its usage to this class
private var head: Node = null//the list starts empty

    def apply(index: Int): A = {
        assert(index >= 0)
        var rover = head
        for (i <- 0 until index) rover = rover.next
        rover.data
    }
/* We look for a piece of data and return said piece of data
Rover starts at head and we loop thru to find the index we are looking for
Each time we go through the for loop, the rover points to the next node*/


    def update(index: Int, data: A): Unit = {
        assert(index >= 0)
        var rover = head
        for (i <- 0 until index) 
        rover = rover.next
        rover.data = data
    }

/* Updates a node (index tells rover which node we are updating)
using the new data that we've been given*/

    def insert(index: Int, data: A): Unit = {
        assert(index >= 0)
        if (index == 0) {
            head = new Node(data, head)
        } 
        else {
            var rover = head
            for (i <- 0 until index - 1) rover = rover.next
            rover.next = new Node(data, rover.next)
        }
    }
/* Location in linked list where we want to add a new element*/

    def remove(index: Int): A = {
        assert(index >= 0)
        if (index == 0) {
            val ret = head.data
            head = head.next
            ret
        } 
        else {
            var rover = head
            for (i <- 0 until index - 1) rover = rover.next
            val ret = rover.next.data
            rover.next = rover.next.next
            ret
        }
    }
/* saves head value, adjusts pointers to point from the node before the target node to the node after the target node*/

    def printlist():Unit={

    var rover = head

    while (rover.next != null){

        print(rover.data + " ")
        println()
        println("******")
        rover = rover.next

    }
    }
}//prints out the contents of the singly linked list
    class Movie(var movietitle:String, var releaseyr:Int, var studio:String, var genre:String) {

        override def toString():String ={
            s"$movietitle \n $releaseyr \n $studio \n $genre"
            
        }
    }
    /*creates the structure of the Movie class for our movie library and redefines the "toString" method
    in order to make sure when we print a certain part of the linked list we don't end up printing out
    the memory location by accident*/
    

    var IronMan = new Movie("Iron Man", 2008, "Marvel Studios", "Action/ Sci-Fi")
    var SpiderMan = new Movie("Spider-Man: Homecoming", 2017, "Marvel Studios", "Action/ Adventure")
    var PursuitofHappyness = new Movie("Pursuit of Happiness", 2006, "Columbia Pictures", "Drama")
    var DespicableMe3 = new Movie("Despicable Me 3", 2017, "Illumination", "Family/ Comedy")
    var Blackfish = new Movie("Blackfish", 2013, "Manny O Productions", "Documentary")
    var JujutsuKaisen0 = new Movie("Jujutsu Kaisen 0", 2022, "Mappa", "Action/ Fantasy")
    var StandandDeliver = new Movie("Stand and Deliver", 1988, "Warner Bros", "Drama")
    var EightMile = new Movie("8 Mile", 2002, "Imagine Entertainment", "Drama/ Musical")
    var OrderofPhoenix = new Movie("Harry Potter and the Order of the Phoenix", 2007, "Warner Bros Pictures", "Fantasy/ Adventure")
    var Encanto = new Movie("Encanto", 2021, "Walt Disney Pictures", "Family/ Musical")


    /*creates instances of the movie class- each instance containing the information for 
    a movie that we will be able to access through any of our actions*/

        var moviels = new SinglyLinkedList[Movie]
        moviels.insert(0,IronMan)
        moviels.insert(1,SpiderMan)
        moviels.insert(2,PursuitofHappyness)
        moviels.insert(3,DespicableMe3)
        moviels.insert(4,Blackfish)
        moviels.insert(5,JujutsuKaisen0)
        moviels.insert(6,StandandDeliver)
        moviels.insert(7,EightMile)
        moviels.insert(8,OrderofPhoenix)
        moviels.insert(9,Encanto)
    
    //creates the singly linked list that will contain all of our movies

    
def displayamovie():Unit={
    println("0 "+IronMan.movietitle + " " +IronMan.releaseyr)
    println("1 "+SpiderMan.movietitle+ " " +SpiderMan.releaseyr)
    println("2 "+PursuitofHappyness.movietitle+ " " +PursuitofHappyness.releaseyr)
    println("3 "+DespicableMe3.movietitle+ " " +DespicableMe3.releaseyr)
    println("4 "+Blackfish.movietitle+ " " +Blackfish.releaseyr)
    println("5 "+JujutsuKaisen0.movietitle+ " " +JujutsuKaisen0.releaseyr)
    println("6 "+StandandDeliver.movietitle+ " " +StandandDeliver.releaseyr)
    println("7 "+EightMile.movietitle+ " " +EightMile.releaseyr)
    println("8 "+OrderofPhoenix.movietitle+ " " +OrderofPhoenix.releaseyr)
    println("9 "+Encanto.movietitle+ " " +Encanto.releaseyr)
    println("Enter the digit for the desired movie title (0-9): ")
    var moviepick = readInt
    println(moviels.apply(moviepick))
    }

//display a specific movie that is the user is asking for and exists within our movie library
//uses a digit that correlates with the index of said movie in order to access the movie's contents


def displaymovie(moviels:SinglyLinkedList[Movie]):Unit={//updated
        moviels.printlist()
}
//prints out each movie one by one and adds a space after each movie is printed. The function finishes when all movies are printed.



def filter(){//updated
    println("Enter how you would like to filter movies by (Title, Year Released, Studio, Genre): ")
    var ifilter = readLine
    ifilter match{
        case "Title" =>{
            println("**********************************************")
            println("**********************************************")
            println(" 0 "+IronMan.movietitle)
            println(" 1 "+SpiderMan.movietitle)
            println(" 2 "+PursuitofHappyness.movietitle)
            println(" 3 "+DespicableMe3.movietitle)
            println(" 4 "+Blackfish.movietitle)
            println(" 5 "+JujutsuKaisen0.movietitle)
            println(" 6 "+StandandDeliver.movietitle)
            println(" 7 "+EightMile.movietitle)
            println(" 8 "+OrderofPhoenix.movietitle)
            println(" 9 "+Encanto.movietitle)
            println("**********************************************")
            println("Enter the digit for the desired movie title(0-9): ")
            println("********************************************** \n")
            var titlefilter = readInt
            println(moviels.apply(titlefilter))
            }
                

        case "Year Released" =>{ 
            println("**********************************************")
            println("**********************************************")
            println(" 0 "+IronMan.releaseyr)
            println(" 1 "+SpiderMan.releaseyr)
            println(" 2 "+PursuitofHappyness.releaseyr)
            println(" 3 "+DespicableMe3.releaseyr)
            println(" 4 "+Blackfish.releaseyr)
            println(" 5 "+JujutsuKaisen0.releaseyr)
            println(" 6 "+StandandDeliver.releaseyr)
            println(" 7 "+EightMile.releaseyr)
            println(" 8 "+OrderofPhoenix.releaseyr)
            println(" 9 "+Encanto.releaseyr)
            println("**********************************************")
            println("Enter the digit for the desired movie release year (0-9): ")
            println("**********************************************")
            var yrfilter = readInt
            println(moviels.apply(yrfilter))
        }

        case "Studio" =>{
            println("**********************************************")
            println("**********************************************")
            println(" 0 "+IronMan.studio)
            println(" 1 "+SpiderMan.studio)
            println(" 2 "+PursuitofHappyness.studio)
            println(" 3 "+DespicableMe3.studio)
            println(" 4 "+Blackfish.studio)
            println(" 5 "+JujutsuKaisen0.studio)
            println(" 6 "+StandandDeliver.studio)
            println(" 7 "+EightMile.studio)
            println(" 8 "+OrderofPhoenix.studio)
            println(" 9 "+Encanto.studio)
            println("**********************************************")
            println("Enter the digit for the desired movie studio (0-9): ")
            println("**********************************************")
            var stfilter = readInt
            println(moviels.apply(stfilter))
        }

        case "Genre" =>{
            println("**********************************************")
            println("**********************************************")
            println(" 0 "+IronMan.genre)
            println(" 1 "+SpiderMan.genre)
            println(" 2 "+PursuitofHappyness.genre)
            println(" 3 "+DespicableMe3.genre)
            println(" 4 "+Blackfish.genre)
            println(" 5 "+JujutsuKaisen0.genre)
            println(" 6 "+StandandDeliver.genre)
            println(" 7 "+EightMile.genre)
            println(" 8 "+OrderofPhoenix.genre)
            println(" 9 "+Encanto.genre)
            println("**********************************************")
            println("Enter the digit for the desired movie genre (0-9): ")
            println("**********************************************")
            var gfilter = readInt
            println(moviels.apply(gfilter))
        }
        
    }
}
/*filters through the movie library to search for movies based on parts of their content 
such as title, year released, studio and genre*/


def moviesuggestions():Unit = {//updated
    println("Enter the title of the movie you would like to see us acquire: ")
    var newmovietitle = readLine
    println("Enter the year the suggested movie was released: ")
    var newmovieyr = readInt
    println("Enter the studio/ production company of the suggested movie: ")
    var newmoviestudio = readLine
    println("Enter the suggested movie's genre(s): ")
    var newmoviegenre = readLine

    var suggestedmovie = new Movie(" *Suggested movie* \n" + newmovietitle, newmovieyr, newmoviestudio, newmoviegenre)

    println(suggestedmovie)
    moviels.insert(10,suggestedmovie)
}

def rentmovie():Unit={//updated
    println("**********************************************")
    println("**********************************************")
    println(" 0 "+IronMan.movietitle + " "+IronMan.releaseyr)
    println(" 1 "+SpiderMan.movietitle+ " " +SpiderMan.releaseyr)
    println(" 2 "+PursuitofHappyness.movietitle+ " " +PursuitofHappyness.releaseyr)
    println(" 3 "+DespicableMe3.movietitle +DespicableMe3.releaseyr)
    println(" 4 "+Blackfish.movietitle+ " " +Blackfish.releaseyr)
    println(" 5 "+JujutsuKaisen0.movietitle+ " " +JujutsuKaisen0.releaseyr)
    println(" 6 "+StandandDeliver.movietitle+ " " +StandandDeliver.releaseyr)
    println(" 7 "+EightMile.movietitle+ " " +EightMile.releaseyr)
    println(" 8 "+OrderofPhoenix.movietitle+ " " +OrderofPhoenix.releaseyr)
    println(" 9 "+Encanto.movietitle+ " " +Encanto.releaseyr)
    println("**********************************************")
    println("Enter the digit of the movie you'd like to rent(0-9): ")
    println("**********************************************")
    var rtmoviedigit = readInt
    var rtdmovie = moviels.apply(rtmoviedigit)
    var rentedmovies = new SinglyLinkedList[Movie]
    var moviecopies = 1
    println("You have successfully rented: \n" + moviecopies +" copy of ")
    rentedmovies.insert(0,rtdmovie)
    println(rentedmovies.apply(0))
    println(" for $3.22 (taxes included).")
}



def createmovielist():Unit={//updated
    println("Do you want to add your favorite movie to your list? (Enter: Yes) or (Enter: No)")
    var favmovie = readLine
    var favrunner = true
    while (favrunner == true){
        favmovie match{
            case "Yes" =>{
                var favmovieindex = 0
                var favmovielist = new SinglyLinkedList[Movie]//creates singly linked list where favorite movies will be stored
                println("**********************************************")
                println("**********************************************")
                println(" 0 "+IronMan.movietitle+ " " +IronMan.releaseyr)
                println(" 1 "+SpiderMan.movietitle+ " " +SpiderMan.releaseyr)
                println(" 2 "+PursuitofHappyness.movietitle+ " " +PursuitofHappyness.releaseyr)
                println(" 3 "+DespicableMe3.movietitle+ " " +DespicableMe3.releaseyr)
                println(" 4 "+Blackfish.movietitle+ " " +Blackfish.releaseyr)
                println(" 5 "+JujutsuKaisen0.movietitle+ " " +JujutsuKaisen0.releaseyr)
                println(" 6 "+StandandDeliver.movietitle+ " " +StandandDeliver.releaseyr)
                println(" 7 "+EightMile.movietitle+ " " +EightMile.releaseyr)
                println(" 8 "+OrderofPhoenix.movietitle+ " " +OrderofPhoenix.releaseyr)
                println(" 9 "+Encanto.movietitle+ " " +Encanto.releaseyr)
                println("**********************************************")
                println("Enter the digit of your fav movie (0-9): ")
                println("**********************************************")
                var favmoviedigit = readInt
                var favmoviecontent = moviels.apply(favmoviedigit)/*copies the movie from the original library 
                of movies in order to add it to the favorite movies list*/
                favmovielist.insert(favmovieindex,favmoviecontent)//inserts copy of favorite movie into the favorite movie list
                println("Your favorite movie list consists of: \n ****************")
                println(favmovielist.apply(favmovieindex))
                println("***************")
                favmovieindex += 1
                favrunner = false
            }
            case "No" =>{
                println("Okay bye!")
                favrunner = false
            }
        }
            
    }
}


var runner = true
    while(runner == true){
        println(" \n \n ")
        println("*************************************************** \n")
        println("Welcome to Jose's new Movie Library! Enter an action: \n")
        println("*************************************************** \n")
        println(" ")
        println("\n -Filter through movies by movie studio, title, \n year released, or genre (Enter: Filter) \n")
        println(" ")
        println("\n -Add a movie suggestion (Enter: Movie Sugg) \n")
        println(" ")
        println("\n -Display rentable movies (Enter: Rent Movie) \n")
        println(" ")
        println("\n -Display a specific movie (Enter: Display A Movie) \n")
        println(" ")
        println("\n -Display all available movies (Enter: Display Movies) \n")
        println(" ")
        println("\n -Make a movie list (Enter: Movie List) \n")
        println(" ")
        println("\n -Close the movie library (Enter: Quit) \n")
        println("**********************************************")
        val line = readLine
        line match{
            case "Filter" =>{
                filter()
            }
            case "Movie Sugg" =>{
                moviesuggestions()
            }
            case "Rent Movie" =>{
                rentmovie
            }
            case "Display Movies" =>{
                displaymovie(moviels)
            }
            case "Display A Movie" =>{
                displayamovie()
            }
            case "Movie List" =>{
                createmovielist()
            }
            case "Quit" => {
                println("Bye!")
                runner = false
            }
            case m =>{
                println("We didn't understand your message- try again.")
                println(m)
            }
        }
    }
}
