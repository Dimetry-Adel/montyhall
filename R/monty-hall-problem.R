#' @title
#'   Create a new Monty Hall Problem game
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param
#'	... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'	Contestant selects a random door.
#'
#' @description
#'	`select_door()` this function to let the contestant makes his/her
#'	first selection.	
#'
#' @details
#'	After creating the game, this second function gives the contestant
#'	a chance to randomly select one of the three doors.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	The function returns a number (numeric vector) between 1 and 3
#'	indicating the position of the selected door by the contestant.
#'
#' @examples
#'	select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Host opens goat door.
#'
#' @description
#'	`open_goat_door()` this funcction for the host to open
#'	a door with a goat behind it.
#'
#' @details
#'	After randomly selecting one of the three doors, this function gives
#'	the host the ability to open a door with a goat behind it.
#'	Under the condition that it can’t be the door that contestant
#'	has already selected from the previous function.
#'	So this door must not be a car AND the current selection of the
#'	contestant. However, if the contestant selects the car on his/her
#'	first selection the host now can open either door and
#'	if the contestant selects a door of goat,
#'	the host only has one door to open.
#'
#' @param 
#'	game and a.pick ... these are default arguments from the previous function
#'	`select_door` to run the function correctly.
#'
#' @return 
#'	The function returns a number (numeric vector) between 1 and 3
#'	indicating the position of the opened door by the host.
#'
#' @examples
#'	open_goat_door( game = T, a.pick = T )

#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'	Contestant changes doors.
#'
#' @description
#'	`change_door()` this function is to give the chance for
#'	the contestant to change his/her first selection.
#'
#' @details
#'	After opening one of the goats' doors, now this function gives
#'	the host contestant the option to change to the other door that
#'	is still closed or stay with his/her first selection.
#'
#' @param 
#'	opened.door, a.pick ... these are default arguments from the previous
#'	function `open_goat_door` to run the function correctly.
#'	
#'	stay=TRUE or FALSE ... this argument represents the game-playing
#'					strategy to stay or change the door.
#'
#' @return 
#'	The function returns a number (numeric vector) between 1 and 3 or
#'	logical vector [TRUE (the same initial position)] indicating the position
#'	of the finally picked door by the contestant.
#'
#' @examples
#'	change_door(stay=T, opened.door, a.pick = T)
#'	change_door(stay=F, opened.door, a.pick = T)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'	Determine if contestant has won.
#'
#' @description
#'	`determine_winner()` this function is to determine
#'	the winning status of the contestant.
#'
#' @details
#'	After the final discussion of the contestant,
#'	either to change his/her door selection or to stay
#'	with the first selection, the function will determine
#'	his/her winning.
#'
#' @param 
#'	final.pick and game ... these are default arguments from the previous
#'	function `change_door` to run the function correctly.
#'
#' @return 
#'	The function will return either "WIN" or "LOSE"
#'	(character vector) that indicating the winning status.
#'
#' @examples
#'	this.game <- c("goat","car","goat")
#'	determine_winner( final.pick=1, game=this.game )
#'	determine_winner( final.pick=2, game=this.game )
#'	determine_winner( final.pick=3, game=this.game )
#'
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'	Test or play one round of the game.
#'
#' @description
#'	`play_game()` this function is to test the two strategies of the game.
#'
#' @details
#'	This function combines all the other functions of Monty Hall Problem
#'	game. For one round only, It tests the two strategies of the
#'	game (changing or staying with the first selection).
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'	The function will return a dataframe containing the winning
#'	and losing outcome that related to the two strategies
#'	(switching and staying). 
#'
#' @examples
#'	play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'	Test or play multiple rounds of the game.
#'
#' @description
#'	`play_n_games()` this function is to test the two
#'	strategies of the game for multiple times.
#'
#' @details
#'	This function combines all the other functions of Monty Hall Problem
#'	game. For mulitple 'n' rounds, It tests the two strategies of the
#'	game (changing or staying with the first selection).
#'
#' @param
#'	n ... the numbers of the times for playing or testing the game.
#'
#' @return
#'	The function will return a dataframe containing the winning
#'	and losing outcome that related to the two strategies
#'	(switching and staying). 
#'
#' @examples
#'	play_game( n = 10 )
#'
#'
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}