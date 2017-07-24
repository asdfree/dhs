if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
my_project <- Sys.getenv( "my_project" )
library(lodown)
lodown( "dhs" , output_dir = file.path( getwd() ) , 
	your_email = my_email_address , 
	your_password = my_password , 
	your_project = my_project )
