if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
my_project <- Sys.getenv( "my_project" )
this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

dhs_cat <-
	get_catalog( "dhs" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address , 
		your_password = my_password ,
		your_project = my_project )

# some indian files are too large to test on 
dhs_cat <- subset( dhs_cat , !grepl( "IAIR52" , full_url ) )
	
record_categories <- ceiling( seq( nrow( dhs_cat ) ) / ceiling( nrow( dhs_cat ) / 12 ) )

dhs_cat <- unique( rbind( dhs_cat[ record_categories == this_sample_break , ] , dhs_cat[ dhs_cat$year == 2004 & dhs_cat$country == 'Malawi' & grepl( "MWIR4EDT" , dhs_cat$full_url ) , ] ) )

lodown( "dhs" , dhs_cat , 
		your_email = my_email_address , 
		your_password = my_password ,
		your_project = my_project )

