if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
my_password <- Sys.getenv( "my_password" )
my_project <- Sys.getenv( "my_project" )
library(lodown)

dhs_cat <-
	get_catalog( "dhs" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address , 
		your_password = my_password ,
		your_project = my_project )

# some indian files are too large to test on 
dhs_cat <- subset( dhs_cat , !grepl( "IAIR52" , full_url ) )
		
# sample 10% of the records
which_records <- sample( seq( nrow( dhs_cat ) ) , round( nrow( dhs_cat ) * 0.10 ) )

# always sample the 2004 malawi sample
dhs_cat <- unique( rbind( dhs_cat[ which_records , ] , subset( dhs_cat , year == 2004 & country == 'Malawi' & grepl( "MWIR4DDT" , full_url ) ) ) )

lodown( "dhs" , dhs_cat , 
		your_email = my_email_address , 
		your_password = my_password ,
		your_project = my_project )

