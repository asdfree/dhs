if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)
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


# skip flat ascii, sas, and spss files
dhs_cat <- subset( dhs_cat , !grepl( "fl\\.zip|sd\\.zip|sv\\.zip" , full_url , ignore.case = TRUE ) )

# skip some large files
dhs_cat <- subset( dhs_cat , !grepl( "iabr71|iahr71|iair71|egir01dt|egkr01dt|iakr71|iapr71" , full_url , ignore.case = TRUE ) )

record_categories <- ceiling( seq( nrow( dhs_cat ) ) / ceiling( nrow( dhs_cat ) / 20 ) )

dhs_cat <- dhs_cat[ record_categories == this_sample_break , ]

lodown( "dhs" , dhs_cat , 
		your_email = my_email_address , 
		your_password = my_password ,
		your_project = my_project )
if( any( dhs_cat$year == 2004 & dhs_cat$country == 'Malawi' & grepl( "MWIR4EDT" , dhs_cat$full_url ) ) ){
library(lodown)
# examine all available DHS microdata files
dhs_cat <-
	get_catalog( "dhs" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address , 
		your_password = my_password , 
		your_project = my_project )

# malawi 2004 only
dhs_cat <- subset( dhs_cat , country == 'Malawi' & year == 2004 )
# download the microdata to your local computer





library(survey)

dhs_df <- 
	readRDS( 
		file.path( getwd() , 
		"Malawi/Standard DHS 2004/MWIR4EFL.rds" )
	)

# convert the weight column to a numeric type
dhs_df$weight <- as.numeric( dhs_df$v005 )

# paste the `sdist` and `v025` columns together
# into a single strata variable
dhs_df$strata <- do.call( paste , dhs_df[ , c( 'sdist' , 'v025' ) ] )
# as shown at
# http://userforum.dhsprogram.com/index.php?t=rview&goto=2154#msg_2154

dhs_design <- 
	svydesign( 
		~ v021 , 
		strata = ~strata , 
		data = dhs_df , 
		weights = ~weight
	)
dhs_design <- 
	update( 
		dhs_design , 
		
		one = 1 ,
		
		total_children_ever_born = v201 ,
		
		surviving_children = v201 - v206 - v207 ,
		
		urban_rural = factor( v025 , labels = c( 'urban' , 'rural' ) ) ,
		
		ethnicity =
			factor( v131 , levels = c( 1:8 , 96 ) , labels =
				c( "Chewa" , "Tumbuka" , "Lomwe" , "Tonga" , 
				"Yao" , "Sena" , "Nkonde" , "Ngoni" , "Other" ) ) ,
				
		no_formal_education = as.numeric( v149 == 0 )
		
	)
sum( weights( dhs_design , "sampling" ) != 0 )

svyby( ~ one , ~ urban_rural , dhs_design , unwtd.count )
svytotal( ~ one , dhs_design )

svyby( ~ one , ~ urban_rural , dhs_design , svytotal )
svymean( ~ surviving_children , dhs_design )

svyby( ~ surviving_children , ~ urban_rural , dhs_design , svymean )
svymean( ~ ethnicity , dhs_design , na.rm = TRUE )

svyby( ~ ethnicity , ~ urban_rural , dhs_design , svymean , na.rm = TRUE )
svytotal( ~ surviving_children , dhs_design )

svyby( ~ surviving_children , ~ urban_rural , dhs_design , svytotal )
svytotal( ~ ethnicity , dhs_design , na.rm = TRUE )

svyby( ~ ethnicity , ~ urban_rural , dhs_design , svytotal , na.rm = TRUE )
svyquantile( ~ surviving_children , dhs_design , 0.5 )

svyby( 
	~ surviving_children , 
	~ urban_rural , 
	dhs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ surviving_children , 
	denominator = ~ total_children_ever_born , 
	dhs_design 
)
sub_dhs_design <- subset( dhs_design , v447a %in% 40:49 )
svymean( ~ surviving_children , sub_dhs_design )
this_result <- svymean( ~ surviving_children , dhs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ surviving_children , 
		~ urban_rural , 
		dhs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( dhs_design )
svyvar( ~ surviving_children , dhs_design )
# SRS without replacement
svymean( ~ surviving_children , dhs_design , deff = TRUE )

# SRS with replacement
svymean( ~ surviving_children , dhs_design , deff = "replace" )
svyciprop( ~ no_formal_education , dhs_design ,
	method = "likelihood" )
svyttest( surviving_children ~ no_formal_education , dhs_design )
svychisq( 
	~ no_formal_education + ethnicity , 
	dhs_design 
)
glm_result <- 
	svyglm( 
		surviving_children ~ no_formal_education + ethnicity , 
		dhs_design 
	)

summary( glm_result )
library(srvyr)
dhs_srvyr_design <- as_survey( dhs_design )
dhs_srvyr_design %>%
	summarize( mean = survey_mean( surviving_children ) )

dhs_srvyr_design %>%
	group_by( urban_rural ) %>%
	summarize( mean = survey_mean( surviving_children ) )

}
