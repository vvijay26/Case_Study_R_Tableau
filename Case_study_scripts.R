#Case Study

##########################################
#######Checkpoint 1 Start#################
##########################################

#Clear the Environment - To avoid any testing issues.
rm(list=ls())

# Set working directory - physical location to read and write files from
setwd("C:/pgdds/Course 1/Project")

# Check if working directory is set
getwd()

# Read companies and rounds2 files into data frames
#quote = "" ignores all quotes - this is required in this dataset, since
# row number 114850 has quotes and we have to ignore quotes in both the
#  files for comparison - /ORGANIZATION/ZWAYO-"ON-DEMAND-VALET-PARKING"

companies <- read.table(file="companies.txt",sep="\t",
                        header=TRUE,fill=TRUE,comment.char = "",quote = "")
rounds2 <- read.csv(file="rounds2.csv", stringsAsFactors = FALSE,quote = "")

# Check structure of the 2 dataframes
str(companies)
str(rounds2)

#Case Study questions
#1.1- Data cleaning
#1 - How many unique companies are present in rounds2? 
# Answer - 66368

# tolower is used to make the permalink case 
# insensitive (as in the files provided, the casse does not match)

rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

length(unique(rounds2$company_permalink))

#2 - How many unique companies are present in companies?
# Answer - 66368
length(unique(companies$permalink))

#3 - In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
companies$permalink

#4 - Are there any companies in the rounds2 file which are not present in companies? 
# Answer yes or no: Y/N 
# (Answer is NO)

# We can achieve this through merging the 2 files (Left outer - Taking all rows 
# from Rounds2) and then checking for "NA" # values in the columns populated 
# from Companies dataframe.

#all.x ensures all rows of rounds2 are present even if there is no matching 
#permalink in companies - like a left outer join of sql

#by.x and by.y are used to match the columns (since the names are different, 
# this is required)

master_frame <- merge(rounds2, companies, by.x = "company_permalink", 
                      by.y="permalink", all.x = TRUE)

#The below command will identify all the "NA" - i.e., rows not present 
#in companies file that are present in Rounds2 - Answer to question 4 in 1.1
# Answer is NO
which(is.na(master_frame$name)=="TRUE")

#5 Merge the two data frames so that all variables (columns) in the companies 
#frame are added to the rounds2 data frame. Name the merged frame master_frame. 
#How many observations are present in master_frame?

#Data is merged as part of Step 4 above.

#Total observations in master_frame is same as rounds2 (as we did a left outer)
length(master_frame$company_permalink)

#Distinct companies
length(unique(master_frame$company_permalink))

##########################################
#########Checkpoint 1 End#################
##########################################

##########################################
#######Checkpoint 2 Start#################
##########################################

#2.1
#1 Average funding of each type
master_frame_rollup_raised_amt <- setNames(aggregate(master_frame$raised_amount_usd, 
                     by=list(master_frame$funding_round_type), 
                     FUN=mean, na.rm="TRUE"),
                    c("Funding_Round_Type","Raised_Amount_USD"))

master_frame_funding_type <- subset(master_frame_rollup_raised_amt,
                                    master_frame_rollup_raised_amt$Funding_Round_Type
                                    == "venture"|
                                      master_frame_rollup_raised_amt$Funding_Round_Type
                                    == "seed"|
                                    master_frame_rollup_raised_amt$Funding_Round_Type
                                    == "angel"|
                                      master_frame_rollup_raised_amt$Funding_Round_Type
                                    == "private_equity")


##########################################
#######Checkpoint 2 End###################
##########################################


##########################################
########Checkpoint 3 Start################
##########################################

# 3.1 Top nine countries which have received the highest total 
# funding (across ALL sectors for the chosen investment type)

#Populate Chosen_type variable with funding type and raised amount
#This code is re-usable for other investment types, just replace the "venture" 
# to some other type as needed for future use.
Chosen_type <- subset(master_frame_funding_type,master_frame_funding_type$Funding_Round_Type
       == "venture")

#Next step is to subset the master_frame on funding type (in this case "venture")
venture_records <- subset(master_frame,
                          master_frame$funding_round_type==as.character(Chosen_type[1]))

#Next step is to aggregate the venture funding amounts by country
venture_records_by_country <- setNames(aggregate(venture_records$raised_amount_usd, by=list(venture_records$country_code), FUN=sum,na.rm=TRUE),c("Country_Code","Raised_Amount_USD"))

#Next step is to remove records that have blank country_type
venture_records_by_country_non_blanks <- subset(venture_records_by_country,venture_records_by_country$Country_Code!="")

#We use arrange function from plyr package, as per CRAN community, this seems to
# be the fastest way to sort a data.frame

#Below packages need to be installed (if not available, one time activity)
# install.packages(pkgs="plyr")    
# library(plyr)

#Sort the venture funding amounts in descending order by country
venture_records_by_country_non_blanks_desc_amt <- arrange(venture_records_by_country_non_blanks,desc(Raised_Amount_USD))

#Populate top9 with the top 9 countries (first goal of the analysis)
top9 <- head(venture_records_by_country_non_blanks_desc_amt,n=9)

#Next step is to identify the top 3 english speaking countries
# (second goal of the analysis)

#From the link http://www.emmir.org/fileadmin/user_upload/admission/Countries_where_English_is_an_official_language.pdf
# We can see that usa, united kingdom and india are the top 3 countries
#USA	-	  y
#CHN	-	  n
#GBR	-		y
#IND	-		y
#CAN	-		y
#FRA	-		n
#ISR	-		n
#DEU	-		n
#JPN	-		n

#We have hardcoded the country codes as per TA guidance 
#(We can modify this code to take in a dataframe and compare, later)
top3 <- subset(top9,top9$Country_Code == "USA"|top9$Country_Code == "GBR"|top9$Country_Code == "IND")
