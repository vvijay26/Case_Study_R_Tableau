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

#Below packages need to be installed (if not available, a one time activity)
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

#Create a dataframe for english speaking countries
#As a sample, 4 countries are present, can be increased if necessary
country_name <- c("USA","CHN","GBR","IND")
eng_countries<- c("y","n","y","y")
eng_speaking <- data.frame(country_name,eng_countries)

#Merge the top9 countries (With highest venture funding)
# with eng_speaking data frame
# Countries are english speaking or not based on eng_speaking$eng_countries
merged_countries <- merge(top9, eng_speaking, by.x = "Country_Code", 
                      by.y="country_name")

#Filter english speaking countries alone
eng_speaking_countries <- subset(merged_countries,merged_countries$eng_countries =="y")

#Sort the output (Descending order of Venture funding by country)
eng_speaking_countries_sorted <- arrange(eng_speaking_countries,desc(eng_speaking_countries$Raised_Amount_USD))

#output of eng_speaking_countries_sorted  
#Country_Code Raised_Amount_USD eng_countries
#1          USA      422510842796             y
#2          GBR       20245627416             y
#3          IND       14391858718             y

##########################################
#######Checkpoint 3 End###################
##########################################


##########################################
########Checkpoint 4 Start################
##########################################

#4.1.	Extract the primary sector of each category list from the category_list column

#We use the str_split_fixed function from stringr package to split
# the category_list into primary category (keep the first category)
# Since pipe - | is a special character, it needs to be escaped
# The primary category is added as the 16th column in master_frame

library(stringr)

master_frame$primary_category <- str_split_fixed(master_frame$category_list, 
                                                 "\\|", 3)[,1]

#4.2 2.	Use the mapping file 'mapping.csv' to map each primary sector to 
#one of the eight main sectors (Note that ‘Others’ is also considered 
# one of the main sectors)

#Step 1 - load the mapping file to a data.frame
#check.names=FALSE is required since there are special characters like
# comma (,), Ampersand (&), Slash (/) etc. in the csv file
mapping <- read.csv(file="mapping.csv", stringsAsFactors = FALSE,sep=",",check.names=FALSE)

#Add sector names in mapping file as a column

mapping$sector_names <- names(mapping)[-1][apply(mapping[2:10], 1, function(x) which(x=="1"))]

#convert case on category in both master_Frame and mapping dataframe
master_frame$primary_category <- tolower(master_frame$primary_category)
mapping$category_list <- tolower(mapping$category_list)
#Merge with master_frame on primary_Category to get an additional column on sector
# in master_frame
master_frame2 <- merge(master_frame,mapping,by.x = "primary_category", 
      by.y="category_list", all.x = TRUE)

##########################################
#######Checkpoint 4 End###################
##########################################


##########################################
########Checkpoint 5 Start################
##########################################

#Data extracted till now -
# Dataframe with companys main sector mapped - master_frame2
# Top 3 english speaking countries - eng_speaking_countries_sorted (usa/gbr/ind)
# Funding type - Venture (the below command will also extract it)
master_frame_funding_type[which(master_frame_funding_type$Raised_Amount_USD >5000000 & master_frame_funding_type$Raised_Amount_USD < 15000000),][1,1]

#As part of checkpoint 5, Now, the aim is to find out the most 
#heavily invested main sectors in each of the three countries 
#(for funding type FT and investments range of 5-15 M USD).

#	Create three separate data frames D1, D2 and D3 for each 
#of the three countries containing the observations of funding 
#type <FT> falling within the 5-15 million USD range. The 
#three data frames should contain:
#•	All the columns of the master_frame along with the primary sector and the main sector
#•	The total number (or count) of investments for each main sector in a separate column
#•	The total amount invested in each main sector in a separate column

#Store funding type in FT variable
FT <- master_frame_funding_type[which(master_frame_funding_type$Raised_Amount_USD >5000000 & master_frame_funding_type$Raised_Amount_USD < 15000000),][1,1]

#Subset master_frame2(with orimary sector info) into only FT type
# and store in master_frame3
master_frame3 <- subset(master_frame2,master_frame2$funding_round_type==FT)

#Create D1, D2 and D3 data frames dynamically and subset
#the master_frame3 into D1, D2 and D3 depending on top 3 
#english speaking countries (using dataframe eng_speaking_countries_sorted)
# All this is done thru loop to scale in future (suppose analysis..
# ..needs to be done for top 5 countries etc., it can be easily done
for( i in 1:nrow(eng_speaking_countries_sorted) ){
  nam <- paste("D", i, sep = "")
  df <- data.frame(subset(master_frame3,master_frame3$country_code==eng_speaking_countries_sorted[i, 1]),stringsAsFactors = FALSE)
  df[,17:25]<- NULL #Removing columns from mapping.csv as they are not reqd
  assign(nam,df)
}

#The following 2 steps remain -
#•	The total number (or count) of investments for each main sector 
#   in a separate column

#•	The total amount invested in each main sector in a separate column
