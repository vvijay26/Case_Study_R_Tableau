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
