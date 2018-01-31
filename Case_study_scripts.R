#Case Study

##########################################
#######Checkpoint 1 Start#################
##########################################

#Clear the Environment - To avoid any testing issues.
rm(list = ls())

# Set working directory - physical location to read and write files from
setwd("C:/pgdds/Course 1/Project")

# Check if working directory is set
getwd()

# Read companies and rounds2 files into data frames
#quote = "" ignores all quotes - this is required in this dataset, since
# row number 114850 has quotes and we have to ignore quotes in both the
#  files for comparison - /ORGANIZATION/ZWAYO-"ON-DEMAND-VALET-PARKING"

companies <- read.table(
  file = "companies.txt",
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  comment.char = "",
  stringsAsFactors = FALSE,
  quote = ""
)

rounds2 <-
  read.csv(file = "rounds2.csv",
           stringsAsFactors = FALSE,
           quote = "")

# Check structure of the 2 dataframes
str(companies)

str(rounds2)

#Case Study questions
#1.1- Data cleaning
#How many unique companies are present in rounds2?
# Answer - 66368

# tolower is used to make the permalink case
# insensitive (as in the files provided, the casse does not match)

rounds2$company_permalink <- tolower(rounds2$company_permalink)

companies$permalink <- tolower(companies$permalink)

length(unique(rounds2$company_permalink))

#1.2 How many unique companies are present in companies?
# Answer - 66368
length(unique(companies$permalink))

#1.3 - In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
companies$permalink

#1.4 - Are there any companies in the rounds2 file which are not present in companies?
# Answer yes or no: Y/N
# (Answer is NO)

# We can achieve this through merging the 2 files (Left outer - Taking all rows
# from Rounds2) and then checking for "NA" # values in the columns populated
# from Companies dataframe.

#all.x ensures all rows of rounds2 are present even if there is no matching
#permalink in companies - like a left outer join of sql

#by.x and by.y are used to match the columns (since the names are different,
# this is required)

master_frame <-
  merge(rounds2,
        companies,
        by.x = "company_permalink",
        by.y = "permalink",
        all.x = TRUE)

#The below command will identify all the "NA" - i.e., rows not present
#in companies file that are present in Rounds2 - Answer to question 4 in 1.1
# Answer is NO
which(is.na(master_frame$name) == "TRUE")

#1.5 Merge the two data frames so that all variables (columns) in the companies
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
master_frame_rollup_raised_amt <-
  setNames(
    aggregate(
      master_frame$raised_amount_usd,
      by = list(master_frame$funding_round_type),
      FUN = mean,
      na.rm = "TRUE"
    ),
    c("Funding_Round_Type", "Raised_Amount_USD")
  )

# Filter on the 4 investement types (as asked in checkpoint 2)
# Calculate the average investment amount for each of the four funding types 
# (venture, angel, seed, and private equity) and report the answers in Table 2.1
master_frame_funding_type <- subset(
  master_frame_rollup_raised_amt,
  master_frame_rollup_raised_amt$Funding_Round_Type
  == "venture" |
    master_frame_rollup_raised_amt$Funding_Round_Type
  == "seed" |
    master_frame_rollup_raised_amt$Funding_Round_Type
  == "angel" |
    master_frame_rollup_raised_amt$Funding_Round_Type
  == "private_equity"
)

# 2.2
#Based on the average investment amount calculated above, 
# which investment type do you think is the most suitable for Spark Funds?
filter(master_frame_funding_type, Raised_Amount_USD >= 5000000 & Raised_Amount_USD <= 15000000)

# ANSWER - "VENTURE" , as its the only one between 5 and 15 M USD.

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
Chosen_type <-
  subset(
    master_frame_funding_type,
    master_frame_funding_type$Funding_Round_Type
    == "venture"
  )

#Next step is to subset the master_frame on funding type (in this case "venture")
venture_records <- subset(master_frame,
                          master_frame$funding_round_type == as.character(Chosen_type[1]))

#Next step is to aggregate the venture funding amounts by country
venture_records_by_country <-
  setNames(
    aggregate(
      venture_records$raised_amount_usd,
      by = list(venture_records$country_code),
      FUN = sum,
      na.rm = TRUE
    ),
    c("Country_Code", "Raised_Amount_USD")
  )

#Next step is to remove records that have blank country_type
venture_records_by_country_non_blanks <-
  subset(venture_records_by_country,
         venture_records_by_country$Country_Code != "")

#We use arrange function from plyr package, as per CRAN community, this seems to
# be the fastest way to sort a data.frame

#Below packages need to be installed (if not available, a one time activity)
# install.packages(pkgs="plyr")
library(plyr)
library(dplyr)

#Sort the venture funding amounts in descending order by country
venture_records_by_country_non_blanks_desc_amt <-
  arrange(venture_records_by_country_non_blanks,
          desc(Raised_Amount_USD))

#Populate top9 with the top 9 countries (first goal of the analysis)
top9 <- head(venture_records_by_country_non_blanks_desc_amt, n = 9)

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
country_name <- c("USA", "CHN", "GBR", "IND")

eng_countries <- c("y", "n", "y", "y")

eng_speaking <- data.frame(country_name, eng_countries)

#Merge the top9 countries (With highest venture funding)
# with eng_speaking data frame
# Countries are english speaking or not based on eng_speaking$eng_countries
merged_countries <-
  merge(top9, eng_speaking, by.x = "Country_Code",
        by.y = "country_name")

#Filter english speaking countries alone
eng_speaking_countries <-
  subset(merged_countries, merged_countries$eng_countries == "y")

#Sort the output (Descending order of Venture funding by country)
eng_speaking_countries_sorted <-
  arrange(eng_speaking_countries,
          desc(eng_speaking_countries$Raised_Amount_USD))

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

master_frame$primary_category <-
  str_split_fixed(master_frame$category_list,
                  "\\|", 3)[, 1]

#2 2.	Use the mapping file 'mapping.csv' to map each primary sector to
#one of the eight main sectors (Note that ‘Others’ is also considered
# one of the main sectors)

#Step 1 - load the mapping file to a data.frame
#check.names=FALSE is required since there are special characters like
# comma (,), Ampersand (&), Slash (/) etc. in the csv file
mapping <-
  read.csv(
    file = "mapping.csv",
    stringsAsFactors = FALSE,
    sep = ",",
    check.names = FALSE
  )

# The mapping.csv file provided has an Issue, the letters "na"
# have been replaced by "0" for some reason in the first column (category_list). 
# These need to be corrected. Except for "Enterprise 2.0"
# https://learn.upgrad.com/v/course/113/question/57073

library(stringr)

# to convert Strings like "A0lytics" to "Analytics"
mapping$category_list <- str_replace_all(mapping$category_list, "0", "na")

#to ignore Enterprise 2.0
mapping$category_list <- str_replace_all(mapping$category_list, "\\.na", ".0")

#Convert Categories with first 2 characters as "na" to "Na" - sentence case

mapping$category_list <- str_replace_all(mapping$category_list, "^na", "Na")


#Add sector names in mapping file as a column
# Another way to achieve this is by using "gather" function (needs tidyr package)
# mapping_new <- gather(data = mapping, key = sector, value = value, "Automotive & Sports":"Social, Finance, Analytics, Advertising")
# mapping_new <- subset(mapping_new,mapping_new$value == "1")  'or' mapping_new <- mapping_new[!(mapping_new$value == 0),]
# mapping_new[,3] <- NULL 'or' mapping_new <- mapping_new[, -3]

mapping$sector_names <-
  names(mapping)[-1][apply(mapping[2:10], 1, function(x)
    which(x == "1"))]

mapping[, 2:10] <-
  NULL #Removing wide columns from mapping.csv as they are not reqd anymore

#convert case on category in both master_Frame and mapping dataframe
master_frame$primary_category <-
  tolower(master_frame$primary_category)

mapping$category_list <- tolower(mapping$category_list)
#Merge with master_frame on primary_Category to get an additional column on sector
# in master_frame
master_frame2 <-
  merge(
    master_frame,
    mapping,
    by.x = "primary_category",
    by.y = "category_list",
    all.x = TRUE
  )

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
master_frame_funding_type[which(
  master_frame_funding_type$Raised_Amount_USD > 5000000 &
    master_frame_funding_type$Raised_Amount_USD < 15000000
),][1, 1]

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
FT <-
  master_frame_funding_type[which(
    master_frame_funding_type$Raised_Amount_USD >= 5000000 &
      master_frame_funding_type$Raised_Amount_USD <= 15000000
  ),][1, 1]

# Filter master_frame2(with orimary sector info) into only FT type
# and store in master_frame3. Also filter only those records that
# have each round within 5 to 15 million USD (As Spark funds is only
# concerned with funding rounds within that range).
master_frame3 <-
  filter(master_frame2, funding_round_type == FT &
           raised_amount_usd >= 5000000 &
           raised_amount_usd <= 15000000
         )

#Create D1, D2 and D3 data frames dynamically and subset
#the master_frame3 into D1, D2 and D3 depending on top 3
#english speaking countries (using dataframe eng_speaking_countries_sorted)
# All this is done thru loop to scale in future (suppose analysis..
# ..needs to be done for top 5 countries etc., it can be easily done
#Similar code can be used at other places (written as for loop to make it clear)
for (i in 1:nrow(eng_speaking_countries_sorted)) {
  nam <- paste("D", i, sep = "")
  df <-
    data.frame(
      subset(
        master_frame3,
        master_frame3$country_code == eng_speaking_countries_sorted[i, 1]
      ),
      stringsAsFactors = FALSE
    )
  assign(nam, df)
}

#The following 2 steps remain -
#•	The total number (or count) of investments for each main sector
#   in a separate column
#•	The total amount invested in each main sector in a separate column

#!!!There could be categories in master_frame 
# which have no mapping in mapping.csv
# Replacing them with Blanks (As of now in the current D1, D2, D3, 
# only D1 has 1 record for which sector_name is blanks)
# Category - biotechnology and semiconductor, Name - HealthTell

D1[which(is.na(D1$sector_names)),1] <- "Blanks"
D2[which(is.na(D2$sector_names)),1] <- "Blanks"
D3[which(is.na(D3$sector_names)),1] <- "Blanks"

# let's create new Dataframes for each of the 3 dataframes to store
# aggregate by sectors. Later we can merge it to D1, D2, D3

D1_group_by_sector <-  setNames(
  aggregate(
    D1$raised_amount_usd,
    by = list(D1$sector_names),
    FUN = sum,
    na.rm = "TRUE"
  ),
  c("sector_names", "Aggregate_USD")
)

D1_count_by_sector <- setNames(data.frame(table(D1$sector_names)),
                               c("sector_names", "count_of_inv"))

#If this needs to reflect in the D1 dataframe, we can merge on Main_Sector
D1 <-
  merge(D1, D1_group_by_sector, by = "sector_names", all.x = TRUE)
D1 <-
  merge(D1, D1_count_by_sector, by = "sector_names", all.x = TRUE)

#Similarly for D2 and D3
D2_group_by_sector <-  setNames(
  aggregate(
    D2$raised_amount_usd,
    by = list(D2$sector_names),
    FUN = sum,
    na.rm = "TRUE"
  ),
  c("sector_names", "Aggregate_USD")
)

D2_count_by_sector <- setNames(data.frame(table(D2$sector_names)),
                               c("sector_names", "count_of_inv"))

D2 <-
  merge(D2, D2_group_by_sector, by = "sector_names", all.x = TRUE)

D2 <-
  merge(D2, D2_count_by_sector, by = "sector_names", all.x = TRUE)

D3_group_by_sector <-  setNames(
  aggregate(
    D3$raised_amount_usd,
    by = list(D3$sector_names),
    FUN = sum,
    na.rm = "TRUE"
  ),
  c("sector_names", "Aggregate_USD")
)

D3_count_by_sector <- setNames(data.frame(table(D3$sector_names)),
                               c("sector_names", "count_of_inv"))

D3 <-
  merge(D3, D3_group_by_sector, by = "sector_names", all.x = TRUE)

D3 <-
  merge(D3, D3_count_by_sector, by = "sector_names", all.x = TRUE)

#D1,D2,D3 are the 3 dataframes for each of the top 3
# english speaking countries for FT ("Venture") type.
# They contain all the columns of master_frame
# + the primary category
# + the main sector
# + the total count of investments for each main sector in a separate column
# + the total amount invested in each main sector in a separate column

# Checkpoint 5 Answers
-----------------------
#5.1 Total number of Investments (count)
  
  sum(D1_count_by_sector$count_of_inv)
  sum(D2_count_by_sector$count_of_inv)
  sum(D3_count_by_sector$count_of_inv)
  
#5.2 Total amount of investment (USD)
  
  sum(D1_group_by_sector$Aggregate_USD)
  sum(D2_group_by_sector$Aggregate_USD)
  sum(D3_group_by_sector$Aggregate_USD)
  
#5.3 Top Sector name (no. of investment-wise)
#5.6 Number of investments in top sector (3)
  top_D1_sector_nbr <- arrange(D1_count_by_sector,desc(count_of_inv))[1,]
  top_D2_sector_nbr <- arrange(D2_count_by_sector,desc(count_of_inv))[1,]
  top_D3_sector_nbr <- arrange(D3_count_by_sector,desc(count_of_inv))[1,]
  
#5.4 Second Sector name (no. of investment-wise)
#5.7 Number of investments in second sector (4)
  second_D1_sector_nbr <- arrange(D1_count_by_sector,desc(count_of_inv))[2,]
  second_D2_sector_nbr <- arrange(D2_count_by_sector,desc(count_of_inv))[2,]
  second_D3_sector_nbr <- arrange(D3_count_by_sector,desc(count_of_inv))[2,]
  
  
#5.5 Third Sector name (no. of investment-wise)
#5.8 Number of investments in third sector (5)
  third_D1_sector_nbr <- arrange(D1_count_by_sector,desc(count_of_inv))[3,]
  third_D2_sector_nbr <- arrange(D2_count_by_sector,desc(count_of_inv))[3,]
  third_D3_sector_nbr <- arrange(D3_count_by_sector,desc(count_of_inv))[3,]
  
  
#5.9 For point 3 (top sector count-wise), which company received the highest investment?
  
  #Subset D1 to keep only top sector rows.
  D1_subset_top_sector = filter(D1,sector_names == top_D1_sector_nbr[1,1]) 
  #Aggregate by company permalink for top sector
  D1_funding_by_company <- setNames(aggregate(D1_subset_top_sector$raised_amount_usd,by= list(D1_subset_top_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D1_top_funded_amount <- arrange(D1_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D1_top_companies <- filter(D1_funding_by_company,Total_inv == D1_top_funded_amount[1,2])
  D1_top_company_name <- filter(D1,D1$company_permalink == D1_top_companies$Permalink)[1,"name"]
  
  #similarly for d2 and d3....
  
  
  #Subset D2 to keep only top sector rows.
  D2_subset_top_sector = filter(D2,sector_names == top_D2_sector_nbr[1,1]) 
  #Aggregate by company permalink for top sector
  D2_funding_by_company <- setNames(aggregate(D2_subset_top_sector$raised_amount_usd,by= list(D2_subset_top_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D2_top_funded_amount <- arrange(D2_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D2_top_companies <- filter(D2_funding_by_company,Total_inv == D2_top_funded_amount[1,2])
  D2_top_company_name <- filter(D2,D2$company_permalink == D2_top_companies$Permalink)[1,"name"]

  #and D3...  
  
  
  #Subset D3 to keep only top sector rows.
  D3_subset_top_sector = filter(D3,sector_names == top_D3_sector_nbr[1,1]) 
  #Aggregate by company permalink for top sector
  D3_funding_by_company <- setNames(aggregate(D3_subset_top_sector$raised_amount_usd,by= list(D3_subset_top_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D3_top_funded_amount <- arrange(D3_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D3_top_companies <- filter(D3_funding_by_company,Total_inv == D3_top_funded_amount[1,2])
  D3_top_company_name <- filter(D3,D3$company_permalink == D3_top_companies$Permalink)[1,"name"]
  
#5.10 For point 4 (second best sector count-wise), which company received the highest investment?
  
  
  #Subset D1 to keep only second sector rows.
  D1_subset_second_sector = subset(D1,D1$sector_names == second_D1_sector_nbr[1,1])
  #Aggregate by company permalink for second sector
  D1_funding_by_company <- setNames(aggregate(D1_subset_second_sector$raised_amount_usd,by= list(D1_subset_second_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D1_second_funded_amount <- arrange(D1_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D1_second_companies <- filter(D1_funding_by_company,Total_inv == D1_second_funded_amount[1,2])
  D1_second_company_name <- filter(D1,D1$company_permalink == D1_second_companies$Permalink)[1,"name"]
  
  
  #Subset D2 to keep only second sector rows.
  D2_subset_second_sector = subset(D2,D2$sector_names == second_D2_sector_nbr[1,1])
  #Aggregate by company permalink for second sector
  D2_funding_by_company <- setNames(aggregate(D2_subset_second_sector$raised_amount_usd,by= list(D2_subset_second_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D2_second_funded_amount <- arrange(D2_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D2_second_companies <- filter(D2_funding_by_company,Total_inv == D2_second_funded_amount[1,2])
  D2_second_company_name <- filter(D2,D2$company_permalink == D2_second_companies$Permalink)[1,"name"]
  
  
  #Subset D3 to keep only second sector rows.
  D3_subset_second_sector = subset(D3,D3$sector_names == second_D3_sector_nbr[1,1])
  #Aggregate by company permalink for second sector
  D3_funding_by_company <- setNames(aggregate(D3_subset_second_sector$raised_amount_usd,by= list(D3_subset_second_sector$company_permalink),FUN=sum,na.rm=TRUE),c("Permalink","Total_inv"))
  #Sort by descending total investment in a company
  D3_second_funded_amount <- arrange(D3_funding_by_company,desc(Total_inv))
  #Reason for filtering like this is , there may be more than 1 company, so we cant just sort desc and pick 1
  D3_second_companies <- filter(D3_funding_by_company,Total_inv == D3_second_funded_amount[1,2])
  D3_second_company_name <- filter(D3,D3$company_permalink == D3_second_companies$Permalink)[1,"name"]
  

##########################################
#######Checkpoint 5 End###################
##########################################


##########################################
########Checkpoint 6 Start################
##########################################

# This checkpoint is on Tableau

##########################################
#######Checkpoint 6 End###################
##########################################

#-----***Write Unit Tests***---------
# These tests are written by analysing the data of companies
# and rounds2 using excel and then populating certain constants.
# These constants are then compared with the output of the R code
# above. If they match, then "SUCCESS" else "FAILURE"
# These kind of tests ensure that any code change above (say done
# for performance or formatting etc.) doesnt change the expected
# results.
# Chose type is venture (As per analysis done on xls)
# For Venture funding type
# IND total 2976543602 
# GBR total 5436843539
# USA total 108531347515
#
l = ls()
if (sum(D1$raised_amount_usd, na.rm = TRUE) == 108531347515
    & sum(D2$raised_amount_usd, na.rm = TRUE) == 5436843539
    & sum(D3$raised_amount_usd, na.rm = TRUE) == 2976543602
    & FT == "venture") {
  cat('Code looks fine')
} else {
  cat('!!!****ISSUE****!!!')
}
