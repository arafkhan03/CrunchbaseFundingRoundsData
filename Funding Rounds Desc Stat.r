colMeans(is.na(funding_rounds))

# Funding Rounds Data Descriptive Stats

# 1.0 Data Preparation and Library Loading
library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

        ## Summary
dim(funding_rounds)
skim(funding_rounds)

        ## Only Keep Columns that have NAs Less than 75%
funding_rounds_NA <- as.data.frame(
        round(colMeans(is.na(funding_rounds)), 2)
)
sum(funding_rounds_NA$`round(colMeans(is.na(funding_rounds)), 2)` < 0.75)
funding_rounds_WNA <- funding_rounds[, funding_rounds_NA$`round(colMeans(is.na(funding_rounds)), 2)` < 0.75]

        ## Summary
skim(funding_rounds_WNA)






# 2.0 Top Organizations by Number of Funding Rounds
org_count_funding_rounds <- funding_rounds_WNA %>%
        dplyr::group_by(`Organization Name`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Orgs = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
org_count_funding_rounds <- arrange(org_count_funding_rounds, 
                                    desc(org_count_funding_rounds$Count_by_Orgs))




# 3.0 Funding Rounds by Number of Funding Types
fundtype_count_funding_rounds <- funding_rounds_WNA %>%
        dplyr::group_by(`Funding Type`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Fund_Types = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

fundtype_count_funding_rounds <- arrange(fundtype_count_funding_rounds, 
                                    desc(fundtype_count_funding_rounds$Count_by_Fund_Types))





plot(funding_rounds_WNA$`Money Raised Currency (in USD)`, funding_rounds_WNA$`Transaction Name`, type = 'l')


getwd()




