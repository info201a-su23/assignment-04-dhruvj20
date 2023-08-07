### Sources:

### https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### https://environmentalcomputing.net/graphics/ggplot/ggplot-labels/
### https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
### https://r-graph-gallery.com/278-the-maps-library.html
### https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c470c37e-a4fd-40d7-8c78-b0530123d189

### Copy pasted the read CSV and the 2010 and beyond filtering from Chart 1

prisjailpop_per100k_percountystate1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true",
header = TRUE, sep = ",", stringsAsFactors = TRUE)

### Reading the CSV link to the per 100k post-1990 incarcerated population dataset 
### as a dataframe

smaller_2010_prisjaildata <- prisjailpop_per100k_percountystate1990 %>% 
dplyr::group_by(year) %>% dplyr::filter(year >= 2010)

### Creating a new dataset from the previous dataset to only show data from 2010 onwards

smaller_2018_prisjaildata <- smaller_2010_prisjaildata %>%
dplyr::filter(year >= 2018)

### Filtering the previous dataset to the year 2018 and creating a new data frame

black_prison_data <- smaller_2018_prisjaildata %>% select(year, county_name, state, 
black_jail_pop_rate)

### Selecting the relevant columns from the previous dataset and creating a new dataframe

na.omit(black_prison_data$black_jail_pop_rate)

### Omitting NA values from the jail incarceration rate column corresponding to black people

### black_prison_data_aggregate %>% group_by %>% sum(na.omit(black_jail_pop_rate))
### Couldn't get past this line

library(maps)
US_map <- map('state')