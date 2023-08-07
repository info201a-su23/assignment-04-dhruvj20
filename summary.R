### https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### https://canvas.uw.edu/courses/1643812/files/folder/Lectures_PG
### https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c470c37e-a4fd-40d7-8c78-b0530123d189
### https://www.digitalocean.com/community/tutorials/get-number-of-rows-and-columns-in-r
### Used these sources

prisjailpop_per100k_percountystate1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true",
header = TRUE, sep = ",", stringsAsFactors = TRUE)

### Reading the CSV link to the per 100k post-1990 incarcerated population dataset 
### as a dataframe

smaller_2010_prisjaildata <- prisjailpop_per100k_percountystate1990 %>% 
dplyr::group_by(year) %>% dplyr::filter(year >= 2010)

### Values 1 & 2

### Creating a new dataset from the previous dataset to only show data from 2010 onwards

black_prispop_rate_condensed <- smaller_2010_prisjaildata %>%
dplyr::select(year, state, county_name, urbanicity, black_prison_pop_rate)

### Creating a new dataset from the previous dataset to only show the selected columns
### to compare urbanicity and black prison population rate

black_prispop_ranked_by_urbanicity <- black_prispop_rate_condensed %>% 
dplyr::arrange(desc(black_prison_pop_rate))

### Creating a new dataset that arranges the previous dataset's black prison population rate
### column from maximum to minimum

highest_20_urbanicity <- head(black_prispop_ranked_by_urbanicity, 20)

### Creating a new dataframe that prints the highest 20 black prison population rates

highest_50_urbanicity <- head(black_prispop_ranked_by_urbanicity, 50)

### Creating a new dataframe that prints the highest 50 black prison population rates

rural_urbanicity_20 <- highest_20_urbanicity %>% filter(urbanicity == "rural") %>% 
nrow()

small_mid_urbanicity_20 <- highest_20_urbanicity %>% filter(urbanicity == "small/mid") %>% 
nrow()

rural_urbanicity_50 <- highest_50_urbanicity %>% filter(urbanicity == "rural") %>% 
nrow()

small_mid_urbanicity_50 <- highest_50_urbanicity %>% filter(urbanicity == "small/mid") %>% 
nrow()

suburban_urbanicity_50 <- highest_50_urbanicity %>% filter(urbanicity == "suburban") %>% 
nrow()

### Creating variables for all the numbers I want to show in R markdown by filtering
### and counting rows

rural_in_20 <- function(){
  this_num = rural_urbanicity_20
  return (this_num)
}

smallmid_in_20 <- function(){
  this_num = small_mid_urbanicity_20
  return (this_num)
}

rural_in_50 <- function(){
  this_num = rural_urbanicity_50
  return (this_num)
}

suburban_in_50 <- function(){
  this_num = suburban_urbanicity_50
  return (this_num)
}

smallmid_in_50 <- function(){
  this_num = small_mid_urbanicity_50
  return (this_num)
}

### Calling functions to return each of these values in R Markdown

### Values 3-12

latinx_white_jailpop_rate_condensed <- smaller_2010_prisjaildata %>%
dplyr::select(year, state, county_name, latinx_jail_pop_rate, white_jail_pop_rate)

### Condensing the smaller data frame (2010 data onward) previously mentioned to a new
### data frame with the latinx and white jail populations for future comparison

latinx_white_jailpop_rate_condensed <- latinx_white_jailpop_rate_condensed %>% 
dplyr::mutate(latinx_white_differential = latinx_jail_pop_rate - white_jail_pop_rate)

### Mutating the previous dataframe to create a new dataframe with 
### the differential between jail-incarcerated
### Latinx individuals and white individuals to show discrepancies

differential_ranking_desc <- latinx_white_jailpop_rate_condensed %>% 
dplyr::arrange(desc(latinx_white_differential))

### Ranking the differentials between incarceration of latinx people 
### and incarceration of white people in descending order

differential_ranking_asc <- latinx_white_jailpop_rate_condensed %>% 
dplyr::arrange(latinx_white_differential)

### Ranking the differentials between incarceration of latinx people 
### and incarceration of white people in ascending order

iqr_differential <- latinx_white_jailpop_rate_condensed %>% 
summarise(iqr = IQR(na.omit(latinx_white_differential))) %>%
select(iqr) %>% pull()

### Pulling a vector of the interquartile range of
### differentials between incarceration of latinx people and incarceration of 
### white people sorted by year

create_fivenum_summary <- function(yearnum){
### Creating a function that gets a five number summary for each year 
### Fivenum is (min, Q1, median, Q3, max)
    quartiles_differential = latinx_white_jailpop_rate_condensed %>% 
    summarise(quartiles = fivenum(na.omit(latinx_white_differential))) %>%
    filter(year == yearnum) %>% select(quartiles) %>% pull()
### Calling fivenum operation on the differentials and then filtering by year
### to get the fivenum summary for each year from 2010-2018, selecting only the quartiles,
### and pulling that value
    return (quartiles_differential)
### Returning the quartiles_differential variable set to the previous operation
}

### For the next 9 values: calling the function to get the fivenum summary from 2010-2018

quart_diff_2010 <- create_fivenum_summary(2010)

quart_diff_2011 <- create_fivenum_summary(2011)

quart_diff_2012 <- create_fivenum_summary(2012)

quart_diff_2013 <- create_fivenum_summary(2013)

quart_diff_2014 <- create_fivenum_summary(2014)

quart_diff_2015 <- create_fivenum_summary(2015)

quart_diff_2016 <- create_fivenum_summary(2016)

quart_diff_2017 <- create_fivenum_summary(2017)

quart_diff_2018 <- create_fivenum_summary(2018)

### Sources: 
### https://www.statology.org/create-table-in-r/
### https://www.guru99.com/r-matrix-tutorial.html
### Help function for creating matrix

full_fivenum_list <- c(quart_diff_2010, quart_diff_2011, quart_diff_2012, quart_diff_2013,
quart_diff_2014, quart_diff_2015, quart_diff_2016, quart_diff_2017, quart_diff_2018)

### Creating a list out of all the fivenum values for all the years (list of 45)

quartiles_colnames <- c('Minimum','Q1','Median', 'Q3', 'Maximum')

### Creating a list out of all the five number summary values to represent all column
### headers

quartiles_rownames <- c('2010','2011','2012', '2013', '2014', '2015', '2016', 
'2017', '2018')

### Creating a list oit of all the years in my analysis to represent all the row headers

quartile_differentials_matrix <- matrix(full_fivenum_list, nrow=9, ncol=5, byrow=TRUE,
dimnames = list(quartiles_rownames, quartiles_colnames))

### Creating a matrix of all the quartile differentials using all the variables above

quartile_differentials_table <- as.table(quartile_differentials_matrix)

### Creating a table out of that matrix

smallest_max <- function(){
  this_num  = quart_diff_2010[5]
  return (this_num)
}

### Creating a function that returns the smallest maximum for R Markdown

largest_max <- function(){
  this_num = quart_diff_2018[5]
  return (this_num)
}

### Creating a function that returns the largest maximum for R Markdown