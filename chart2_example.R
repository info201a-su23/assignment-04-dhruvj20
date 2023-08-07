### Sources:

### https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### https://environmentalcomputing.net/graphics/ggplot/ggplot-labels/
### https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
### https://r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html
### https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c470c37e-a4fd-40d7-8c78-b0530123d189
### https://www.tutorialspoint.com/how-to-remove-all-rows-having-na-in-r#:~:text=To%20remove%20all%20rows%20having%20NA%2C%20we%20can%20use%20na,omit(df).


### Copy pasted the read CSV and the 2010 and beyond filtering from Chart 1

prisjailpop_per100k_percountystate1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true",
header = TRUE, sep = ",", stringsAsFactors = TRUE)

### Reading the CSV link to the per 100k post-1990 incarcerated population dataset 
### as a dataframe

smaller_2010_prisjaildata <- prisjailpop_per100k_percountystate1990 %>% 
dplyr::group_by(year) %>% dplyr::filter(year >= 2010)

### Creating a new dataset from the previous dataset to only show data from 2010 onwards

black_incarceration_2010_prisjail <- smaller_2010_prisjaildata %>% 
select(year, white_prison_pop_rate, black_prison_pop_rate)

### Selecting the year, total population, jail rates for black people and then creating a 
### new data frame

dataset_without_NA <- na.omit(black_incarceration_2010_prisjail)

### Omitting all of the NA values (otherwise graphing and filtering becomes very difficult)

filtered_dataset <- dataset_without_NA %>%
filter(white_prison_pop_rate <= 2000 & black_prison_pop_rate <= 12500)

### Filtering out a lot of outliers to get a better looking trend 
### (still not very good in terms of R^2 and correlation but better than before)

library(ggplot2)

black_incarceration_vs_white_incarceration <- 
ggplot(filtered_dataset, 
aes(x = filtered_dataset$white_prison_pop_rate, 
y = filtered_dataset$black_prison_pop_rate)) +
geom_point() + labs(x = "White Prison Population Rate (People)", 
y = "Black Prison Population Rate (People)", 
title = "Comparing Prison Population Rates between White People and Black People") +
theme(plot.title = element_text(hjust = 0.5))

### Making the scatterplot (using all of the same features from chart 1)