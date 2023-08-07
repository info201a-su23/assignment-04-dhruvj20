### Sources:
### https://www.geeksforgeeks.org/how-to-create-a-plot-using-ggplot2-with-multiple-lines-in-r/#
### https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### https://environmentalcomputing.net/graphics/ggplot/ggplot-labels/
### https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
### https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
### https://r-graph-gallery.com/line-chart-ggplot2.html
### https://www.statology.org/ggplot-line-type/
### https://www.r-bloggers.com/2022/07/how-to-change-background-color-in-ggplot2-3/ (reference)
### https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651/3
### https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=c470c37e-a4fd-40d7-8c78-b0530123d189


prisjailpop_per100k_percountystate1990 <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates-1990.csv?raw=true",
header = TRUE, sep = ",", stringsAsFactors = TRUE)

### Reading the CSV link to the per 100k post-1990 incarcerated population dataset 
### as a dataframe

smaller_2010_prisjaildata <- prisjailpop_per100k_percountystate1990 %>% 
dplyr::group_by(year) %>% dplyr::filter(year >= 2010)

### Creating a new dataset from the previous dataset to only show data from 2010 onwards

adjusted_2010_prisjaildata <- smaller_2010_prisjaildata %>% mutate(total_aapi_pop_rate
= aapi_jail_pop_rate + aapi_prison_pop_rate, total_black_pop_rate = black_jail_pop_rate + 
black_prison_pop_rate, total_latinx_pop_rate = latinx_jail_pop_rate + latinx_prison_pop_rate, 
total_native_pop_rate = native_jail_pop_rate + native_prison_pop_rate,
total_white_pop_rate = white_jail_pop_rate + white_prison_pop_rate)

### Adding columns that combine prison & jail population rates to new dataframe
### for each race

totals_only <- adjusted_2010_prisjaildata %>% select(year, total_aapi_pop_rate, 
total_black_pop_rate, total_latinx_pop_rate, total_native_pop_rate, total_white_pop_rate)

### Selecting for only the year and the total columns

totals_median <- totals_only %>% group_by(year) %>% summarise(aapi_med =
median(na.omit(total_aapi_pop_rate)), black_med = median(na.omit(total_black_pop_rate)),
latinx_med = median(na.omit(total_latinx_pop_rate)), native_med = 
median(na.omit(total_native_pop_rate)), white_med = 
median(na.omit(total_white_pop_rate))) %>% filter(year <= 2016)

### Created a new dataset with the year and calculated all the relevant medians for
### each race

colors <- c("Asian Americans/Pacific Islanders" = "purple", "Black People" = "green",
"Latinx People" = "red", "Native Americans" = "blue", "White People" = "orange")

time_plot <- ggplot(totals_median, aes(year)) +
geom_line(aes(y = aapi_med, color = "Asian Americans/Pacific Islanders"), 
size = 1, linetype = 2) + geom_line(aes(y = black_med, color = "Black People"), 
size = 1, linetype = 2) + geom_line(aes(y = latinx_med, color = "Latinx People"), 
size = 1, linetype = 2) + geom_line(aes(y = native_med, color = "Native Americans"), 
size = 1, linetype = 2) + geom_line(aes(y = white_med, color = "White People"), 
size = 1, linetype = 2) + labs(y = "Median Prison + Jail Incarceration Rate (Per 100K)", 
x = "Year (2010-2016)", title = "Incarceration Rates by Race vs Time", color = "Legend") + 
theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = colors)

