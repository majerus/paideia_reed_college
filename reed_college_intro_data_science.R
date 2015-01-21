## Intro to Data Science using rvest, dplyr and ggplot2 
## Reed College 
## 1/23/2015

# R version 3.0 or higher is required for this class ----------------------

R.Version()$major == '3' # should be True if not follow steps 1-3 here: http://reed.edu/data-at-reed/software/R/r_studio.html 

# install, update and load packages -----------------------------------------------
pkg <- c("devtools", "rvest", "stringr", "ggthemes",  "dplyr", "ggplot2",  "magrittr", "ggmap", "RColorBrewer", "htmlwidgets", "knitr", "magrittr", "RCurl")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}

update.packages(checkBuilt=TRUE, ask=FALSE)

library(knitr)
library(devtools)
library(htmlwidgets)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(magrittr)
library(RCurl)

if (!require("rvest")) devtools::install_github("hadley/rvest")
library(rvest)

if (!require("leaflet")) devtools::install_github("rstudio/leaflet")
library(leaflet)


# scrape data from Washington Post into a data frame -------------------------------------------
webpage <- html("http://apps.washingtonpost.com/g/page/local/college-grants-for-the-affluent/1526/")

wp_data <-
  webpage %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

# clean colnames and data types -------------------------------------------

# create function to clean col names
clean.names <- function(df){
  colnames(df) <- gsub("[^[:alnum:]]", "", colnames(df))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

# create function to convert curreny vars to numeric
currency.to.numeric <- function(x){
  x <- gsub('\\$','', as.character(x))
  x <- gsub('\\,','', as.character(x))
  x <- as.numeric(x)
  return(x)
}

wp_data %<>% 
  clean.names() %>%
  rename(comp_fee = tuitionfeesroomandboard201314,
         p_no_need_grant = percentoffreshmenreceivingnoneedgrantsfromschool,
         ave_no_need_grant = averagenoneedawardtofreshmen,
         p_need_grant = percentreceivingneedbasedgrantsfromschool) %>%
  mutate(state = as.factor(state),                                                 # strings to factors 
         sector = as.factor(sector),
         comp_fee = currency.to.numeric(comp_fee),                                 # currencies to numeric 
         ave_no_need_grant = currency.to.numeric(ave_no_need_grant), 
         p_no_need_grant = ifelse(is.na(p_no_need_grant), .5, p_no_need_grant))    # missing values to non-missing 




# geocode and map data ----------------------------------------------------

# geocode school locations (takes about two minutes, you can run this code after the course to try it out if you'd like) 
# gc <- do.call(rbind, lapply(as.character(wp_data$school), geocode))
# wp_data <- cbind(wp_data, gc) # cbind geocodes back onto data

# for the class we will just read in geocodes and cbind them onto our data 
x <- getURL("https://raw.githubusercontent.com/majerus/paideia_reed_college/master/geocodes.csv")
gc <- read.csv(text = x)
gc$X <- NULL
wp_data <- cbind(wp_data, gc)

# let's take a quick look at where all the colleges in the data are located
leaflet(wp_data) %>% 
    addTiles() %>% 
    setView(-93.65, 42.0285, zoom = 3) %>%
    addCircles(wp_data$lon, wp_data$lat) 


# it might be helpful to color code the schools based on a variable in the data 

pal <- colorQuantile("YlOrRd", NULL, n = 6) # define color pal

leaflet(wp_data) %>% 
  addTiles() %>% 
  setView(-93.65, 42.0285, zoom = 3) %>%
  addCircles(wp_data$lon, wp_data$lat) %>%
    addCircles(wp_data$lon, wp_data$lat, color = ~pal(ave_no_need_grant)) %>%
    addCircleMarkers(wp_data$lon, wp_data$lat, color = ~pal(ave_no_need_grant)) # darker circles indicate higher values 





### STEP 1: CREATING NEW VARIABLES


# EXAMPLE: create a region (south) variable  -----------------------------------------------
table(wp_data$state)
state.list <- c('AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'NC', 'SC', 'TN', 'TX')

wp_data %<>%
  mutate(south = ifelse(state %in% state.list, 'south', 'non-south'))





# EXERCISE: create a region (NE) variable  -----------------------------------------------
  # change the above code to create a binary variable that identifies the following states
state.list <- c('CT', 'DC', 'DE', 'MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT')







### STEP 2: ARRANGE and SELECT DATA 



# EXAMPLE: arranging/sorting the data by region (south)  -----------------------------------------------

# first let's sort the data by the south region variable

wp_data %<>% 
  arrange(south)

# the default for arrange is ascending, let's put that in descending order instead

wp_data %<>% 
  arrange(desc(south))

# it might be more useful to sort the data by multiple variables

wp_data %<>% 
  arrange(desc(south), ave_no_need_grant)

# we can also create a new data frame with just these variables while executing this code

south.data <- 
wp_data %>%                            # note change in operator 
  arrange(desc(south), ave_no_need_grant) %>%
  select(south, ave_no_need_grant)

# oops we forgot to include school name and sector

south.data <- 
  wp_data %>%                            
  arrange(desc(south), ave_no_need_grant) %>%
  select(school, sector, south, ave_no_need_grant)



# EXERCISE: create an ordered data frame called north_east that contains three variables (school, ne, and ave_no_need_grant) -----------------------------------------------






### STEP 3: SUMMARIZE YOUR DATA 


# EXAMPLE: find the mean and standard deviation of each variable by region (south)  -----------------------------------------------

south.data %>%
  group_by(south) %>%
  summarise_each(funs(mean(.), sd(.)),                      
                 ave_no_need_grant) 



# EXERCISE: find the mean and standard deviation of ave_no_need_grant by region (Northeast)  -----------------------------------------------
    # add min and max to your summary statistics






### STEP 4: VISUALIZE YOUR DATA (HISTORGRAMS)


# EXAMPLE: Create histograms of ave_no_need_grant (south)  -----------------------------------------------

ggplot(south.data, aes(x=ave_no_need_grant, fill=as.factor(south))) + 
  geom_histogram() +
  theme_classic() +
  facet_wrap( ~  south, ncol=2)

ggplot(south.data, aes(x=ave_no_need_grant, fill=as.factor(south))) + 
  geom_histogram() +
  theme_classic() +
  facet_grid(south ~  sector)

ggplot(wp_data, aes(x=ave_no_need_grant, fill=as.factor(south))) + 
  geom_histogram() +
  theme_classic() +
  facet_grid(south ~  sector)



# EXERCISE: Replicate the above histograms of ave_no_need_grant using your northeast variable and data  -----------------------------------------------
  # Bonus: change the colors of your histograms, change the theme of your graph, and/or add a title







### STEP 5: VISUALIZE YOUR DATA (MAPS)


# EXAMPLE: Create a state-level choropleth map of the percent of students receiving non-need based aid --------

# calculate state level means for the percent of students receiving no need grants
state_means <- 
  wp_data %>%
  select(state, ave_no_need_grant) %>%               
  group_by(state) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%    
  arrange(desc(ave_no_need_grant)) 

# join state names onto state level data
state_means$state_name <- tolower(state.name[match(state_means$state, state.abb)])

# load mapping data 
state_df <- map_data("state")

# join state means and mapping data 
choropleth <- merge(state_df, state_means, by.x = 'region', by.y = 'state_name', all.x=T, all.y=F)

# order data 
choropleth <- choropleth[order(choropleth$order), ]

# define color pal
my.cols <- brewer.pal(8, "Greens")

# create cuts for % no need grants
choropleth$ave_no_need_grant_d <- cut(choropleth$ave_no_need_grant, breaks = c(seq(0, 25000, by = 3125))) 

# create map
ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = ave_no_need_grant_d)) +
  geom_path(color="white") +
  scale_fill_manual("Ave. \nNo Need Grant", values = my.cols, guide = "legend") +
  theme_classic()






# EXERCISE: Replicate the above map using a different numeric variable  --------
  # Bonus: change the color scheme, add a title, change the chart theme and/or make multiple maps









