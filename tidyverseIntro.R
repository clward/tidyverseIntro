# Intro to tidyr and dplyr packages
# Colette Ward
# 14 Jan 2018


# Install and load packages
install.packages("tidyverse") # installs the 8 core tidyverse packages (readr, tibble, tidyr, dplyr, stringr, forcats, purrr, ggplot2)
install.packages("lubridate")
library(tidyverse); library(lubridate)


##########################################
##########################################
##########################################

# Data cleaning with tidyr


# 1. gather(), spread()
# example is from cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html
weather <- as_tibble(read.csv("./data/weather.csv", stringsAsFactors = FALSE))
weather


weather2 <- gather(weather, key = weirdDay, value = temp, d1:d31, na.rm = TRUE) # gather the 31 day columns into a new variable called "day"
weather2


weather3 <- weather2 %>% # start with weather2
  mutate(day = parse_number(weirdDay)) %>% # dplyr::mutate() creates a new variable called day using the calculation specified on the right side of "="
  select(id, year, month, day, element, temp) %>% # the intended purpose of select() is to retain only those variables you want to keep, but it can also be used to set the order of columns
  arrange(id, year, month, day) # re-order rows
weather3


# This dataset is mostly tidy, but the element column is not a variable - it stores the names of variables
# spread() does the inverse of gather() - it spreads the element and value columns back out into the columns:
weather4 <- weather3 %>% spread(element, temp)
weather4

# Now we have tidy data!




# Note these steps can all go into a single pipe:
weather5 <- weather %>%
  gather(weirdDay, temp, d1:d31, na.rm = TRUE) %>%
  
  mutate(day = parse_number(weirdDay)) %>%
  select(id, year, month, day, element, temp) %>%
  arrange(id, year, month, day) %>%
  
  spread(element, temp)
weather5


###########

# 2. Dealing with missing values: drop_na(), fill(), replace_na()

weather6 <- gather(weather, key = weirdDay, value = temp, d1:d31) 
View(weather6)

# Remove rows where temp == NA
weather7 <- drop_na(weather6, temp)
weather7

# fill in NA's in temp with the most recent non-NA value:
weather8 <- fill(weather6, temp)
View(weather8)

# replace NAs in temp
weather9 <- replace_na(weather6, list(temp = 42))
View(weather9)


###########

# 3. Split / combine cells: separate(), unite()
# 4. Expand tables: complete(), expand()

# We'll do an example with the missing zero problem

gill <- as_tibble(read.csv("./data/gillnet.csv", stringsAsFactors = FALSE))
View(gill)


# Say we want to fill in zeros for all combinations of date & commonName 
# (i.e. create a record of cue = 0 and cueWt = 0 whenever a species was NOT observed)
length(unique(gill$date)) # 2
length(unique(gill$commonName)) # 9
# so, a table with all combinations of date & commonName should have 18 rows
nrow(gill) # 12 ... so, gill has missing zeros

gill2 <- gill %>%
  select(date, commonName, cue, cueWt) %>%
  complete(date, commonName, fill = list(cue = 0, cueWt = 0))
View(gill2)


# But what if I also care about the individual samples on each date? (the more likely scenario)

# Note that you can use complete() to get all combinations of >2 columns
# Let's try that for all unique combinations of date, sample, & commonName:
gill3 <- gill %>%
  select(date, sample, commonName, cue, cueWt) %>%
  complete(date, sample, commonName, fill = list(cue = 0, cueWt = 0))
View(gill3)

# This does not yield what we want! It creates non-existent sammples (e.g. Sample 5 on 6/18/74)
# Instead, we need to bookend complete() with unite() and separate() to get the intended outcome:
# Use complete() for all combations of (date, sample) & commonName:
gill4 <- gill %>%
  select(date, sample, commonName, cue, cueWt) %>%
  unite(dateSample, date, sample, sep = ",") %>% # need all combinations of each sampling event & species, ie [date, sample] & commonName; n = 45
  complete(dateSample, commonName, fill = list(cue = 0, cueWt = 0)) %>%
  separate(dateSample, into = c("date", "sample"), sep = ",", convert = T)
View(gill4)

# An alternative to complete() is expand(), which creates a new tibble with all possible combinations of the variables you choose


##########################################
##########################################
##########################################

# Use dplyr to wrangle the cleaned data


# A) working with single tables
gill5 <- gill4 %>%
  
  # 1. select cases (rows):
  filter(commonName %in% c("Alewife", "Gizzard shad", "Northern pike", "White perch", "Yellow perch")) %>%
  #filter(commonName == "Alewife")
  #filter(cueWt > 0)
  #filter(commonName %in% c("Alewife", "Gizzard shad", "Northern pike", "White perch", "Yellow perch"),
         #cueWt > 0)
  #filter(is.na(cue))

  
  # 2. reorder cases (rows):
  arrange(commonName) %>% # works with numeric variables too
  #arrange(desc(commonName)) %>%
  
  
  # 3. add new variables that are created from existing variables: mutate(), transmute()
  mutate(date = readr::parse_date(date, c('%m/%d/%y')), # overwrites the old date
         yr = lubridate::year(date), # creates a new variable called "yr"
         month = lubridate::month(date),
         julien = lubridate::yday(date)) %>%
  #transmute(julien = lubridate::yday(date))
  
  
  # 4. select variables: select(), rename()
  #select(yr, sample, commonName, cueWt)
  #select(-cue, -date) %>%
  #select(commonName, yr, sample, cueWt) # select and reorder variables
  #select(date:cue)
  #select(starts_with("cue"))
  #select(ends_with("e"))
  #select(contains("t"))
  
  #rename(catch = cueWt, year = yr)
  select(year = yr, month, julian = julien, sample, commonName, cueWt) %>% # you can combine select() and rename()

  # 5. Condense multiple values into a single value: group_by() %>% summarize() %>% ungroup()
  group_by(year, commonName) %>%
  summarize(meanCueWt = mean(cueWt),
            maxCueWt = max(cueWt)) %>%
  ungroup() %>% # always do an ungroup() after summarize(), otherwise you'll get funky results from remaining manipulations
  
  # 6. Take random samples of rows: sample_n(), sample_frac()
  group_by(year) %>% 
  sample_n(size = 2, replace = TRUE) %>% # take 2 random samples for each year
  #sample_frac(size = 0.6, replace = FALSE) %>% # sample 60% of rows from each year
  ungroup()

View(gill5)


###########

# B) Combining multiple tables

# Example: add a look-up table with nutrient values

# create a tibble by rows:
nut <- tibble::tribble(~year, ~totPhos, ~TKN,
                       1972, 55, 300,
                       1974, 47, 250)

# or by columns:
nut <- tibble::tibble(year = c(1972, 1974), 
                      totPhos = c(55, 47),
                      TKN = c(300, 250))
nut


gillNut <- left_join(gill5, nut, by = "year")
View(gillNut)