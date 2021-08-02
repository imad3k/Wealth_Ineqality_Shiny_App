# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# install.packages("tidytuesdayR")
library(tidytuesdayR)
library(rmarkdown)
library(flexdashboard)
library(gifski)
library(gganimate)
library(transformr)
library(ggplot2)
library(plotly)


# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-02-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

lifetime_earn <- tuesdata$lifetime_earn

# Or read in the data manually

lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')





# Cleaning

library(tidyverse)
library(readxl)

### Urban Institute

# Student Debt ------------------------------------------------------------



student_debt_raw <- read_excel("Data/StudentLoans.xlsx", skip = 1)

student_debt_val <- student_debt_raw %>%
  rename(year = 1) %>%
  slice(1:10) %>%
  arrange(desc(year)) %>%
  pivot_longer(cols = -year, names_to = "race", values_to = "loan_debt") %>%
  mutate(across(c(year, loan_debt), as.double))

student_debt_pct <- student_debt_raw %>%
  rename(year = 1) %>%
  slice(14:23) %>%
  arrange(desc(year)) %>%
  pivot_longer(cols = -year, names_to = "race", values_to = "loan_debt_pct") %>%
  mutate(across(c(year, loan_debt_pct), as.double))

student_debt <- left_join(student_debt_val, student_debt_pct, by = c("year", "race"))

student_debt 

student_debt %>% 
  write_csv("Data/student_debt.csv")

#### TESTTING GGPLOT ######

s_debt <- ggplot(data = student_debt, 
                 mapping =  aes(x = year,
                                y = loan_debt,
                                color = race)) +
  geom_point(size = 3) +
  geom_line() +
  theme_stata()  
# geom_smooth(method = lm, se = F) +
# facet_wrap(~ race) 

s_debt


s_debt.animation = s_debt +
  transition_reveal(year)

s_debt.animation





# Retirement --------------------------------------------------------------


retirement_raw <- read_excel("Data/Retirement.xlsx", skip = 1)

retirement <- retirement_raw %>%
  rename(year = 1) %>%
  slice(1:10) %>%
  mutate(across(everything(), as.double)) %>%
  pivot_longer(-year, names_to = "race", values_to = "retirement")

retirement

retirement %>% 
  write_csv("Data/retirement.csv")

#### TESTTING GGPLOT ######

ggplot(data = retirement,
       mapping = aes(x = year,
                     y = retirement,
                     color = race)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~ race)



# Home Ownership ----------------------------------------------------------

home_raw <- read_excel("Data/Homeownership.xlsx", skip = 1)

home_owner <- home_raw %>%
  rename(year = 1) %>%
  filter(!is.na(Black)) %>%
  mutate(across(everything(), as.double)) %>%
  pivot_longer(-year, names_to = "race", values_to = "home_owner_pct")

home_owner %>% 
  write_csv("Data/home_owner.csv")

#### TESTTING GGPLOT ######

ggplot(data = home_owner,
       mapping = aes(x = year,
                     y = home_owner_pct,
                     color = race)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~ race)

# Lifetime Earnings -------------------------------------------------------

lifetime_raw <- read_excel("Data/LifetimeEarnings.xlsx")

lifetime_earn <- lifetime_raw %>%
  set_names(nm = c("gender", "race", "lifetime_earn")) %>%
  fill(gender) %>%
  filter(!is.na(race))

lifetime_earn

lifetime_earn %>% 
  write_csv("Data/lifetime_earn.csv")

l_earn <- ggplot(data = lifetime_earn,
                 mapping = aes(x = race,
                               y = lifetime_earn,
                               color = gender)) +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~ race)



# Wealth Lifetime ---------------------------------------------------------

wealth_life_raw <- read_excel("Data/WealthbyRaceoverLifetime.xlsx", skip = 1)

wealth_life <- wealth_life_raw %>%
  rename(race = 1) %>%
  filter(!is.na(`1983`)) %>%
  filter(!is.na(race)) %>%
  mutate(type = rep(c("Average", "Median"), each = 2), .before = "race") %>%
  pivot_longer(cols = c(`1983`:`2016`), names_to = "year", values_to = "wealth_lifetime")

wealth_life

wealth_life %>% 
  write_csv("Data/lifetime_wealth.csv")


# Wealth by Race ----------------------------------------------------------

wealth_race_raw <- read_excel("Data/WealthbyRace.xlsx", skip = 1)

wealth_race <- wealth_race_raw %>%
  rename(year = 1) %>%
  filter(!is.na(White)) %>%
  filter(!is.na(year)) %>%
  mutate(across(everything(), as.double)) %>%
  mutate(type = rep(c("Average", "Median"), each = 12), .before = "year") %>%
  pivot_longer(cols = c(`Non-White`:Hispanic), names_to = "race", values_to = "wealth_family")

wealth_race

wealth_race %>% 
  write_csv("Data/race_wealth.csv")


# Income Distribution -----------------------------------------------------

income_raw <- read_excel("Data/IncomeDistribution.xlsx", skip = 1)

income_time <- income_raw %>%
  filter(!is.na(`10th Percentile`)) %>%
  mutate(year = as.integer(Year)) %>%
  pivot_longer(
    cols = 2:4,
    names_to = "percentile",
    values_to = "income_family",
    names_pattern = "([0-9]+[th]+)"
  ) %>%
  select(year, percentile, income_family)

income_time

income_time %>% 
  write_csv("Data/income_time.csv")


# Wealth Distribution -----------------------------------------------------

wealth_raw <- read_excel("Data/WealthDistribution.xlsx", skip = 1)

wealth_distribution <- wealth_raw %>%
  rename(percentile = ...1) %>%
  filter(!is.na(`1963`)) %>%
  pivot_longer(
    cols = -percentile,
    names_to = "year",
    values_to = "wealth_family"
  ) %>%
  mutate(across(c(year, percentile), as.integer))

wealth_distribution

wealth_distribution %>% 
  write_csv("Data/wealth_distribution.csv")


########################## GRAPH 1 ########################################
####################### ORIGINAL FIRST GRAPH ###########################





graph1 <- wealth_distribution %>%
  ggplot(aes(x = percentile, y = wealth_family, color = year, group = year)) +
  geom_line() +
  scale_color_viridis_c()


graph1





####################### MODIFIED FIRST GRAPH - bar chart ###########################

graph1a <- wealth_distribution %>%
  filter(year >= 1980) %>% 
  
  ggplot(aes(x = percentile, y = wealth_family, fill = year)) +
  geom_col() +
  labs(x = "Population Percentile", y = "Family Wealth (dollars)") +
  facet_wrap(~ year) +
  guides(fill = FALSE) +
  theme(strip.text = element_text(color = "white",
                                  face = "bold",
                                  size = 14),
        strip.background = element_rect(fill = "grey50")) +
  labs(title = "Change in Family Wealth by Population Percentile",
       subtitle = "From Years 1983 - 2016")



graph1a 


########## FINAL PROJECT ############

library(ggthemes)


wealth_dist_chart <- wealth_distribution %>% 
  ggplot(aes(x = percentile,
             y = wealth_family,
             color = year,
             group = year)) +
  geom_point(size = 2.5, show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~year) +
  scale_x_continuous(breaks = c(25, 50, 75, 100)) +
  # scale_y_continuous(breaks = c(0, 6000000, 90000000, 12000000)) +
  ggtitle("Wealth Distribution by Population Percentile Over Time") +
  xlab("Population Percentile") +
  ylab("Wealth") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0),
        panel.grid.major.x = element_line(linetype = 4),
        panel.grid.major.y = element_line(linetype = 4)) +
  scale_color_viridis_c()

wealth_dist_chart

#############################################################

# VS - changed Year and Percentile around

graph1b <-wealth_distribution %>%
  filter(year >= 1990) %>% 
  ggplot(aes(x = year, y = wealth_family, fill = percentile)) +
  geom_col(position = "dodge") +
  # scale_fill_brewer(palette="Dark2") +
  labs(x = "Year", y = "Family Wealth")



graph1b

####################### MODIFIED FIRST GRAPH - boxplot ###########################


graph2a <-wealth_distribution %>%
  ggplot(aes(x = percentile, y = wealth_family, group = year)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Percentile", y = "Family Wealth")



graph2a


####################### MODIFIED FIRST GRAPH - HISTOGRAM BUT NOT WORKING ###########################



graph1b <-wealth_distribution %>%
  ggplot(aes(y = wealth_family, group_by(year))) +
  geom_histogram()


graph1b


### Census


# H1a----------------------------------------------------------------------

h01a_raw <- read_excel("Data/h01a.xlsx", skip = 6)

h1_asian <- h01a_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = rep(c("Asian Alone or in Combination", "Asian Alone"), each = 36),
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 36),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_asian


# h1ar --------------------------------------------------------------------

h01ar_raw <- read_excel("Data/h01ar.xlsx", skip = 5)

h1_all <- h01ar_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "All Races",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 53),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_all


# h1b ---------------------------------------------------------------------

h01b_raw <- read_excel("Data/h01b.xlsx", skip = 6)

h1_black <- h01b_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>% 
  mutate(
    race =
      rep(
        c(
          rep("Black Alone or in Combination", 18),
          rep("Black Alone", 53)
        ),
        times = 2
      ),
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 71),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_black


# h1h ---------------------------------------------------------------------

h01h_raw <- read_excel("Data/h01h.xlsx", skip = 5)

h1_hispanic <- h01h_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "Hispanic",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 48),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_hispanic


# h1w ---------------------------------------------------------------------

h01w_raw <- read_excel("Data/h01w.xlsx", skip = 6)

h01w_raw

h1_white <- h01w_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "White Alone",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 53),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_white


# h1wnh -------------------------------------------------------------------

h01wnh_raw <- read_excel("Data/h01wnh.xlsx", skip = 6)

h01wnh_raw

h1_white_nh <- h01wnh_raw %>%
  rename(year = 1, number = 2, `Top 5%` = 7) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "White, Not Hispanic",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 48),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = Lowest:last_col(),
    names_to = "income_quintile",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  )

h1_white_nh

# h1 combo ----------------------------------------------------------------



all_h1_comb <- bind_rows(
  h1_all,
  h1_asian,
  h1_black,
  h1_hispanic,
  h1_white,
  h1_white_nh
) %>%
  mutate(
    number = as.integer(number),
    number = number * 1000
  )


all_h1_comb %>% 
  write_csv("Data/income_limits.csv")

## H2


# h2a ---------------------------------------------------------------------


h2a_raw <- read_excel("Data/h02a.xlsx", skip = 5)

h2a_raw

h2_asian <- h2a_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = rep(c("Asian Alone or in Combination", "Asian Alone"), each = 18),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_asian

# h2ar --------------------------------------------------------------------

h2ar_raw <- read_excel("Data/h02ar.xlsx", skip = 4)

h2ar_raw

h2_all <- h2ar_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "All Races",
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_all

# h2b ---------------------------------------------------------------------

h2b_raw <- read_excel("Data/h02b.xlsx", skip = 5)

h2b_raw

h2_black <- h2b_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = c(
      rep("Black Alone or in Combination", 18),
      rep("Black Alone", 53)
    ),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_black


# h2h ---------------------------------------------------------------------

h2h_raw <- read_excel("Data/h02h.xlsx", skip = 4)

h2h_raw

h2_hispanic <- h2h_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "Hispanic",
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_hispanic

# h2w ---------------------------------------------------------------------

h2w_raw <- read_excel("Data/h02w.xlsx", skip = 5)

h2w_raw

h2_white <- h2w_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "White Alone",
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_white

# h2wnh -------------------------------------------------------------------

h2wnh_raw <- read_excel("Data/h02wnh.xlsx", skip = 5)

h2wnh_raw

h2_white_nh <- h2wnh_raw %>%
  rename(year = 1, number = 2) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(number)) %>%
  mutate(
    race = "White, Not Hispanic",
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_share",
    values_transform = list(income_share = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%"),
    number = as.integer(number)
  )

h2_white_nh

# h2 combo ----------------------------------------------------------------

all_h2_comb <- bind_rows(
  h2_all,
  h2_asian,
  h2_black,
  h2_hispanic,
  h2_white,
  h2_white_nh
) %>%
  mutate(
    number = as.integer(number),
    number = number * 1000
  )

all_h2_comb

all_h2_comb %>% 
  write_csv("Data/income_aggregate.csv")

## h3 ----

# H3a----------------------------------------------------------------------

h03a_raw <- read_excel("Data/h03a.xlsx", skip = 5)

h3_asian <- h03a_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race = rep(c("Asian Alone or in Combination", "Asian Alone"), each = 36),
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 36),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_asian


# h3ar --------------------------------------------------------------------

h03ar_raw <- read_excel("Data/h03ar.xlsx", skip = 4)

h03ar_raw

h3_all <- h03ar_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race = "All Races",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 53),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_all



# h3b ---------------------------------------------------------------------

h03b_raw <- read_excel("Data/h03b.xlsx", skip = 5)

h3_black <- h03b_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race =
      rep(
        c(
          rep("Black Alone or in Combination", 18),
          rep("Black Alone", 53)
        ),
        times = 2
      ),
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 71),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_black

########## START HERE ###########

# h3h ---------------------------------------------------------------------

h03h_raw <- read_excel("Data/h03h.xlsx", skip = 4)

h03h_raw

h3_hispanic <- h03h_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race = "Hispanic",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 48),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_hispanic


# h1w ---------------------------------------------------------------------

h03w_raw <- read_excel("Data/h03w.xlsx", skip = 5)

h03w_raw

h3_white <- h03w_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race = "White Alone",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 53),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_white


# h3wnh -------------------------------------------------------------------

h03wnh_raw <- read_excel("Data/h03wnh.xlsx", skip = 5)

h03wnh_raw

h3_white_nh <- h03wnh_raw %>%
  rename(year = 1) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(year = str_sub(year, 1, 4), year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Lowest\nfifth`)) %>%
  mutate(
    race = "White, Not Hispanic",
    dollar_type = rep(c("Current Dollars", "2019 Dollars"), each = 48),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = 4:last_col(),
    names_to = "income_quintile",
    names_pattern = "^(.*?)[\n]",
    values_to = "income_dollars",
    values_transform = list(income_dollars = as.double)
  ) %>%
  mutate(
    income_quintile = str_replace(income_quintile, "5", "5%")
  )

h3_white_nh

# h1 combo ----------------------------------------------------------------

all_h3_comb <- bind_rows(
  h3_all,
  h3_asian,
  h3_black,
  h3_hispanic,
  h3_white,
  h3_white_nh
)

all_h3_comb 

all_h3_comb %>% 
  count(year, sort = TRUE)

all_h3_comb %>% 
  write_csv("Data/income_mean.csv")


################################# GRAPH 2 ##############



graph2 <- all_h3_comb %>%
  filter(income_quintile != "Top 5%") %>%
  mutate(income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))) %>%
  filter(dollar_type == "2019 Dollars") %>%
  filter(race %in% c("White Alone", "Black Alone", "Hispanic")) %>%
  ggplot(aes(x = year, y = income_dollars, color = race, fill = race, group = race)) +
  geom_line() +
  facet_wrap(~income_quintile, scales = "free_y")

graph2


######### MODIFIED GRAPH 2 #######################



graph2a <- all_h3_comb %>%
  filter(income_quintile != "Top 5%") %>%
  mutate(income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))) %>%
  filter(dollar_type == "2019 Dollars") %>%
  filter(race %in% c("White Alone", "Black Alone", "Hispanic")) %>%
  ggplot(aes(x = year, y = income_dollars, color = race, fill = race, group = race)) +
  geom_point(size = 0.15) +
  geom_smooth(method = lm) +
  geom_line() +
  facet_wrap(~income_quintile, scales = "free_y") +
  theme(strip.text = element_text(color = "white",
                                  face = "bold",
                                  size = 14),
        strip.background = element_rect(fill = "grey50")) +
  labs(title = "Change in Family Income by Earning Brackets") +
  xlab("Year") +
  ylab("Family Income (Dollars)")


graph2a




############################# FINAL EXAM PLOT ##############

library(ggthemes)

graph2c <- all_h3_comb %>%
  filter(income_quintile != "Top 5%") %>%
  mutate(income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))) %>%
  filter(dollar_type == "2019 Dollars") %>%
  filter(race %in% c("White Alone", "Black Alone", "Hispanic")) %>%
  ggplot(aes(x = year, y = income_dollars, color = race, fill = race, group = race)) +
  geom_point(size = 0.15) +
  geom_smooth(method = lm) +
  geom_line() +
  facet_wrap(~income_quintile, scales = "free_y") +
  theme(strip.text = element_text(color = "white",
                                  face = "bold",
                                  size = 14),
        strip.background = element_rect(fill = "grey50")) +
  labs(title = "Change in Family Income by Earning Brackets") +
  xlab("Year") +
  ylab("Family Income (Dollars)") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0),
        panel.grid.major.x = element_line(linetype = 4),
        panel.grid.major.y = element_line(linetype = 4))


graph2c


##### Histogram of Imcome Quantile Levels #######

graph2b <- all_h3_comb %>%
  filter(income_quintile != "Top 5%") %>%
  mutate(income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))) %>%
  filter(dollar_type == "2019 Dollars") %>%
  filter(race %in% c("White Alone", "Black Alone", "Hispanic")) %>%
  ggplot(aes(x = year, y = income_dollars, color = race, fill = race, group = race)) +
  geom_bar(stat= "identity", width = 0) +
  facet_wrap(~income_quintile, scales = "free_y") +
  theme(strip.text = element_text(color = "white",
                                  face = "bold",
                                  size = 14),
        strip.background = element_rect(fill = "grey50")) +
  labs(title = "Distribution of Family Income by Earning Brackets")



#### H17 ####


# H17 ---------------------------------------------------------------------

h17_raw <- read_excel("Data/h17.xlsx", skip = 5)

income_distribution <- h17_raw %>%
  rename(
    year = 1,
    number = 2,
    income_median = Estimate...13,
    income_mean = Estimate...15,
    income_med_moe = `Margin of error (±)...14`,
    income_mean_moe = `Margin of error (±)...16`
  ) %>%
  filter(str_detect(year, "2017 \\(|\\(38", negate = TRUE)) %>%
  mutate(
    year = str_sub(year, 1, 4),
    year = as.integer(year),
    number = as.integer(number)
  ) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(`Under $15,000`)) %>%
  select(-Total) %>%
  mutate(
    race = c(
      rep("All Races", 53),
      rep("White Alone", 53),
      rep("White Alone, Not Hispanic", 48),
      rep("Black Alone or in Combination", 18),
      rep("Black Alone", 53),
      rep("Asian Alone or in Combination", 18),
      rep("Asian Alone", 33),
      rep("Hispanic (Any Race)", 48)
    ),
    .after = "year"
  ) %>%
  pivot_longer(
    cols = `Under $15,000`:`$200,000 and over`,
    names_to = "income_bracket",
    values_to = "income_distribution",
    values_transform = list(income_distribution = as.double)
  ) %>%
  mutate(
    across(c(income_median:income_mean_moe), as.integer),
    number = number * 1000
  )

income_distribution 

income_distribution %>% 
  write_csv("Data/income_distribution.csv")

income_levels <- c(
  "Under $15,000",
  "$15,000 to $24,999",
  "$25,000 to $34,999",
  "$35,000 to $49,999",
  "$50,000 to $74,999",
  "$75,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 to $199,999",
  "$200,000 and over"
)



####################### GRAPH 3 #####################




graph3 <- income_distribution %>%
  filter(race %in% c("Black Alone", "White Alone", "Hispanic (Any Race)")) %>%
  filter(year >= 1980) %>%
  mutate(income_bracket = factor(income_bracket, levels = income_levels)) %>%
  ggplot(aes(x = year, y = income_distribution, color = race, fill = race)) +
  geom_col(position = "fill") +
  facet_wrap(~income_bracket)

graph3




# Nancys graph #

graph3b <- income_distribution %>%
  filter(race %in% c("Black Alone", "White Alone", "Hispanic (Any Race)")) %>%
  filter(year >= 1980) %>%
  ggplot(aes(x= year, y = income_mean, color= race, fill= race)) +
  geom_bar(stat= "identity", width = 0.2) + 
  facet_wrap(~race) +
  theme_stata()
theme(legend.position = "none",
      axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of Mean Income by Race") +
  xlab("Year") +
  ylab(" Mean Income")

# gganimate specific bits:
# transition_states(year, transition_length = 2, state_length = 1) +
# view_follow(fixed_x = TRUE)  +
# ease_aes('sine-in-out')


graph3b


# Testing geom density

graph3c <- income_distribution %>%
  filter(race %in% c("Black Alone", "White Alone", "Hispanic (Any Race)")) %>%
  filter(year >= 1980) %>%
  
  ggplot(aes(x= year, group = race, color = race, fill = income_mean)) +
  # geom_line(x = year, y = income_mean)
  geom_density(adjust = 1.5, alpha = 0.5) + 
  facet_wrap(~race) +
  theme_stata()
theme(legend.position = "none",
      axis.text.x = element_text(angle = 90)) +
  labs(title = "Distribution of Mean Income by Race") +
  xlab("Year") +
  ylab(" Mean Income")

# gganimate specific bits:
# transition_states(year, transition_length = 2, state_length = 1) +
# view_follow(fixed_x = TRUE)  +
# ease_aes('sine-in-out')


graph3c


