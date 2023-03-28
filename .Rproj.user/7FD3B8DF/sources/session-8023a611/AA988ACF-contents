# Import packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(readr)
library(lubridate)
library(ggthemes)
library(extrafont)
library(caret)
fonts()


# upload dataset

kickstarter <- read_csv("kickstarter_projects.csv")

# check dataset

glimpse(kickstarter)
sum(is.na(kickstarter))
sum(duplicated(kickstarter))

# Manipulate data into new columns

kickstarter$time <- format(kickstarter$Launched, format = "%H:%M:%S")
kickstarter$launch_date <- format(kickstarter$Launched, format = "%m/%d/%y")
kickstarter$launch_month <- format(kickstarter$Launched, format = "%b")
kickstarter$launch_day <- format(kickstarter$Launched, format = "%A")
kickstarter$duration <- round(difftime(kickstarter$Deadline,kickstarter$Launched, units = "days"), 1)

# Select only completed projects (no "Suspended, "Live" or "Cancelled)
unique(kickstarter$State)
kickstarter_clean <- kickstarter %>%
  filter(State %in% c("Failed", "Successful"))

# Check Summary Data

kickstarter_clean %>%
  select(c("Goal", "Pledged", "Backers")) %>%
  summary()

# Success Rate by Category 

kickstarter_category <- kickstarter_clean %>%
  group_by(Category, State) %>%
  summarise(count = n()) %>%
  mutate(perc = round((count/sum(count))*100,2))%>%
  filter(State == "Successful")

kickstarter_category %>%
  ggplot(aes(x = Category, y = perc, fill = Category)) +
  geom_col() +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(family = "Rubik"), 
    axis.text.x  = element_text(
      family = "Rubrik", # improve reading of x axis
      angle = 90, 
      hjust = 1, 
      vjust = 0.5)) +
  labs(
    y     = "Success Rate (%)", 
    x     = "Category",
    title = "Successs Rates of Categories" # making clear labels and title
  )

kickstarter_category %>%
  ggplot(aes(x=count, y = perc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(family = "Rubik")) +
  labs(
    x     = "Count within Category", 
    y     = "Success Rate (%)",
    title = "Success with Count Rate in Categories")  


# Success rates by Country

kickstarter_clean %>%
  group_by(Country, State) %>%
  summarise(count = n()) %>%
  mutate(perc = round((count/sum(count))*100,2)) %>%
  filter(State == "Successful") %>%
  ggplot(aes(y = Country, x = perc, fill = Country)) +
  geom_col() +
  scale_x_continuous(expand = c(0,0)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(family = "Rubik")) +
  labs(
    x     = "Success Rate (%)", 
    y     = "Country",
    title = "Success Rates for Countries")  # making clear labels and title 


# Success Rates according to Goal Size

kickstarter_clean %>%
  mutate(bins = cut(Goal, breaks = seq(0,250000,by = 25000))) %>%
  group_by(bins, State) %>%
  summarise(count = n()) %>%
  mutate(perc = round((count/sum(count))*100,2)) %>%
  filter(State == "Successful") %>%
  ggplot(aes(x = factor(bins), y = perc)) +
  geom_col(position = "identity", fill = "darkblue") +
  scale_x_discrete(labels = c("0-25000","25000-50000","50000-75000", "75000-100000","100000-125000","125000-150000","150000-175000","175000-200000","200000-225000","225000-250000","250000 +")) +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(family = "Rubik"),
    axis.text.x  = element_text(
      family = "Rubik",
      angle = 90, 
      hjust = 1, 
      vjust = 0.5)) +
  labs(
    y     = "Average Success Rate (%)", 
    x     = "Goal Amounts ($)",
    title = "Success Rates for Goal Amounts" # making clear labels and title
  ) 

# Success Rate According to funding duration

kickstarter_clean %>%
  mutate(duration = as.numeric(duration), bins = cut(duration, breaks = seq(0,90, by = 15))) %>%
  group_by(bins, State) %>%
  summarise(count = n()) %>%
  mutate(perc = round((count/sum(count))*100,2)) %>%
  filter(State == "Successful") %>%
  ggplot(aes(x= bins, y = perc)) +
  geom_col(fill = 'darkblue') +
  scale_x_discrete(labels = c("0-15","15-30","30-45", "45-60","60-75","75-90","90+")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(family = "Rubik")) +
  labs(
    y     = "Average Success Rate (%)", 
    x     = "Project Funding Duration (days)",
    title = "Success Rates for Funding Durations" 
  )


# Predicitng Using Linear Regression Model

table(kickstarter_clean$Category)

glimpse(kickstarter_clean)
kickstarter_regression <- kickstarter_clean %>%
  mutate(duration = as.numeric(duration))
         
         
kickstarter_regression$Category <- as.factor(kickstarter_regression$Category)
kickstarter_regression$Subcategory <- as.factor(kickstarter_regression$Subcategory)
kickstarter_regression$Country <- as.factor(kickstarter_regression$Country)
kickstarter_regression$State <- as.factor(kickstarter_regression$State)

str(kickstarter_regression)

set.seed(1234)

training.samples <- kickstarter_regression$State %>%
  createDataPartition(p = 0.6, list = FALSE)

train.data <- kickstarter_regression[training.samples, ]
test.data <- kickstarter_regression[-training.samples, ]



lrm_model <- glm(State ~ Category + Goal + duration + Country, family = "binomial", data = train.data)
summary(success_model)

res <- predict(lrm_model,test.data, type = "response")
res
res <-predict(lrm_model,train.data, type = "response")
res

confmatrix <- table(Actual_Value = train.data$State, Predicted_Value = res >0.5)
confmatrix          

# Accuracy
(confmatrix[[1,1]] + confmatrix[[2,2]])/ sum(confmatrix)
                              
