# Bristow Richards and Sara Khoshhal
# 12/1/2022
# Processing data from SmellPGH

## Load Libraries
library(tidyverse)
library(knitr)
library(scales)
library(httr)
library(jsonlite)
library(lubridate)
library(sf)

# create query function
smell_query <- function() {
  ## API call
  df <- GET(
    'https://api.smellpittsburgh.org/api/v2/smell_reports?city_ids=1&timezone_string=America%2FNew_York'
  ) %>%
    # get content, convert to character and finally to tabular from JSON
    .$content %>%
    rawToChar() %>% 
    fromJSON() %>%
    tibble() %>%
    # remove string columns
    select(
      -c(smell_description, feelings_symptoms, additional_comments)
    ) %>%
    # add various date columns for ease of use
    mutate(
      datetime = as_datetime(observed_at),
      date = date(datetime),
      month = month(date),
      year = year(date),
      smell_category = case_when(
        smell_value %in% c(1,2) ~ 'Low',
        smell_value == 3 ~ 'Medium',
        TRUE ~ 'High'
      )
    )
  
  ## Return data
  return(df)
}

# run the query unless the data already exists
ifelse(
  file.exists('Data/smell_data.csv'),
  { # if exists
    df <- read_csv(
      'Data/smell_data.csv',
      show_col_types = FALSE)
    },
  { # if not exists
    df <- smell_query();
    write_csv(df, 'Data/smell_data.csv')
  }
)

# plots
df %>%
  group_by(year, smell_value) %>%
  summarize(
    n = n()
  ) %>%
  ggplot(data=.,
         aes(x=year,
             y=n,
             fill=smell_value)) +
  geom_bar(stat='identity',
           position='dodge')

df %>%
  group_by(zipcode) %>%
  summarize(
    percent_smell_45 = label_percent()(sum(smell_value %in% c(4,5)) / n()),
    report_count = n()
  ) %>% 
  filter(report_count >= 100) %>%
  arrange(desc(percent_smell_45)) %>%
  head(10) %>%
  kable()


df %>%
  group_by(zipcode) %>%
  summarize(
    percent_smell_45 = label_percent()(sum(smell_value %in% c(4,5)) / n()),
    report_count = n()
  ) %>% 
  filter(report_count >= 100) %>%
  arrange(desc(report_count)) %>%
  head(10) %>%
  kable()

zipcodes_high_freq <- df$zipcode %>% 
  as.factor() %>% 
  summary() %>% 
  sort(decreasing=TRUE) %>%
  .[1:5] %>%
  names() %>%
  as.integer()

df %>% 
  group_by(zipcode) %>%
  summarize(
    percent_smell_45 = sum(smell_value %in% c(4,5)) / n(),
    report_count = n(),
    label = ifelse(
      zipcode %in% zipcodes_high_freq, 
      zipcode,
      NA
    )
  ) %>%
  ggplot(data=.,
         aes(x=report_count,
             y=percent_smell_45,
             label=label)
  ) +
  geom_point() #+
  ggrepel::geom_text_repel(size = 3, show.legend = FALSE) 

