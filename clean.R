# libraries
library(tidyverse)
library(data.table)
library(janitor)
library(chron)
library(lubridate)

# read data
evenness <- read_tsv("data/evenness.tsv")
faith <- read_tsv("data/faith_pd.tsv")
metadata <- read.delim("data/map_year1.txt")
relab <- read_tsv("data/relab6.tsv")
richness <- read_tsv("data/richness.tsv")

# transpose relab table
t_relab <- as.data.frame(t(relab) |>
  row_to_names(row_number = 1)) |>
  rownames_to_column("ID")

# join dataframes
df <- metadata |>
  left_join(evenness, join_by(SampleID == ...1)) |>
  left_join(richness, join_by(SampleID == ...1)) |>
  left_join(faith, join_by(SampleID == `#SampleID`)) |>
  left_join(t_relab, join_by(SampleID == ID))

# convert numeric time values to xx:xx format
df$time.converted <- sprintf("%02d:%02d",
                            df$Sampline.time %/% 100,
                            df$Sampline.time %% 100)

df <- df %>% 
  mutate(hour_measured = Sampline.time %/% 100) %>%
  mutate(minute_measured = Sampline.time %% 100) %>%
  mutate(minutes_after_6am = (hour_measured-6)*60 + minute_measured)

calculate_sequential_days <- function(date_strings) {
  dates <- lubridate::dmy(date_strings)
  sequential_days <- as.integer(difftime(dates, lubridate::dmy("4-May-20"), units = "days")) + 1
  return(sequential_days)
}

df <- df %>% 
  mutate(day_number = calculate_sequential_days(Sampling.date))

# write csv
write_csv(df, "./data/lemurs.csv")
