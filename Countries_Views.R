library(ggplot2)
library(dplyr)
library(readr)

# Reading the csv file for the hours watched column
hours_watched_raw <- read_csv("C:/Users/hanni/Downloads/What_We_Watched_A_Netflix_Engagement_Report_2025Jan-Jun(Shows).csv", skip = 5)

# Get top 20 by hours viewed
top_20_hours <- hours_watched_raw %>% 
  select(Title, `Hours Viewed`, Views) %>%
  arrange(desc(`Hours Viewed`)) %>%
  head(20)

# Reading the csv file for the shows and country names
countries_raw <- read_csv("C:/Users/hanni/Downloads/netflix_titles.csv.csv")

countries_clean <- countries_raw %>% 
  select(title, country)

# Create manual matches
title_matches <- data.frame(
  Hours_Title = c(
    "Squid Game: Season 2",
    "When Life Gives You Tangerines",
    "Adolescence: Limited Series",
    "Ginny & Georgia: Season 3",
    "The Night Agent: Season 2",
    "Squid Game: Season 3 ",
    "Until You Burn: Season 1",
    "Squid Game: Season 1",
    "Zero Day: Limited Series",
    "You: Season 5",
    "The Residence: Season 1",
    "Valle Salvaje: Season 1",
    "Sirens: Limited Series",
    "The Trauma Code: Heroes on Call: Season ",
    "American Primeval: Limited Series",
    "Ransom Canyon: Season 1",
    "Love Is Blind: Season 8",
    "Missing You: Limited Series",
    "The Night Agent: Season 1",
    "Dept. Q: Season 1"
  ),
  Country_Title = c(
    "Squid Game",
    "When Life Gives You Tangerines",
    "Adolescence",
    "Ginny & Georgia",
    "The Night Agent",
    "Squid Game",
    "Until You Burn",
    "Squid Game",
    "Zero Day",
    "You",
    "The Residence",
    "Valle Salvaje",
    "Sirens",
    "The Trauma Code",
    "American Primeval",
    "Ransom Canyon",
    "Love Is Blind",
    "Missing You",
    "The Night Agent",
    "Dept. Q"
  )
)

# Join top 20 with manual matches
top_20_matched <- top_20_hours %>%
  left_join(title_matches, by = c("Title" = "Hours_Title"))

# Then join with countries data
top_20_with_countries <- top_20_matched %>%
  left_join(countries_clean, by = c("Country_Title" = "title")) %>%
  select(Title, `Hours Viewed`, Views, country)

# PASTE METHOD 3 HERE - Replace all the country values
top_20_with_countries$country <- c(
  "South Korea",      # Row 1
  "South Korea",      # Row 2
  "Brazil",           # Row 3
  "United States",    # Row 4
  "United States",    # Row 5
  "South Korea",      # Row 6
  "Colombia",         # Row 7
  "South Korea",      # Row 8
  "United States",    # Row 9
  "United States",    # Row 10
  "United States",    # Row 11
  "Spain",            # Row 12
  "Turkey",           # Row 13
  "South Korea",      # Row 14
  "United States",    # Row 15
  "United States",    # Row 16
  "United States",    # Row 17
  "United Kingdom",   # Row 18
  "United States",    # Row 19
  "Denmark"           # Row 20
)

# Remove weird symbols from titles
top_20_with_countries$Title <- gsub(" //.*", "", top_20_with_countries$Title)



View(top_20_with_countries)


country_hours <- top_20_with_countries %>%
  group_by(country) %>%
  summarise(Total_Hours = sum(`Hours Viewed`))

ggplot(country_hours, aes(x = reorder(country, Total_Hours), y = Total_Hours)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Hours Viewed by Country (Top 20 Shows)",
       x = "Country",
       y = "Total Hours Viewed") +
  theme_minimal()




