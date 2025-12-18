library(ggplot2)
library(dplyr)
library(readr)
library(gt)
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

# Replace all the country values
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
top_20_with_countries$Title <- gsub(" //.*", "", top_20_with_countries$Title, useBytes = TRUE)



View(top_20_with_countries)

netflix_table <- country_hours %>%
  arrange(desc(Total_Hours)) %>%
  gt() %>%
  tab_header(
    title = md("**Total Hours Viewed by Country**"),
    subtitle = "Top 20 Netflix Shows"
  ) %>%
  cols_label(
    country = "Country",
    Total_Hours = "Total Hours Viewed"
  ) %>%
  fmt_number(
    columns = Total_Hours,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  data_color(
    columns = Total_Hours,
    colors = scales::col_numeric(
      palette = c("#141414", "#E50914"),
      domain = NULL
    )
  ) %>%
  tab_options(
    table.background.color = "#141414",
    heading.background.color = "#141414",
    heading.title.font.size = 24,
    heading.subtitle.font.size = 14,
    heading.align = "left",
    column_labels.background.color = "#E50914",
    column_labels.font.weight = "bold",
    column_labels.font.size = 14,
    table.font.color = "white",
    table.border.top.color = "#E50914",
    table.border.bottom.color = "#E50914",
    data_row.padding = px(8)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "white"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "#B3B3B3"),
    locations = cells_title(groups = "subtitle")
  )

# Display the table
netflix_table

# Save as PNG
gtsave(netflix_table, "netflix_table.png", vwidth = 800, vheight = 600)

# Save as PDF
gtsave(netflix_table, "netflix_table.pdf")




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




