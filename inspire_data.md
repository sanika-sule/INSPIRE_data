inspire_data
================
Sanika Sule
2024-05-15

.

``` r
enrollement =
  read_csv("data/enrollement.csv") |>
  janitor::clean_names() |>
  select(date, count, honor, masters, professional, verified)
```

    ## Rows: 1037 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): course_id
    ## dbl  (8): audit, count, credit, cumulative_count, honor, masters, profession...
    ## dttm (1): created
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
enrollement_location = 
  read_csv("data/enrollement_location.csv") |>
  janitor::clean_names() |>
  select(date, count, country_name, country_alpha3) 
```

    ## Rows: 152 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): country.alpha2, country.alpha3, country.name, course_id
    ## dbl  (1): count
    ## dttm (1): created
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
enrollement_education = 
  read_csv("data/enrollement_education.csv") |>
  janitor::clean_names()|>
  select(date, count, education_level)
```

    ## Rows: 10 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): course_id, education_level
    ## dbl  (1): count
    ## dttm (1): created
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
engagement_activity = 
  read_csv("data/engagement_activity.csv") |>
  janitor::clean_names()|>
  mutate(total_time = as.Date(interval_end) - as.Date(interval_start))|>
  mutate(date = as.Date(created)) |>
  select(date, interval_start, interval_end, total_time, played_video, posted_forum, attempted_problem, any )
```

    ## Rows: 131 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): course_id
    ## dbl  (4): any, attempted_problem, played_video, posted_forum
    ## dttm (3): created, interval_end, interval_start
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
long_eng = engagement_activity |>
  pivot_longer(cols = c("played_video", "posted_forum", "attempted_problem", "any"), 
               names_to = "activity_type", 
               values_to = "count")

ggplot(long_eng, aes(x = activity_type, y = count, fill = activity_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Engagement Activities", 
       x = "Activity Type", 
       y = "Count")
```

    ## Warning: Removed 126 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](inspire_data_files/figure-gfm/eng_act_plot-1.png)<!-- -->

``` r
yearly_summary = engagement_activity |>
  mutate(year = lubridate::year(as.Date(interval_start))) |>
  group_by(year) |>
  summarise(
    total_played_video = sum(played_video, na.rm = TRUE),
    total_posted_forum = sum(posted_forum, na.rm = TRUE),
    total_attempted_problem = sum(attempted_problem, na.rm = TRUE),
    total_any = sum(any, na.rm = TRUE),
    total_all_activities = sum(played_video, posted_forum, attempted_problem, any, na.rm = TRUE)
  )
```

``` r
# Activites total by year
ggplot(yearly_summary, aes(x = as.factor(year), y = total_all_activities, fill = as.factor(year))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Engagement Activities by Year", 
       x = "Year", 
       y = "Total Activities")
```

![](inspire_data_files/figure-gfm/activity_year-1.png)<!-- -->

``` r
eng_long_month = engagement_activity |>
  mutate(
    year = year(as.Date(interval_start)),
    month = month(as.Date(interval_start), label = TRUE)  
  ) |>
  pivot_longer(
    cols = c("played_video", "posted_forum", "attempted_problem", "any"),
    names_to = "activity_type",
    values_to = "count"
  )
```

``` r
# Engagement by month, paneled by year
ggplot(eng_long_month, aes(x = month, y = count, fill = activity_type)) +
  geom_col(position = position_dodge()) +  
  facet_wrap(~ year, scales = "free_x") +  
  labs(
    title = "Monthly Engagement Activities by Year",
    x = "Month",
    y = "Count of Activities",
    fill = "Activity Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
```

    ## Warning: Removed 126 rows containing missing values or values outside the scale range
    ## (`geom_col()`).

![](inspire_data_files/figure-gfm/activity_month-1.png)<!-- -->

``` r
education_summary = enrollement_education |>
  group_by(education_level) |>
  summarise(total_count = sum(count, na.rm = TRUE))
kable(education_summary)
```

| education_level  | total_count |
|:-----------------|------------:|
| associates       |          43 |
| bachelors        |         489 |
| doctorate        |          66 |
| junior_secondary |          18 |
| masters          |         528 |
| none             |           1 |
| other            |          21 |
| primary          |           0 |
| secondary        |         120 |
| NA               |         388 |

``` r
geo_summary = enrollement_location |>
  group_by(country_name) |>
  summarise(count = sum(count, na.rm = TRUE))
kable(geo_summary)
```

| country_name                           | count |
|:---------------------------------------|------:|
| Afghanistan                            |     1 |
| Albania                                |     1 |
| Algeria                                |     1 |
| Argentina                              |     7 |
| Armenia                                |     4 |
| Australia                              |    25 |
| Austria                                |     4 |
| Azerbaijan                             |     3 |
| Bahamas                                |     0 |
| Bangladesh                             |    23 |
| Belgium                                |     3 |
| Belize                                 |     1 |
| Benin                                  |     2 |
| Bermuda                                |     1 |
| Bhutan                                 |     2 |
| Bolivia                                |     5 |
| Bonaire, Sint Eustatius and Saba       |     1 |
| Bosnia and Herzegovina                 |     1 |
| Botswana                               |     8 |
| Brazil                                 |    40 |
| Brunei                                 |     1 |
| Bulgaria                               |     1 |
| Burkina Faso                           |     4 |
| Cambodia                               |     9 |
| Cameroon                               |     7 |
| Canada                                 |    50 |
| Chad                                   |     1 |
| Chile                                  |     4 |
| China                                  |     8 |
| Colombia                               |    21 |
| Congo                                  |     4 |
| Congo (the Democratic Republic of the) |     1 |
| Costa Rica                             |     1 |
| Cyprus                                 |     3 |
| Czechia                                |     1 |
| Côte d’Ivoire                          |     1 |
| Denmark                                |     3 |
| Dominican Republic                     |     2 |
| Ecuador                                |    32 |
| Egypt                                  |     9 |
| El Salvador                            |     7 |
| Equatorial Guinea                      |     1 |
| Eswatini                               |    11 |
| Ethiopia                               |    21 |
| Fiji                                   |     2 |
| Finland                                |     1 |
| France                                 |    21 |
| Gambia                                 |     1 |
| Georgia                                |     2 |
| Germany                                |    24 |
| Ghana                                  |     8 |
| Greece                                 |    72 |
| Guam                                   |     1 |
| Guatemala                              |    12 |
| Guyana                                 |     1 |
| Haiti                                  |     5 |
| Honduras                               |     2 |
| Hong Kong                              |     3 |
| Iceland                                |     1 |
| India                                  |    80 |
| Indonesia                              |    30 |
| Iran                                   |     1 |
| Iraq                                   |     9 |
| Ireland                                |     4 |
| Isle of Man                            |     1 |
| Israel                                 |     3 |
| Italy                                  |    13 |
| Japan                                  |    11 |
| Jordan                                 |     5 |
| Kazakhstan                             |     2 |
| Kenya                                  |    63 |
| Kuwait                                 |     3 |
| Kyrgyzstan                             |     3 |
| Lebanon                                |     6 |
| Liberia                                |     2 |
| Libya                                  |     1 |
| Lithuania                              |     1 |
| Luxembourg                             |     3 |
| Macao                                  |     1 |
| Madagascar                             |     2 |
| Malawi                                 |     5 |
| Malaysia                               |     5 |
| Mali                                   |     1 |
| Malta                                  |     1 |
| Mauritius                              |     3 |
| Mexico                                 |    26 |
| Mongolia                               |     3 |
| Morocco                                |     1 |
| Mozambique                             |     6 |
| Myanmar                                |    12 |
| Namibia                                |     3 |
| Nepal                                  |     6 |
| Netherlands                            |    15 |
| New Zealand                            |     7 |
| Nicaragua                              |     1 |
| Niger                                  |     3 |
| Nigeria                                |    67 |
| North Macedonia                        |     1 |
| Norway                                 |     5 |
| Oman                                   |     0 |
| Pakistan                               |    25 |
| Palestine, State of                    |     1 |
| Panama                                 |     2 |
| Papua New Guinea                       |     2 |
| Paraguay                               |     0 |
| Peru                                   |    12 |
| Philippines                            |    12 |
| Poland                                 |     4 |
| Portugal                               |    11 |
| Puerto Rico                            |     1 |
| Qatar                                  |     3 |
| Romania                                |     3 |
| Russia                                 |     4 |
| Rwanda                                 |    13 |
| Saudi Arabia                           |     3 |
| Senegal                                |     2 |
| Serbia                                 |     2 |
| Seychelles                             |     1 |
| Sierra Leone                           |     4 |
| Singapore                              |    10 |
| Sint Maarten (Dutch part)              |     1 |
| Somalia                                |    13 |
| South Africa                           |    33 |
| South Korea                            |     6 |
| South Sudan                            |     1 |
| Spain                                  |    31 |
| Sri Lanka                              |     5 |
| Sudan                                  |    10 |
| Sweden                                 |     3 |
| Switzerland                            |    16 |
| Syria                                  |     5 |
| Taiwan                                 |     1 |
| Tanzania                               |    25 |
| Thailand                               |    14 |
| Togo                                   |     1 |
| Trinidad and Tobago                    |     2 |
| Tunisia                                |     3 |
| Türkiye                                |    28 |
| UNKNOWN                                |     1 |
| Uganda                                 |    53 |
| Ukraine                                |     2 |
| United Arab Emirates                   |     7 |
| United Kingdom                         |    55 |
| United States of America               |   271 |
| Uruguay                                |     2 |
| Uzbekistan                             |     1 |
| Venezuela                              |     3 |
| Vietnam                                |     4 |
| Virgin Islands (British)               |     1 |
| Yemen                                  |     1 |
| Zambia                                 |    23 |
| Zimbabwe                               |    30 |

``` r
geo_order = enrollement_location |>
  arrange(desc(count)) |>
  slice_head(n = 10)
# location plot
ggplot(geo_order, aes(x = reorder(country_name, -count), y = count, fill = count)) +
  geom_bar(stat = "identity") + 
  labs(title = "Enrollment by Country (top 10)", x = "Country", y = "Enrollment Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
```

![](inspire_data_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
pathfinding_countries = c("Armenia", "Botswana", "Brazil", "Burkina Faso", "Cambodia", "Canada", 
                           "Colombia", "Côte D’Ivoire", "El Salvador", "Ethiopia", "Finland", "France", 
                           "Georgia", "Guinea", "Honduras", "Indonesia", "Jamaica", "Japan", "Jordan", 
                           "Kenya", "Mexico", "Mongolia", "Montenegro", "Namibia", "Nigeria", 
                           "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Romania", "South Africa", 
                           "Sri Lanka", "Sweden", "Tanzania", "Uganda", "United Arab Emirates", "Zambia", "Zimbabwe")


pathfinding_data = enrollement_location |>
  filter(country_name %in% pathfinding_countries)
```

``` r
pathfinding_order <- pathfinding_data |>
  arrange(desc(count))
# Pathfinding plot
ggplot(pathfinding_order, aes(x = reorder(country_name, -count), y = count, fill = count)) +
  geom_bar(stat = "identity") + 
  labs(title = "Enrollment by Pathfinding Country", x = "Country", y = "Enrollment Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](inspire_data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Pathfinding table
kable(pathfinding_order)
```

| date       | count | country_name         | country_alpha3 |
|:-----------|------:|:---------------------|:---------------|
| 2024-04-18 |    67 | Nigeria              | NGA            |
| 2024-04-18 |    63 | Kenya                | KEN            |
| 2024-04-18 |    53 | Uganda               | UGA            |
| 2024-04-18 |    50 | Canada               | CAN            |
| 2024-04-18 |    40 | Brazil               | BRA            |
| 2024-04-18 |    33 | South Africa         | ZAF            |
| 2024-04-18 |    30 | Indonesia            | IDN            |
| 2024-04-18 |    30 | Zimbabwe             | ZWE            |
| 2024-04-18 |    26 | Mexico               | MEX            |
| 2024-04-18 |    25 | Tanzania             | TZA            |
| 2024-04-18 |    23 | Zambia               | ZMB            |
| 2024-04-18 |    21 | Colombia             | COL            |
| 2024-04-18 |    21 | Ethiopia             | ETH            |
| 2024-04-18 |    21 | France               | FRA            |
| 2024-04-18 |    12 | Peru                 | PER            |
| 2024-04-18 |    12 | Philippines          | PHL            |
| 2024-04-18 |    11 | Japan                | JPN            |
| 2024-04-18 |     9 | Cambodia             | KHM            |
| 2024-04-18 |     8 | Botswana             | BWA            |
| 2024-04-18 |     7 | United Arab Emirates | ARE            |
| 2024-04-18 |     7 | El Salvador          | SLV            |
| 2024-04-18 |     5 | Jordan               | JOR            |
| 2024-04-18 |     5 | Sri Lanka            | LKA            |
| 2024-04-18 |     4 | Armenia              | ARM            |
| 2024-04-18 |     4 | Burkina Faso         | BFA            |
| 2024-04-18 |     3 | Mongolia             | MNG            |
| 2024-04-18 |     3 | Namibia              | NAM            |
| 2024-04-18 |     3 | Romania              | ROU            |
| 2024-04-18 |     3 | Sweden               | SWE            |
| 2024-04-18 |     2 | Georgia              | GEO            |
| 2024-04-18 |     2 | Honduras             | HND            |
| 2024-04-18 |     2 | Papua New Guinea     | PNG            |
| 2024-04-18 |     1 | Finland              | FIN            |
| 2024-04-18 |     0 | Paraguay             | PRY            |

``` r
non_pathfinding_data = enrollement_location %>%
  filter(!country_name %in% pathfinding_countries)

# Summarize the enrollment counts for each group
summary_pathfinding <- pathfinding_data %>%
  summarize(total_enrollment = sum(count), 
            average_enrollment = mean(count), 
            median_enrollment = median(count))

summary_non_pathfinding <- non_pathfinding_data %>%
  summarize(total_enrollment = sum(count), 
            average_enrollment = mean(count), 
            median_enrollment = median(count))

# Combine summaries into a single data frame
summary_comparison <- rbind(
  c("Pathfinding Countries", summary_pathfinding$total_enrollment, summary_pathfinding$average_enrollment, summary_pathfinding$median_enrollment),
  c("Non-Pathfinding Countries", summary_non_pathfinding$total_enrollment, summary_non_pathfinding$average_enrollment, summary_non_pathfinding$median_enrollment)
)

# Convert to data frame and set column names
summary_comparison <- as.data.frame(summary_comparison)
colnames(summary_comparison) <- c("Group", "Total Enrollment", "Average Enrollment", "Median Enrollment")

# Print the summary table using kable
kable(summary_comparison)
```

| Group                     | Total Enrollment | Average Enrollment | Median Enrollment |
|:--------------------------|:-----------------|:-------------------|:------------------|
| Pathfinding Countries     | 606              | 17.8235294117647   | 10                |
| Non-Pathfinding Countries | 1068             | 9.05084745762712   | 3                 |

``` r
# Plot the comparison
comparison_data <- rbind(
  data.frame(Group = "Pathfinding", Enrollment = pathfinding_data$count),
  data.frame(Group = "Non-Pathfinding", Enrollment = non_pathfinding_data$count)
)

ggplot(summary_comparison, aes(x = Group)) +
  geom_bar(aes(y = as.numeric(`Total Enrollment`)), stat = "identity", fill = "steelblue") +
  geom_text(aes(y = as.numeric(`Total Enrollment`), label = round(as.numeric(`Total Enrollment`), 1)), vjust = -0.5) +
  labs(title = "Total Enrollment Comparison: Pathfinding vs Non-Pathfinding Countries", x = "Group", y = "Total Enrollment") +
  theme_minimal()
```

![](inspire_data_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
