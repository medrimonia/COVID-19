library(ggplot2)
library(stringr)
library(tidyr)

## TODO list:
## - Offset Date / relative
## - Add Recovery

## Countries Populations
countries_df <- data.frame(country =
                               c(
                                   "France",
                                   "Germany",
                                   "Italy",
                                   "Japan",
                                   "Spain",
                                   "Sweden",
                                   "Switzerland"
                               ),
                           population =
                               c(
                                   66.89 * 10 ** 6,
                                   82.79 * 10 ** 6,
                                   60.48 * 10 ** 6,
                                   126.8 * 10 ** 6,
                                   46.66 * 10 **6,
                                   10.12 * 10 ** 6,
                                   8.57 * 10 ** 6
                               )
                           )

countries <- c("France", "Italy", "Japan", "Switzerland", "Germany")
starting_date <- as.Date("02.20.20","%m.%d.%y")


filter_data <- function(input, selected_countries, starting_date)
{
    head(input)
    names(input)
    df <- gather(input, day, count, -Province.State, -Country.Region, -Lat, -Long)
    # Selecting only relevant countries
    df <- df[which(df$Country.Region %in% selected_countries),]
    # Cleaning up 'day' column
    df$day <- as.Date(str_replace(df$day,"X",""),"%m.%d.%y")
    # Using only latest data
    df <- df[which(df$day >= starting_date),]
    df <- aggregate(count ~ Country.Region * day, df, sum)
    print(head(df))
    df
}


### CASES
cases_data <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
names(cases_data)
cases_data <- filter_data(cases_data, countries, starting_date)
cases_data$type <- "Confirmed cases"
### DEATHS
deaths_data <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
deaths_data <- filter_data(deaths_data, countries, starting_date)
deaths_data$type <- "Deaths"

## df <- full_join(cases_data,deaths_data)
df <- rbind(cases_data,deaths_data)
head(df)


# In progress: testing data
country_indices <- match(df$Country.Region, countries_df$country)
df$cpm <- df$count / countries_df[country_indices,"population"] * 10 **6
head(df)


g <- ggplot(df, aes(x=day, y=cpm, color=Country.Region, linetype=type))
g <- g + geom_line()
g <- g + scale_y_log10()
ggsave("test.png",width = 6, height=8)
