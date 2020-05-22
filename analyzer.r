library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(optparse)

option_list<- list(
    make_option(c("-s","--start"), type="character", default="2020.02.22",
                help="Date of the first data to consider with format YYYY.MM.DD")
    )

parser <- OptionParser(usage="%prog [options]", option_list = option_list)
cmd <- parse_args(parser, commandArgs(TRUE), positional_arguments=0)
starting_date <- as.Date(cmd$options$start,"%Y.%m.%d")

## TODO list:
## - Offset Date / relative
## - Add Recovery
## - Counts per Hospital bed

## Countries Populations and offsets
countries_df <- data.frame(country =
                               c(
                                   "Belgium",
                                   "China",
                                   "France",
                                   "Germany",
                                   "Italy",
                                   "Korea, South",
                                   "Japan",
                                   "Spain",
                                   "Sweden",
                                   "Switzerland",
                                   "Taiwan*",# Not working
                                   "United Kingdom",
                                   "US"
                               ),
                           population =
                               c(
                                   11.46 * 10 ** 6,
                                   1.44 * 10 ** 9,
                                   66.89 * 10 ** 6,
                                   82.79 * 10 ** 6,
                                   60.48 * 10 ** 6,
                                   51.47 * 10 ** 6,# Korea, South
                                   126.8 * 10 ** 6,# Japan
                                   46.66 * 10 **6,
                                   10.12 * 10 ** 6,
                                   8.57 * 10 ** 6,  #Switerland
                                   23.78 * 10 ** 6, #Taiwan
                                   66.65 * 10 ** 6,  #UK
                                   331.0 * 10 ** 6
                               ),
                           ## offset =
                           ##     c(
                           ##         0,
                           ##         7,
                           ##         7,
                           ##         0,
                           ##         0, # Korea, South -> offset inappropriated
                           ##         0, # Japan -> offset inappropriated
                           ##         6,
                           ##         7,
                           ##         5, # Switerland
                           ##         5,
                           ##         6
                           ##     )
                           offset =
                               c(
                                   0,
                                   0,
                                   0,
                                   0,
                                   0,
                                   0, # Korea, South
                                   0, # Japan
                                   0,
                                   0,
                                   0, # Switzerland
                                   0,
                                   0, # UK
                                   0
                               )
                           )

countries <- c(
    ## "Belgium",
    ## "China",
    "France",
    ## "Italy",
    ## "Japan",
    ## "Korea, South",
    "Switzerland"
    ## "Germany",
    ## "Sweden",
    ## "Spain",
    ## "Taiwan*",
    ## "United Kingdom",
    ## "US"
)

filter_data <- function(input, selected_countries, starting_date)
{
    head(input)
    names(input)
    df <- gather(input, day, count, -Province.State, -Country.Region, -Lat, -Long)
    # Selecting only relevant countries
    df <- df[which(df$Country.Region %in% selected_countries),]
    # Cleaning up 'day' column
    df$day <- as.Date(str_replace(df$day,"X",""),"%m.%d.%y")
    # Summing everything
    df <- aggregate(count ~ Country.Region * day, df, sum)
    df <- df %>% arrange(Country.Region, day)
    # Adding the day_count column
    df <- df %>% group_by(Country.Region) %>% mutate(day_count = count - lag(count))
    print(df)
    # Using only latest data
    df <- df[which(df$day >= starting_date),]
    print(df)
    df
}


### CASES
cases_data <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
names(cases_data)
cases_data <- filter_data(cases_data, countries, starting_date)
cases_data$type <- "Cas confirmés"
### DEATHS
deaths_data <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths_data <- filter_data(deaths_data, countries, starting_date)
deaths_data$type <- "Morts"

## df <- full_join(cases_data,deaths_data)
df <- rbind(cases_data,deaths_data)
## df <- deaths_data

### Use only cases in
df <- df[which(df$count > 2),]


# country_indices
country_indices <- match(df$Country.Region, countries_df$country)
df$cpm <- df$count / countries_df[country_indices,"population"] * 10 **6
df$dcpm <- df$day_count / countries_df[country_indices,"population"] * 10 **6
# Counting days since start
df$elapsed <- as.numeric(df$day - starting_date)
# Offset
offsets <- countries_df[country_indices,"offset"]
df$days_adjusted <- df$elapsed - offsets
df$Pays <- sprintf("%s (J-%d)", df$Country.Region, offsets)
head(df,10)


g <- ggplot(df, aes(x=days_adjusted, y=day_count, color=Pays, linetype=type))
## g <- ggplot(df, aes(x=days_adjusted, y=count, color=exhibit_name, linetype=type))
g <- g + geom_line()
## g <- g + scale_y_log10()
g <- g + xlab(sprintf("Jours depuis le %s", strftime(starting_date,"%d %B %Y")))
g <- g + ylab("Occurences par million")
ggsave("test.png",width = 8, height= 6)
