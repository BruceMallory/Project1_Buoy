library(tidyverse)
library(stringr)
library(rstanarm)
library(lubridate)
library(gridExtra)
library(kableExtra)
library(knitr)
library(png)

## Make URLs
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

## Specify the range of years and what months we want
## We have avoided the issue of two digit years by starting with 2000
years <- c(2000:2018)
months <- c(01:12)
months <-str_pad(months, 2, pad="0")

## Create character vectors for the names of our urls, data frames, months, plots, and slopes
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")
month_files <- str_c("M", months, sep = "")
plot_files <- str_c("P", months, sep = "")
slope_files <- str_c("S", months, sep = "")

## Set the number of repetitions for our loops below
N <- length(urls)
M <- 12

## This for loop is executed 12 times, one for each month of the year.
for (j in 1:M){

  ## This for loop is for creating files called M01, M02, ..., M012 and placing them in a data frame.
  for (i in 1:N){
    # Suppresses warnings
    suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE, na = "")))
    
    file <- get(filenames[i])
   
    
    # This is necessary because some years label their year as "YY" (i.e. 2007 is 07)
    colnames(file)[1] <-"YYYY"
    
    # For each year, we grab only the YYYY, MM, and ATMP columns and keep the data for each month in a separate file..
    file %<>%
      select(YYYY, MM, ATMP) %>%  #grab only the columns we are interested in
      transform(YYYY=as.numeric(YYYY), MM=as.numeric(MM), ATMP=as.numeric(ATMP)) %>%  #change from character to numbers
      filter(MM==j, ATMP<70) #grab each month separately and filter out NAs
    
    # This binds together mrYYYY files for each month, to be used in the next step
    if(i == 1){
      MR <- file
    }
    else{
      MR <- rbind.data.frame(MR, file)
    }
  }

  ## We group by year and summarize by ATMP and MM in order to create new columns
  ## new columns are average temperature and the month
  month <- MR %>%
    group_by(YYYY) %>% 
    summarize(mean(ATMP), median(MM))
  colnames(month)[2]<-"AvgTMP"
  colnames(month)[3] <- "MM"
  month <- month %>% mutate(Day=c(1)) %>% 
    mutate(Date=make_date(year=YYYY, month=MM, day=Day))
  
  assign(month_files[j], month)
  
  ## Use lubridate to create a new column that puts together the year, month, and day
  month <- month %>% mutate(Day=c(1)) %>% 
    mutate(Date=make_date(year=YYYY, month=MM, day=Day))
  
  ## Bind together the "month" files to create a new data frame ("M_all") which i

  if(j == 1){
    M_all <- month
  }
  else{
    M_all <- rbind.data.frame(M_all, month)
  }
}

# Fit a regression model for the combined data frame M_all
fit_all <- stan_glm(AvgTMP ~ Date, data = M_all, refresh=0)
print(coef(fit)[2], digits = 6) #look at slope of regression line

# Plot the data and fitted regression line
ggplot(M_all, aes(Date, AvgTMP)) + 
  geom_line() +
  geom_abline(intercept = coef(fit_all)[1], slope = coef(fit_all)[2], color = "blue") +
  geom_hline(aes(yintercept=mean(AvgTMP)), linetype="dotted")

# Over the course of our sample, the regression line is showing an increase in average
# temperature of 0.68 degrees.
coef(fit_all)[2]*max(as.numeric(M_all$Date)) - coef(fit_all)[2]*min(as.numeric(M_all$Date))

## Running separate regressions for each month
for (k in 1:12){
  file <- get(month_files[k])
  fit_month <- stan_glm(AvgTMP ~ YYYY, data = file, refresh=0)
  assign(slope_files[k], as_tibble(round(coef(fit_month)[2], digits = 3))) #gather slopes for each month
  slopes <- get(slope_files[k])
  
  # Combine slopes into a table
  if(k==1){
    df <- slopes
  }
  else{
    df <- rbind.data.frame(df, slopes)
  }
  
  # Create plots for each separate regression
  assign(plot_files[k], ggplot(file, aes(YYYY, AvgTMP)) + 
           geom_point() + 
           geom_abline(intercept = coef(fit_month)[1], slope = coef(fit_month)[2], color = "blue") +
           labs(x = "Year", y = "Average Temp", title = month.abb[k]) + 
           xlim(2000, 2018) + 
           ylim(15, 30))

}

# Arrage plots from above into a single figure
grid.arrange(P01, P02, P03, P04, P05, P06, P07, P08, P09, P10, P11, P12, nrow=3, ncol=4, newpage = TRUE)
# We tried - but failed in our attempt to condense the P01:P12 within grid.arrange :-(

# Manipulate table of slopes so that it is more readable in the report
df <- mutate(df, Month = month.abb[1:12])
colnames(df)[1] <- "Slope"
df <- relocate(df, Month)
df <- t(df)
rownames(df) <- c("Month", "Slope")
kable(df)

# Calculate the average slope to determine whether it is positive or negative 
mean(df$Slope)

# References
citation(package = "tidyverse")
citation(package = "stringr")
citation(package = "rstanarm")
citation(package = "lubridate")
citation(package = "gridExtra")
citation(package = "kableExtra")
citation(package = "knitr")
citation(package = "png")





### More helpful information
# In April and August 2018, there was no recorded data due to lack of funding.

## Practice manipulating columns and variables (not part of our analyses)
data_1997 <- read.table("https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h1997.txt.gz&dir=data/historical/stdmet/", header=T)
data_1997 <- as_tibble(data_1997)

# Use mutate to create a new column with a 4 digit year
data_1997 <- mutate(data_1997, YYYY = data_1997$YY + 1900)

# Use mutate and unite to create a new column with a 4 digit year
data_1997a <- mutate(data_1997, CC = 19)
data_1997a <- data_1997a %>% unite("YEAR", CC,YY, sep = "")


