## beer data clean up

library(tidyverse)
library(nlme)

bcl <- read.csv("bcl-data-beer.csv", stringsAsFactors = FALSE)


bcl_clean <- bcl %>% 
  mutate(Total_vol = Volume_Per_Container*Bottles_Per_Unit) %>% 
  mutate(Price_L = Price/Total_vol) %>% 
  mutate(Value_metric = Rating/Price_L)

Price_Ltemp <- bcl_clean$Price_L %>% 
  round(digits = 2)

bcl_clean <- bcl_clean %>% 
  mutate(Price_L = Price_Ltemp)

# write.csv(bcl_clean, file = "bcl-data-beer-clean.csv")

summary(bcl_clean)

ldb_clean_subset <- bcl_clean %>% 
  filter(!is.na(Rating)) %>% 
  select(Region, Country, Name, Style, Substyle, IBU, Rating, ABV, Price, Price_L, Value_metric)

# write.csv(ldb_clean_subset, file = "bcl-data-beer-clean-subset.csv")


price_rating <- ggplot(ldb_clean_subset, aes(x = Country, y = Value_metric)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 1/5)

price_rating

#price by country
price_country <- ggplot(ldb_clean_subset, aes(x = Country, y = Price_L)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 1/5)

price_country

#Price by region
price_region <- ggplot(bcl_clean, aes(x = Region, y = Price_L, fill = Region, color = Region, alpha = 0.2)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() + 
  ylab("Price per Liter (CAD)") +
  scale_fill_manual(values = c("#FF4081", "#5E35B1", "#2979FF", "#B9F6CA", "#AEEA00", "#F57C00", "#EA80FC")) +
  scale_color_manual(values = c("#FF4081", "#5E35B1", "#2962FF", "#1DE9B6", "#AEEA00", "#F57C00", "#E040FB")) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 1/2, show.legend = FALSE) +
  ggtitle("Beer prices in Canada by producing region")

price_region

# focus on prices for beer from major markets
majorMarket_subset <- bcl_clean %>% 
  filter(Region == "CANADA" | Region == "USA" | Region == "EUROPE")

price_majorMarkets <- ggplot(majorMarket_subset, aes(x = Region, y = Price_L, fill = Region, color = Region, alpha = 0.2)) + 
  scale_y_log10() + 
  ylab("Price per Liter (CAD)") +
  geom_boxplot(outlier.colour = "dark gray", show.legend = FALSE) +
  scale_fill_manual(values = c("#2979FF", "#B9F6CA", "#EA80FC")) +
  scale_color_manual(values = c("#2962FF", "#1DE9B6", "#E040FB")) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), alpha = 1/2, show.legend = FALSE) +
  ggtitle("Beer prices in Canada by producing region") 

price_majorMarkets

boxplot.stats(majorMarket_subset$Price_L[majorMarket_subset$Region == "CANADA"])
boxplot.stats(majorMarket_subset$Price_L[majorMarket_subset$Region == "EUROPE"])
boxplot.stats(majorMarket_subset$Price_L[majorMarket_subset$Region == "USA"])


model_mm <- aov(Price_L ~ Region, data = majorMarket_subset)
summary(model_mm)

# dot size changes with rating
p <- ggplot(majorMarket_subset, aes(x = Price_L, y = ABV)) + 
  geom_point(aes(size = Rating), pch = 21) + 
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "orange", midpoint = 3) +
  scale_size_continuous(range = c(0.1,8)) +
  scale_x_continuous(limits = c(0, 15)) +
  facet_grid(~Region) + 
  aes(alpha = 1/3, fill = Rating) +
  guides(size = FALSE) +
  guides(alpha = FALSE) +
  xlab("Beer price per liter (CAD/L)") +
  ylab("Alcohol content (%)") +
  ggtitle("High alcohol beers have higher prices")

p

# dot size changes with ABV
p2 <- ggplot(majorMarket_subset, aes(x = Price_L, y = Rating)) + 
  scale_x_continuous(limits = c(0, 15)) +
  geom_point(aes(size = ABV), pch = 21) + 
  scale_size_continuous(range = c(0.1,10)) + 
  facet_grid(~Region) + aes(alpha = 1/3, fill = ABV) +
  guides(size = FALSE) +
  guides(alpha = FALSE) +
  scale_fill_gradient2(low = "yellow", high = "blue", mid = "orange", midpoint = 4.5) +
  xlab("Beer price per liter (CAD/L)") +
  ylab("Rating out of 5") +
  ggtitle("Highly rated beers have higher prices")

p2



# overlapping histogram showing prices for beers from each region
majorMarket_subset <- majorMarket_subset %>% 
  filter(Price_L < 16)
p3 <- ggplot(majorMarket_subset, aes(x = Price_L)) +
  geom_histogram(data = subset(majorMarket_subset, Region == "CANADA"), fill = "#2962FF", alpha = 1/3, binwidth = 0.5) +
  geom_histogram(data = subset(majorMarket_subset, Region == "EUROPE"), fill = "#1DE9B6", alpha = 1/2, binwidth = 0.5) +
  geom_histogram(data = subset(majorMarket_subset, Region == "USA"), fill = "#E040FB", alpha = 1/2, binwidth = 0.5) +
  xlab("Beer price per liter (CAD/L)")

p3

# rating vs price lm
p4 <- ggplot(majorMarket_subset, aes(x = Price_L, y = Rating, color = Region, show.legend = FALSE)) +
  # scale_x_log10(limits = c(1,15)) +
  scale_x_continuous(limits = c(2,15)) +
  scale_y_continuous(limits = c(1.5,5)) +
  geom_smooth(show.legend = FALSE) +
  geom_point(alpha = 1/2, show.legend = FALSE) + 
  scale_color_manual(values = c("#2962FF", "#1DE9B6", "#E040FB")) +
  facet_wrap(~Region) +
  xlab("Beer price per liter (CAD/L)") +
  ylab("Rating out of 5") +
  ggtitle("Rating increases with price up to $6/Liter -- you get what you pay for, unless you pay more than $6")

p4

# Rating vs IBU
ldb_IBU <- ldb_clean_subset %>% 
  filter(!is.na(IBU))

p5 <- ggplot(ldb_IBU, aes(x = IBU, y = Rating, color = Style)) +
  geom_point() +
  geom_rug()

p5  


ldb_IBU <- ldb_clean_subset %>% 
  filter(!is.na(IBU)) %>% 
  filter(Style == "LAGER" | Style == "IPA" | Style == "DARK ALE" | Style == "PALE ALE" | Style == "WHEAT")
ldb_IBU$IBU <- as.numeric(ldb_IBU$IBU)


p6 <- ggplot(ldb_IBU, aes(x = IBU, y = Rating, color = Style)) +
  geom_point() +
  scale_x_log10() +
  geom_rug()

p6  

p6_2 <- ggplot(ldb_IBU, aes(x = IBU, y = Rating, color = Style)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(values = c("#FF4081", "#6200EA", "#00C853", "#F57C00", "#E040FB")) +
  geom_smooth(se = FALSE)
p6_2


lager_IPA_IBU <- ldb_IBU %>% 
  filter(Style == "LAGER" | Style == "IPA")
  
p7 <- ggplot(lager_IPA_IBU, aes(x = IBU, y = Rating, color = Style)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(values = c("#6200EA", "#00C853")) +
  geom_smooth(se = FALSE)

p7

IBU.lme <- lme(Rating ~ IBU, data = lager_IPA_IBU, random = ~1|Style)
anova(IBU.lme)


# heat map for beer value metric

ldb_heatmap_subset <- ldb_clean_subset %>% 
  filter(Region == "CANADA" | Region == "EUROPE" | Region == "USA") %>% 
  mutate(Value_metric_norm = Value_metric/max(Value_metric))

write.csv(ldb_heatmap_subset, file = "ldb_heatmap_subset.csv")
  
  
p8 <- ggplot(ldb_heatmap_subset, aes(x = Region, y = Style)) +
  geom_tile(aes(fill = Value_metric_norm), color = "white") +
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "blue", midpoint = 0.5) +
  #scale_fill_gradient2(low = "white", mid = "orange", high = "blue", midpoint = 0.5) +
  labs(fill = "Value") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold")) +
  ggtitle("Get the most bang for your buck!")

p8

summary(ldb_heatmap_subset)

# midpoint = mean(ldb_heatmap_subset$Value_metric_norm)

# size change with number of beers
#   stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +



####################################
# Economic data for beer in the US #
####################################

state <- read.csv("beer_by_state.csv", stringsAsFactors = FALSE)
str(state)

state$Employment_FTEs <-  gsub("[^0-9\\.]", "", state$Employment_FTEs) %>% 
  as.numeric()
state$Labor_income <- gsub("[^0-9\\.]", "", state$Labor_income) %>% 
  as.numeric()
state$Avg_wage <- gsub("[^0-9\\.]", "", state$Avg_wage) %>% 
  as.numeric()
state$Economic_impact <- gsub("[^0-9\\.]", "", state$Economic_impact) %>% 
  as.numeric()
state$Impact_per_21adult <- gsub("[^0-9\\.]", "", state$Impact_per_21adult) %>% 
  as.numeric()

library(usmap)
state <- state %>% 
  mutate(state = tolower(State))
write.csv(state, file = "state_growth.csv")

plot_usmap(data = state, values = "Economic_impact", lines = "#FFA726") +
  scale_fill_continuous(low = "#FFF3E0", high = "#EF6C00", name = "Economic Impact", label = scales::comma) +
  labs(title = "Direct & Indirect Economic Impact of Craft Brewers in 2017") +
  theme(legend.position = "right")
  

########################
# brewery growth in US #
########################

brew_growth <- read.csv("breweries_yr.csv")
str(brew_growth)

percent_growth <- function(x, y){
  growth <- (y-x)/x*100
  return(growth)
}

# past vector
past <- function(x){
  past_vec <- x[1:length(x)-1]
  return(past_vec)
}

# current vector
present <- function(x){
  pres_vec <- x[2:length(x)]
  return(pres_vec)
}

Regional_growth <- percent_growth(x = past(brew_growth$Regional_Breweries), y = present(brew_growth$Regional_Breweries))
Microbrew_growth <- percent_growth(x = past(brew_growth$Microbreweries), y = present(brew_growth$Microbreweries))
BrewPub_growth <- percent_growth(x = past(brew_growth$Brewpubs), y = present(brew_growth$Brewpubs))

Year <- brew_growth$Year[2:length(brew_growth$Year)]
percent_growth_df <- data.frame(Year, Regional_growth, Microbrew_growth, BrewPub_growth) %>% 
  rename(c("Regional_growth" = "Regional_Breweries", "Microbrew_growth" = "Microbreweries", "BrewPub_growth" = "Brewpubs"))


# growth_plot <- ggplot(percent_growth_df, aes(x = Year)) +
#   geom_line(aes(y = Regional_growth), color = "#2962FF", size = 1) +
#   geom_line(aes(y = Microbrew_growth), color = "#D500F9", size = 1) +
#   geom_line(aes(y = BrewPub_growth), color = "#1DE9B6", size = 1) +
#   ylab("Percent growth from previous year")
# 
# growth_plot

library(reshape)
melt.percent.df <- melt(percent_growth_df, id = "Year")

# write.csv(melt.percent.df, file = "brewers_percent.csv")

growth_plot3 <- ggplot(melt.percent.df, aes(x = Year, y = value, color = variable, fill = variable)) +
  geom_line() +
  scale_colour_manual(values=c("#2962FF", "#D500F9", "#1DE9B6")) +
  scale_fill_manual(values=c("#2962FF", "#D500F9", "#1DE9B6"))

growth_plot3

## %change in number of microbreweries, regional craft brewers and brew pubs over time
growth_plot4 <- ggplot(melt.percent.df, aes(x = Year, y = value, color = variable, fill = variable)) +
  geom_line() +
  geom_area(position = "identity", alpha = 1/2) +
  scale_colour_manual(values=c("#2962FF", "#D500F9", "#1DE9B6")) +
  scale_fill_manual(values=c("#2962FF", "#D500F9", "#1DE9B6")) +
  ylab("% growth in number of craft brewers") +
  theme(legend.position="top")

growth_plot4

## Actual numbers of craft brewers in US over time
melt.actual.df <- melt(brew_growth, id = "Year")
# write.csv(melt.actual.df, file = "brewers_actual.csv")


actual_growth <- ggplot(melt.actual.df, aes(x = Year, y = value, color = variable, fill = variable)) +
  geom_line() +
  geom_area(position = "identity", alpha = 1/2) +
  scale_colour_manual(values=c("#2962FF", "#D500F9", "#1DE9B6")) +
  scale_fill_manual(values=c("#2962FF", "#D500F9", "#1DE9B6")) +
  ylab("Number of craft brewers") +
  theme(legend.position="top")

actual_growth  
  


########################  
# US Imports & Exports #
########################

imports <- read.csv("US_imports.csv", stringsAsFactors = FALSE)

str(imports)

# clean up these data

imports <- imports %>% 
  slice(-1)

problem_rows <- imports %>% 
  filter(Change == "%")

problem_rows <- problem_rows %>% 
  select(-Change, -X., -Change.1, -X, -X.1, -X..1)
names(problem_rows) <- c("Country", "Dec_2016", "Dec_2017", "2016_ytd", "2017_ytd")  

## more efficient using [[]]?
problem_rows[, 2]  <- gsub("[^0-9\\.]", "", problem_rows[, 2]) %>% 
  as.numeric()
problem_rows[, 3]  <- gsub("[^0-9\\.]", "", problem_rows[, 3]) %>% 
  as.numeric()
problem_rows[, 4] <- gsub("[^0-9\\.]", "", problem_rows[, 4]) %>% 
  as.numeric()
problem_rows[, 5]  <- gsub("[^0-9\\.]", "", problem_rows[, 5]) %>% 
  as.numeric()


str(problem_rows)


good_rows <- imports %>% 
  filter(X..1 == "%") %>% 
  select(-X..1, -X., -Change.1, -X, -X.1, -X2017)
names(good_rows) <- c("Country", "Dec_2016", "Dec_2017", "2016_ytd", "2017_ytd")

str(good_rows)
good_rows[, 2]  <- gsub("[^0-9\\.]", "", good_rows[, 2]) %>% 
  as.numeric()
good_rows[, 3]  <- gsub("[^0-9\\.]", "", good_rows[, 3]) %>% 
  as.numeric()
good_rows[, 4]  <- gsub("[^0-9\\.]", "", good_rows[, 4]) %>% 
  as.numeric()
good_rows[, 5]  <- gsub("[^0-9\\.]", "", good_rows[, 5]) %>% 
  as.numeric()

clean_import <- union(problem_rows, good_rows) 

# need to clean these up
leftover <- anti_join(imports, clean_import)


clean_import <- clean_import %>% 
  filter(`2016_ytd` != 0) %>% 
  filter(`2017_ytd` != 0) %>% 
  mutate(p_changeYTD = percent_growth(`2016_ytd`, `2017_ytd`)) %>% 
  filter(Country != "LUXEMBURG")
  
str(clean_import)

region_list <- read.csv("Region_country_list.csv")

region_list <- region_list %>% 
  select(Region, Country) %>% 
  slice(1:129)

add_region <- left_join(clean_import, region_list) 
add_region$p_changeYTD <- round(add_region$p_changeYTD, 2)

smry_add_region <- add_region %>% 
  group_by(Region) %>% 
  mutate(mean_pchng = mean(p_changeYTD))

## Biggest areas of growth

import_growth_by_region <- add_region %>% 
  group_by(Region) %>% 
  summarise(mean_pchng = mean(p_changeYTD), sd_pchng = sd(p_changeYTD), max_pchng = max(p_changeYTD), min_pchng = min(p_changeYTD))



#########################################################################################
# Plot imports by region
# STILL WORKING ON THIS

## this is not that useful -- and may have errors
imp_box <- ggplot(add_region, aes(x = Region, y = p_changeYTD, fill = Region, color = Region)) +
  geom_boxplot(outlier.shape = NA, alpha = 1/5, show.legend = FALSE) +
  coord_cartesian(ylim = quantile(add_region$p_changeYTD, c(0.1, 0.9))) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 1/2, show.legend = FALSE)

# imp_box2 <- ggplot(add_region, aes(x = Region, y = p_changeYTD, fill = Region, color = Region)) +
#   geom_boxplot(outlier.shape = NA, alpha = 1/5, show.legend = FALSE) +
#   geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 2, alpha = 1/2, show.legend = FALSE)


imp <- ggplot(add_region, aes(x = Region, y = p_changeYTD, fill = Region, color = Region)) 

# imp + geom_point(size = 4, alpha = 1/2) + scale_y_log10()

imp + geom_bar(alpha = 1/2, stat = "identity") + 
  geom_point(size = 2) + 
  geom_jitter(position = position_jitter(width = 1, height = 0), alpha = 1/5)
####################################################################################### 



summary(add_region)
add_region$Country <- as.factor(add_region$Country)

growth_region_2016 <- add_region %>% 
  select(Region, Country, `2016_ytd`) %>% 
  rename(c("2016_ytd" = "Gallons")) %>% 
  mutate(YearID = rep(c("2016"), dim(add_region)[1]))

growth_region_2017 <- add_region %>% 
  select(Region, Country, `2017_ytd`) %>% 
  rename(c("2017_ytd" = "Gallons")) %>% 
  mutate(YearID = rep(c("2017"), dim(add_region)[1]))

full_growth_region <- rbind(growth_region_2016, growth_region_2017)

full_growth_region$YearID <- as.factor(full_growth_region$YearID)
str(full_growth_region)

full_growth_region %>% 
  as_tibble()

p_fgr <- ggplot(full_growth_region, aes(x = YearID, y = Gallons, group = Country, color = Region))

p_fgr + scale_y_log10() +
  geom_line(size = 0.5, show.legend = FALSE) +
  facet_wrap(~Region) +
  geom_point(aes(color = Region), show.legend = FALSE)

#############  
## Exports ##
#############

exports <- read.csv("US_exports.csv", stringsAsFactors = FALSE)

str(exports)

ex_good_rows <- exports %>% 
  select(Country, X12.2016, X12.2017, X2016, X2017) %>% 
  rename(c("X12.2016" = "Dec_2016", "X12.2017" = "Dec_2017", "X2016" = "2016_ytd", "X2017" = "2017_ytd"))
  

ex_good_rows[, 2]  <- gsub("[^0-9\\.]", "", ex_good_rows[, 2]) %>% 
  as.numeric()
ex_good_rows[, 3]  <- gsub("[^0-9\\.]", "", ex_good_rows[, 3]) %>% 
  as.numeric()
ex_good_rows[, 4]  <- gsub("[^0-9\\.]", "", ex_good_rows[, 4]) %>% 
  as.numeric()
ex_good_rows[, 5]  <- gsub("[^0-9\\.]", "", ex_good_rows[, 5]) %>% 
  as.numeric()

str(ex_good_rows)

clean_export <- ex_good_rows %>% 
  filter(`2016_ytd` != 0) %>% 
  filter(`2017_ytd` != 0) %>% 
  mutate(p_changeYTD = percent_growth(`2016_ytd`, `2017_ytd`))

str(clean_export)



region_list <- read.csv("Region_country_list.csv")

region_list <- region_list %>% 
  select(Region, Country) %>% 
  slice(1:127)

ex_add_region <- left_join(clean_export, region_list) 
ex_add_region$p_changeYTD <- round(ex_add_region$p_changeYTD, 2)



## Biggest areas of growth

smry_ex_add_region <- ex_add_region %>% 
  group_by(Region) %>% 
  summarise(mean_pchng = mean(p_changeYTD), sd_pchng = sd(p_changeYTD), max_pchng = max(p_changeYTD), min_pchng = min(p_changeYTD))

max_growth_exports <- ex_add_region %>% 
  group_by(Region) %>% 
  filter(p_changeYTD == max(p_changeYTD)) %>% 
  select(Region, Country, p_changeYTD)

write.csv(max_growth_exports, file = "max_growth_exports.csv")


max_actual_exports <- ex_add_region %>% 
  group_by(Region) %>% 
  filter(`2017_ytd` == max(`2017_ytd`)) %>% 
  select(Region, Country, `2017_ytd`)

write.csv(max_actual_exports, file = "max_actual_exports.csv")

 
## Make Facet graph showing change in exports from 2016  to 2017
ex_add_region$Country <- as.factor(ex_add_region$Country)

ex_growth_region_2016 <- ex_add_region %>% 
  select(Region, Country, `2016_ytd`) %>% 
  rename(c("2016_ytd" = "Gallons")) %>% 
  mutate(YearID = rep(c("2016"), dim(ex_add_region)[1]))

ex_growth_region_2017 <- ex_add_region %>% 
  select(Region, Country, `2017_ytd`) %>% 
  rename(c("2017_ytd" = "Gallons")) %>% 
  mutate(YearID = rep(c("2017"), dim(ex_add_region)[1]))

ex_full_growth_region <- rbind(ex_growth_region_2016, ex_growth_region_2017)

ex_full_growth_region$YearID <- as.factor(ex_full_growth_region$YearID)
str(ex_full_growth_region)

ex_full_growth_region %>% 
  as_tibble()

p_fgr <- ggplot(ex_full_growth_region, aes(x = YearID, y = Gallons, group = Country, color = Region))

p_fgr + scale_y_log10() +
  geom_line(size = 0.5, show.legend = FALSE) +
  facet_wrap(~Region) +
  geom_point(aes(color = Region), show.legend = FALSE)

  

  
  
  
  
  
  
