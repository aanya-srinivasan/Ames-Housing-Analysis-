library(tidyverse)
getwd()
setwd("C:/Users/srini/Downloads/EDA Labs")
data = read.table("ames_housing.csv", header = T, sep = ",", stringsAsFactors = TRUE)
View(data)

#1a

t.test(data$Sale_Price,
       
       alternative = "two.sided",
       
       mu = 210000,
       
       conf.level = 0.95)

#Null hypothesis (H0): The average sale price is $210,000
#Alternative hypothesis (Ha): The average sale price is not $210,000
#We used a one-sample t-test that resulted in a t-stat/t-value of -19.783, a df of 2924 and a  a p-value of 2.2e - 16. As the p-value is less than 0.05, we can reject the null hypothesis. The average sale price is not $210,000.

#1b

ggplot(data = data) +
  geom_histogram(aes(x = Sale_Price), 
                 fill = "pink", 
                 alpha = 0.5, 
                 color = "blue") +
  labs(title = "Sale Price of homes sold in Ames, Iowa", 
       subtitle = "Data from 2006 - 2010 Ames Housing Dataset", 
       x = "Sale Price ($)", 
       y = "Frequency") +
  geom_vline(aes(xintercept = mean(Sale_Price)), 
             color = "blue", 
             linetype = "dashed", 
             size = 1) +
  scale_x_continuous(breaks = seq(0, 755000, by = 100000)) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 60)) 

#2a

#Chi-squared test of independence 

chitest <- chisq.test(data$Roof_Style, data$Fence)
print(chitest)

#The null hypothesis (H0): Roof Style and Fence and indepedent 
#The alternative hypothesus (Ha): Roof Style and Fence are dependent (not independent)
#We used a chi-squared test of independence that resulted in a t-stat/X-squared of 21.698, a df of 16, and a p-value of 0.1532. As the p_value was greater than 0.01, we can accept our null hypothesis that Roof Style and Fence are independent.

#2b

ggplot(data = data) +
  
  geom_bar(mapping = aes(x = Roof_Style, fill = Fence),
           
           position = "fill") +  
  
  labs(title = "Roof Style Vs. Fence Quality", 
       subtitle = "Data from 2006 - 2010 Ames Housing Dataset", 
       x = "Roof Style", 
       y = "Count", 
       fill = "Fence Quality")

#3a

north_ames <- data %>%
  
  filter(Neighborhood == "North_Ames") %>%
  
  select(Sale_Price)

college_creek <- data %>%
  
  filter(Neighborhood == "College_Creek") %>%
  
  select(Sale_Price)


t.test(x= north_ames, y = college_creek,
       alternative = "two.sided",
       var.equal = TRUE,
       mu = 50000,
       conf.level = 0.98)

#null hypothesis (H0) = The price of houses in college_creek are on average $50,000 more than houses in north_ames
#alternative hypothesus (Ha) = The price of houses in college creek are not on average $50,000 more than houses in north_ames
# We used a two-sample t-test that resulted in a t-stat/t-value of -33.038, a df of 708, and a p-value of 2.2e - 16. As the p-value is less than 0.02, we can reject the null hypothesis that the average price in college creek is 50,000 more then in north ames.

#3b

combined <- data %>%
  
  filter(Neighborhood %in% c("North_Ames", "College_Creek"))

View(combined)

means <- combined %>%
  group_by(Neighborhood) %>%
  summarize(mean = mean(Sale_Price))

print(means)
View(means)

ggplot(data = combined) +
  geom_boxplot (aes(x = Neighborhood, y = Sale_Price), alpha = 0.45, fill = "pink", color = "blue" ) +
  labs(title = "Sales Price by Neighborhood", 
       subtitle = "Data from North_Ames and College_Creek neighborhoods (2006 - 2010)",
       x = "Neighborhood",
       y = "Sale Price ($)") +
  geom_point(data = means, aes(x = Neighborhood, y = mean), color = "black")

#4a

data2 <- data %>%
  
  mutate(Rooms = case_when(
    
  TotRms_AbvGrd >= 2 & TotRms_AbvGrd <= 4 ~ "2-4",
  
  TotRms_AbvGrd >= 5 & TotRms_AbvGrd <= 8 ~ "5-8", 
  
  TotRms_AbvGrd >= 9 ~ "9 or more"))

View(data2)

count <- table (data2$Rooms)
View(count)
sum <- sum(count)
print(sum) 

percentages <- c(0.05, 0.85, 0.10)
proportions <- percentages * sum
print(proportions)
class(proportions)

chisq.test(x = count, p = percentages)

#Null hypothesis (H0) = The percentage distribution of rooms is the same as the distribution proposed by John.
#Alternative hypothesis (Ha) = The percentage distribution of rooms is not the same as the distribution proposed by John.

# We used a chi-squared goodness of fit test that resulted in a t-stat/x-squared of 48.555, df of 2, and p-value of 2.861e-11. As the p-value was less than 0.01 we can reject the null hypothesis that John's percentage distribution was accurate. John's percentage distribute was inaccurate.

#4b

library(patchwork)

g1 <- ggplot(data = data2) +
  
  geom_bar(mapping = aes(x = Rooms, fill = Rooms),
           
           width = 0.9) +
  
  geom_text(aes(x = Rooms,label = scales::percent((..count..)/sum(..count..))),
            
            stat = "count",
            
            vjust = 1.5,
            
            size = 4,
            
            colour = "white") +
  labs (title = "Room Frequencies in Homes", subtitle = "Data from 2006 - 2010 Ames Housing Dataset", y = "Frequency")

g2 <- ggplot(data = data2) +
  
  geom_bar(mapping = aes(x = Rooms, fill = Rooms),
           
           width = 0.9) +
  
  geom_text(aes(x = Rooms,label = ..count..),
            
            stat = "count",
            
            vjust = 1.5,
            
            size = 4,
            
            colour = "white") +
  labs (title = "Room Frequencies in Homes", subtitle = "Data from 2006 - 2010 Ames Housing Dataset", y = "Frequency")

g1 + g2

#5a

ggplot(data = data2) + 
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price, color = Rooms), 
             size = 3, shape = 24) +
  
  labs (title = "Above Ground Living Area Vs. Sale Price", subtitle = "Data from 2006 - 2010 Ames Housing Dataset",
        
        x = "Living Area (ft^2)", y = "Sale Price ($)") +
  
  theme(plot.title=element_text(size=12, face="bold", color = "red"),
        
        axis.text.x=element_text(size=10, color = "black"),
        
        axis.text.y=element_text(size=10,  color = "black"),
        
        axis.title.x=element_text(size=12, color = "blue"),
        
        axis.title.y=element_text(size=12, color = "blue"),
        
        legend.position = "left") 


#5b

ggplot(data = data2) + 
  
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price)) +
  
  facet_wrap(~ Rooms) +
  
  labs (title = "Above Ground Living Area Vs. Sale Price by Rooms", subtitle = "Data from 2006 - 2010 Ames Housing Dataset",
                              
                              x = "Living Area (ft^2)", y = "Sale Price ($)")
