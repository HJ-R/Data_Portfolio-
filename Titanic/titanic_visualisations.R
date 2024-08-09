library(tidyverse)
library(ggplot2)
library(dplyr)

data <- read.csv("titanic_vis_clean.csv")


# barplot
ggplot(data, aes(x = as.factor(survived)))+
         geom_bar()+
         xlab("survived")+
         ylab("count")+
         ggtitle("count of survived passengers")
 
# scatter plot      
ggplot(data, aes(x = data$age, y = data$fare)) +
  geom_point()

#histogram
ggplot(data, aes(x = data$age))+
  geom_histogram(binwidth = 5, fill="blue", color="black")+
  xlab("age")+
  ylab("count")+
  ggtitle("age distribution of passengers")

#boxplot
ggplot(data, aes(x = as.factor(survived), y = age))+
  geom_boxplot()+
  xlab("survived")+
  ylab("age")+
  ggtitle("age distribution and density by survival status")


#violinplot
ggplot(data, aes(x = as.factor(survived), y = age))+
  geom_violin()+
  xlab("survived")+
  ylab("age")+
  ggtitle("age distribution and density by survival status")


#bar plot of passenger class
ggplot(data, aes(x = as.factor(data$pclass)))+
  geom_bar(fill = "green")+
  xlab("passenger class")+
  ylab("count")+
  ggtitle("count of passenger class")


#bar plot of Embarked
ggplot(data, aes(x = embarked))+
  geom_bar(fill = "lightblue")+
  xlab("embarking point")+
  ylab("count")+
  ggtitle("count of passenger class")


#scatterplot of age vs fare
ggplot(data, aes(x = age, y = fare))+
  geom_point(fill = "purple")+
  xlab("age")+
  ylab("fare")+
  ggtitle("age vs fare")


#scatterplot of age vs fare and survival
ggplot(data, aes(x = age, y = fare))+
  geom_point()+
  facet_grid(. ~ survived)+
  xlab("age")+
  ylab("fare")+
  ggtitle("age vs fare by survival staus")

#scatterplot of age vs fare and Pclass
ggplot(data, aes(x = age, y = fare))+
  geom_point(color = "red")+
  facet_grid(. ~ pclass)+
  xlab("age")+
  ylab("fare")+
  ggtitle("age vs fare by passenger class")+
 

  # Combined scatter plot of Age vs Fare by Pclass
  ggplot(data, aes(x = age, y = fare, color = as.factor(pclass))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("1" = "red", "2" = "orange", "3" = "green")) +
  xlab("Age") +
  ylab("Fare") +
  ggtitle("Age vs. Fare by Passenger Class") +
  labs(color = "Passenger Class")



install.packages("plotly")
library(plotly)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(dplyr)
library(plotly)



data <- data %>%
  group_by(pclass, survived) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = count / sum(count) * 100)

# Convert to data frame
data <- as.data.frame(data)

# Convert survived to factor
data$survived <- as.factor(data$survived)

# Create the interactive stacked bar plot
plot <- plot_ly(data, 
                x = ~pclass, 
                y = ~percentage, 
                type = 'bar', 
                color = ~survived,
                text = ~paste('Survived:', survived, '<br>Percentage:', round(percentage, 2), '%'),
                hoverinfo = 'text',
                textposition = 'auto') %>%
  layout(barmode = 'stack',
         xaxis = list(title = 'Passenger Class'),
         yaxis = list(title = 'Percentage'),
         title = 'Survival Proportions by Passenger Class',
         legend = list(title = list(text = 'Survived')))

plot

data <- read.csv("titanic_vis_clean.csv")

# scatter plot of age vs fare colored by embarkation with linear regression (lm)
ggplot(data, aes (x = age, y = fare, color= embarked))+
  geom_point()+
  geom_smooth(method = "lm", col = "blue")
  facet_grid(. ~ embarked)+
  xlab("age")+
  ylab("fare")+
  ggtitle("age and fare by embarkation")
