
library(ggplot2)
library(tidyverse)
library(plotrix)

library(corrplot)

library(ggdendro)

library(ggrepel)


library(ggplot2)
library(knitr)
library(dplyr)
library(GGally)
library(lubridate)
library(gridExtra)
library(psych)
library(memisc)



#Load the Data
YtInd <- read.csv("C:/Users/shreya/Desktop/New folder/Dataset.csv")

#dimension/size of Data set
dim(YtInd)

# Total Attributes in the Data set
attributes(YtInd)

#converting class of 2 columns
YtInd$publish_time <- factor(YtInd$publish_time)
YtInd$publish_time
YtInd$category_id <- factor(YtInd$category_id)
YtInd$category_id

#Data type of every attribute
str(YtInd)

head(YtInd)

plot(YtInd$publish_time)
plot(YtInd$publish_time, type = "b")
v2 = sort(YtInd$publish_time)
plot(v2)

boxplot()
summary(YtInd$likes)
boxplot(YtInd$likes)
boxplot(YtInd$dislikes)

boxplot()
summary(YtInd$category_id)
boxplot(YtInd$category_id)
boxplot(YtInd$category_id)

ggplot(aes(x=category_id),data=YtInd)+geom_bar()

table(YtInd$category_id)

ggplot(aes(x=publish_time),data=YtInd)+geom_bar()

table(YtInd$publish_time)


ggplot(aes(likes),data=YtInd[!is.na(YtInd$likes),])+
  geom_histogram(binwidth=100000)+scale_x_continuous(
    breaks=seq(0,50,500))+
  ggtitle("Distribution of likes")+labs(caption="bin width = 100000")

summary(YtInd$likes)

ggplot(aes(views),data=YtInd)+geom_histogram(binwidth=1000000)

table(YtInd$views)

table(YtInd$views)[which.max(table(YtInd$views))]

median(YtInd$views)

ggplot(aes(x=likes),data=YtInd)+
  geom_histogram(binwidth = 100000)+scale_x_continuous(breaks = seq(0,18,1))

summary(YtInd$likes)

ggplot(aes(x=dislikes),data=YtInd)+
  geom_histogram(binwidth = 10000)+scale_x_continuous(breaks = seq(0,18,1))


summary(YtInd$dislikes)

ggplot(aes(x=comment_count),data=YtInd)+
  geom_histogram(binwidth = 10000)+scale_x_continuous(breaks = seq(0,18,1))

summary(YtInd$comment_count)

barplot(YtInd$likes)


hist(YtInd$likes , breaks = 5)
hist( YtInd$likes, breaks = 3)
hist(YtInd$likes , breaks = 10)

slices <- c(10, 12,4, 16, 8)
lbls <- c("likes", "dislikes", "comment_count", "category_id", "views")
pie(slices, labels = lbls, main="Pie Chart of Trending YouTube Video")








sp1 <- youtube %>% 
  select(channel_title,views) %>% 
  group_by(channel_title) %>%
  summarise(views = sum(views)) %>% 
  arrange(-views)

sp1_head <- data.frame(head(sp1, 10))

ggplot(sp1_head, aes(x=reorder(channel_title, -views), y=views)) +
  geom_segment( aes(x=reorder(channel_title, views), xend=reorder(channel_title, views), y=0, yend=views), color="gold") +
  geom_point( color="gold4", size=6, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "10 Trending Youtube Channel", 
       subtitle = "With Most Views",
       caption = "Source : Youtube Trending Dataset", 
       x = "Category",
       y = "Number of Likes")

sp2 <- youtube %>% 
  filter(category_id == "Entertainment") %>% 
  select(publish_when, likes) %>% 
  group_by(publish_when)

ggplot(data = sp2, aes(x = publish_when, y = log(likes), fill = publish_when)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = log(mean(likes)))) +
  labs(title = "Number of Likes for Entertainment", 
       subtitle = "Based on Posted Time ", 
       caption = "Source : Youtube Trending Dataset", 
       x = NULL, 
       y = NULL)

sp3<-youtube %>% 
  select(timetotrend, publish_when)

ggplot(data = sp3, aes(x = timetotrend)) + 
  geom_bar(aes(fill = publish_when), position = "stack" ) +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  labs(title = "Distribution of Videos", 
       subtitle = "Based on Posted Time & Time to Trend", 
       caption = "Source : Youtube Trending Dataset", 
       x = 'Days Needed for Video to Trend', 
       y = 'Frequency')

sp4<- youtube %>% 
  select(category_id, likes) %>% 
  group_by(category_id) %>%
  summarise(likes = sum(likes)) %>%
  arrange(-likes)

sp4<- head(sp4, 5)

plot_sp4 <- youtube %>% 
  filter(category_id == c("Music","Entertainment", "Comedy","People and Blogs","Howto and Style")) %>% 
  select(category_id, publish_wday, likes) %>% 
  group_by(category_id, publish_wday) %>% 
  summarise(likes = sum(likes))

ggplot(data = plot_sp4, aes(x = publish_wday, y = likes)) +
  geom_line(aes(group = category_id, col  = category_id), size = 1.1) + 
  geom_point() +
  scale_color_brewer(palette = "Set1")  +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~category_id, ncol = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none", 
        # plot.background = element_rect(fill = "grey"), 
        strip.background = element_rect(fill = "firebrick4"), 
        strip.text = element_text(color = "white", size = 12, face = "bold"), 
  ) +
  labs(title = "Summary of Likes Each Day", 
       subtitle = "5 Most Liked Categories", 
       caption = "Source : Youtube Trending Dataset", 
       x = 'Days', 
       y = 'Likes')






















