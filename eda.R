library(tidyverse)
library(ggthemes)
library(lubridate)
library(RColorBrewer)

#set theme
old_theme <- theme_get()
new_theme <- theme_set(theme_tufte() + theme(text = element_text(family = "Noto Sans CJK SC")))
                                             

#read data
topics <- as_tibble(read.csv("Data/AllTopics.csv"))

posts <- as_tibble(read.csv("Data/PostsForAnalysis.csv"))

users <- as_tibble(read.csv("Data/UsersForAnalysis.csv"))

posts_topics <- as_tibble(read.csv("Data/PostsTopicsForAnalysis.csv"))


#get colors from brewer



#Question1 num_followers vs num_posts

#copy of answer
topics %>% 
  arrange(desc(num_posts)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 20) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(name, -num_posts), 
               y = num_posts), 
           stat = "identity",
           fill = "#BAE4BC",
           width = 0.2) +
  geom_point(aes(x = name, y = num_posts, size = num_followers),
             color = "#0868AC") +
  labs(x = "Topic", y = "# Posts")+
  theme(axis.text.x=element_text(angle = 45))

#choose sapmles to make points
#quantile
interval <- c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,1)
enframe(quantile(topics$num_posts, probs = interval),
        name = 'quantile',
        value = 'num_posts')

#boxplot
topics %>% 
  filter(num_posts < quantile(topics$num_posts, probs = .95, na.rm = TRUE)) %>% 
  ggplot(aes(y = num_posts)) +
  geom_boxplot()

#geom_point
topics %>% 
  filter(num_posts < quantile(topics$num_posts, probs = .95, na.rm = TRUE)) %>% 
  ggplot(aes(x = num_posts, y = num_followers)) +
  geom_point()


#Question 2
posts %>% 
  mutate(year = as.factor(year(date))) %>% 
  group_by(year, time_of_day) %>% 
  summarise(votes = sum(votes_count, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = votes, fill = time_of_day)) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = .2) 

#Questoin 3

posts_votes <- posts %>% 
  group_by(user_id) %>% 
  summarise(user_votes = sum(votes_count),
            user_posts = length(unique(id)))



user_votes <- users %>% 
  filter(posts_count > 30) %>% 
  left_join(posts_votes, by = "user_id") %>% 
  mutate(mean_votes = as.double(user_votes) / user_posts) %>% 
  arrange(desc(mean_votes)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 20) %>% 
  select(name, mean_votes)
 
#question4
posts_topics_gather <- posts_topics %>% 
  gather(-id, key = 'Topic', value = 'tag', na.rm = TRUE) %>% 
  rename(post_id = id) %>% 
  select(post_id, Topic) %>% 
  filter(Topic != "tech") %>% 
  group_by(Topic) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 10)

posts_topics_gather %>% 
  ggplot(aes(x = reorder(Topic, -count),  
             y = count, fill = Topic)) +
  geom_bar(stat = "identity",
           width = .3) +
  labs(x = "Topic", y = "Count")+
  theme(axis.text.x=element_text(angle = 45))

