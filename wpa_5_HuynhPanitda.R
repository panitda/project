# Assignment: WPA 5
# Name: Panitda Huynh
# Date: 15 April 2022

#Loading all the packages
library(tidyverse)
library(ggplot2)
library(fivethirtyeight)

head(police_locals)

###A.1

police_locals_new = police_locals %>% 
  pivot_longer(cols = all:asian,
               names_to = "ethnic_group",
               values_to = "perc_locals",) 

police_locals_new <- arrange(police_locals_new, ethnic_group)

head(police_locals_new, 10)

view(police_locals_new)


#these are the solutions for A2, as we will cover plotting later in the seminar
ggplot(data = police_locals_new, mapping = aes(x = reorder(ethnic_group, perc_locals), y = perc_locals)) +
  geom_boxplot() +
  labs(x = 'Ethnic group', y = 'Mean Percentage Locals') + 
  theme(axis.text.x = element_text(angle = 90))

#B.2
head(unisex_names)
view(unisex_names)


unisex_names_new <- unisex_names %>% 
  rename(female = female_share,
         male = male_share) %>% 
  pivot_longer(cols = female:male,
               names_to = "gender",
               values_to = "share",) 
view(unisex_names_new)  

unisex_names_new <- mutate (unisex_names_new, 
                            share = share * 100)
view(unisex_names_new)

unisex_names_new <- unisex_names_new %>% 
  arrange(desc(total))
head(unisex_names_new, 10)

unisex_names_common <- unisex_names_new [unisex_names_new$total > 50000,] 

# 3:these are the solutions for B3, as we will cover plotting later in the seminar
ggplot(data = unisex_names_common, mapping = aes(x = name, y = share, fill = gender)) +
  geom_col() +
  labs(x = 'Name', y = 'Share', fill='Gender') + 
  theme(axis.text.x = element_text(angle = 90))

#C1 

head(tv_states)
view(tv_states)

tv_coverage_hurricans_states <- tv_states %>% 
  pivot_longer(cols= florida: puerto_rico,
               names_to = "US_state",
               values_to = "perc_media_coverage") %>% 
  mutate(perc_media_coverage = perc_media_coverage *100) %>% 
  arrange(desc(date))


view(tv_coverage_hurricans_states)
#2:these are the solutions for C2, as we will cover plotting later in the seminar
ggplot(data = tv_coverage_hurricans_states, mapping = aes(x = date, y= perc_media_coverage, fill = US_state)) + 
  geom_col() +
  labs(x = 'Date', fill = 'State', y= 'Share of sentences')

ggplot(tv_coverage_hurricans_states, aes(x=date, y=perc_media_coverage, group=US_state, color=US_state)) +  
  geom_line() + 
  labs(x = 'Date', y = 'News coverage (%)', color='Area') + 
  scale_color_manual(values=c("#54F708", "blue", "red")) + 
  theme(axis.text.x = element_text(angle = 90, size = 6))

