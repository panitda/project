---
  title: "WPA4"
output: html_notebook
---
  
  This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
# Assignment: WPA 4
# Name: Panitda Huynh
# Date: 5 April 2022
```

```{r}
library(tidyverse)
library(haven)
library(dplyr)
```

```{r}
data1 = read.csv("C:/Users/Kyurin Wong/OneDrive/Studium/Master FS 2022/Programming in R/Homework/WPA4/data/data_wpa4.csv")

glimpse(data1)
```
```{r}
datamatthews = read.csv("C:/Users/Kyurin Wong/OneDrive/Studium/Master FS 2022/Programming in R/Homework/WPA4/data/matthews_demographics.csv")

glimpse(datamatthews)
```

```{r}

new_matthews_data = mutate(data1, gender = recode(data1$gender, `1` = "male",`2` = "female"))
```

```{r}

colnames(data1)

data1 = data1 %>% 
  rename(
    gender_binary = gender
  )

data1 = mutate(data1, gender_binary = recode(data1$gender_binary, `1` = "0",`2` = "1"))

glimpse(data1)
```
```{r}
#new column income_labels in new_matthews_data

new_matthews_data %>%
  mutate(income_labels = new_matthews_data$income)
```
```{r}
#new column = income_recoded
new_matthews_data %>%
  mutate(income_recoded = new_matthews_data$income, case_when(new_matthews_data$income == "25,000"~1,
                                                              new_matthews_data$income == "25,000-50,000"~2,
                                                              new_matthews_data$income =="50,000-100,000"~3,
                                                              new_matthews_data$income == "100,000" ~4))

summarise(new_matthews_data)

###not working as wished

```
```{r}
new_matthews_data %>% mutate_if(is.numeric, as.integer)
```
```{r}
summarise(new_matthews_data)
#unknown because of ex 2
```

```{r}
#pipes did not work
survey_data = new_matthews_data %>%
  mutate(data1, gender = recode(data1$gender, `1` = "male",`2` = "female")) %>%
  data1 = data1 %>% 
  rename(
    gender_binary = gender
  ) %>%
  data1 = mutate(data1, gender_binary = recode(data1$gender_binary, `1` = "0",`2` = "1"))%>%
  mutate(income_labels = new_matthews_data$income) %>%
  %>% mutate_if(is.numeric, as.integer)


```
```{r}

#TASK B1 ; problem

#x = c(new_matthews_data$p1, new_matthews_data$p2, new_matthews_data$p3, )

grouped_data = group_by(new_matthews_data, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

#interesting_columns = c(´p1´, )

new_matthews_data %>%
  mean(grouped_data)

as.numeric(grouped_data)

summarise_all(select(grouped_data)
              
              ```
              ```{r}
              ###TASK B2
              
              new_matthews_data_long = gather(new_matthews_data, key='product', value='wtp', p1:p10)
              
              ```
              
              ```{r}
              print(new_matthews_data_long$id)
              print(new_matthews_data$id)
              ```
              
              ```{r}
              #TASK C
              demographics = new_matthews_data$id
              
              ###matthews_data_all = tibble(new_matthews_data_long$id, demographics)
              
              matthews_data_all = tibble(new_matthews_data_long$id)
              
              ```
              
              ```{r}
              mean_matthews_data_all = group_by(mean(new_matthews_data$wtp), new_matthews_data$id, datamatthews$height, datamatthews$race)
              
              ###group_by does not work???
              ```
              
              ```{r}
              ggplot(data = mean_matthews_data_all, mapping = aes(x = factor(race), y = mean_wtp)) +
                stat_summary(fun = "mean", geom="bar") +
                stat_summary(fun.data = mean_cl_normal, geom = "errorbar", size=1, width=.4) +
                labs(x = 'Race', y = 'Mean WTP') + 
                theme(axis.text.x = element_text(angle = 90))
              ```
              
              ```{r}
              ggplot(data = mean_matthews_data_all, mapping = aes(x = height, y = mean_wtp)) + 
                geom_point(alpha = 0.3, size= 2) +
                geom_smooth(method = lm, color='grey') +
                labs(x='Height', y='Mean WTP') +
                ggtitle("Relationship betweenheight and WTP")
              ```
              
              Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.
              
              When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
              
              The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
              