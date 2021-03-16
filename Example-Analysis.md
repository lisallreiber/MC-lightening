---
title: "Untitled"
author: "Lisa Reiber"
output:
  html_document:
    keep_md: TRUE
theme: "flatly"
ourput_dir: "/docs"
---




```r
pacman::p_load(tidyverse, janitor)

source("helper_functions.R")
# fs_codebook(c())
```


```r
language <- c("R", "R", "Python", "Julia", "SQL", NA)

fake_survey <- data.frame(Q2_MC_r = sample(c('Ja','Nicht Gewählt', NA), 500, rep = TRUE),
                          Q2_MC_python = sample(c('Ja','Nicht Gewählt', NA), 500, rep = TRUE), 
                          Q2_MC_julia = sample(c('Ja','Nicht Gewählt', NA), 500, rep = TRUE), 
                          Q2_MC_sql = sample(c('Ja','Nicht Gewählt', NA), 500, rep = TRUE), 
                          Q1_SC_fav_lang = sample(language, 500, rep = TRUE)
                          ) %>% 
  rownames_to_column(var = "id") %>% 
  mutate(id = as.numeric(id))
```

# Single Choice

Q1: What is your favourite language ?

## Janitor::tabyl

### short

```r
fake_survey %>% janitor::tabyl(Q1_SC_fav_lang) 
```

```
##  Q1_SC_fav_lang   n percent valid_percent
##           Julia  71   0.142     0.1748768
##          Python  86   0.172     0.2118227
##               R 170   0.340     0.4187192
##             SQL  79   0.158     0.1945813
##            <NA>  94   0.188            NA
```

### fancy

```r
fake_survey %>% 
  janitor::tabyl(Q1_SC_fav_lang, show_na = FALSE) %>% 
  arrange(desc(n)) %>% 
  janitor::adorn_pct_formatting() %>% 
  janitor::adorn_title(row_name = "Favourite Language", col_name = "")
```

```
##                                
##  Favourite Language   n percent
##                   R 170   41.9%
##              Python  86   21.2%
##                 SQL  79   19.5%
##               Julia  71   17.5%
```

## count & mutate

### short

```r
fake_survey %>% dplyr::count(Q1_SC_fav_lang) 
```

```
##   Q1_SC_fav_lang   n
## 1          Julia  71
## 2         Python  86
## 3              R 170
## 4            SQL  79
## 5           <NA>  94
```

### fancy

```r
fake_survey %>% 
  drop_na(Q1_SC_fav_lang) %>% 
  count(`Favourite Language` = Q1_SC_fav_lang, name = "Frequency", sort = TRUE) %>% 
  mutate("Proportion" = round(Frequency / sum(Frequency) * 100, 1),
         "Prop in Percent" = paste0(Proportion, "%"))
```

```
##   Favourite Language Frequency Proportion Prop in Percent
## 1                  R       170       41.9           41.9%
## 2             Python        86       21.2           21.2%
## 3                SQL        79       19.5           19.5%
## 4              Julia        71       17.5           17.5%
```

we like it so we turn it into a function...


```r
fake_survey %>% gen_sc_table(type = "fancy")
```

```
##   Favourite Language Frequency Proportion Prop in Percent
## 1                  R       170       41.9           41.9%
## 2             Python        86       21.2           21.2%
## 3                SQL        79       19.5           19.5%
## 4              Julia        71       17.5           17.5%
```

```r
# same as:
# fake_survey %>% 
#  gen_sc_table(sc_var = "Q1_SC_fav_lang", 
#               sc_label = "Favourite Language", 
#               type = "fancy")
```

# Multiple Choice

Q2: Which programming Languages do you know?

As we can see, there are multiple variables for question 2. For each language that participants were able to choose from, one variable was generated.

## R

```r
fake_survey %>% 
 gen_sc_table(sc_var = "Q2_MC_r", 
              sc_label = "Known Language: R", 
              type = "fancy")
```

```
##   Known Language: R Frequency Proportion Prop in Percent
## 1                Ja       165       50.8           50.8%
## 2     Nicht Gewählt       160       49.2           49.2%
```
## Python


```r
fake_survey %>% 
 gen_sc_table(sc_var = "Q2_MC_python", 
              sc_label = "Known Language: Python", 
              type = "fancy")
```

```
##   Known Language: Python Frequency Proportion Prop in Percent
## 1          Nicht Gewählt       165       50.6           50.6%
## 2                     Ja       161       49.4           49.4%
```

## How can we compare all of the answers?

### Step1: Select relevant data

How do we select all the relevant questions?

We can identify all questions of an item battery by their stem. And then we can use the tidyselect helpers `tidyselect::starts_with()`


```r
mc_stem_selected <- "Q2_MC_"
```


```r
mc_selected <- fake_survey %>% 
  select(id, starts_with(all_of(mc_stem_selected))) 
```

let's take a look
### Step2: Reshape to long format

How we 

```r
mc_long <- mc_selected %>% 
  pivot_longer(-id)
```


```r
mc_long %>% 
  janitor::tabyl(name, value)
```

```
##          name  Ja Nicht Gewählt NA_
##   Q2_MC_julia 148           171 181
##  Q2_MC_python 161           165 174
##       Q2_MC_r 165           160 175
##     Q2_MC_sql 180           165 155
```

How do I get frequencies?


```r
mc_long %>% 
  janitor::tabyl(name, value) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_totals(where = "row") %>% 
  adorn_ns()
```

```
##          name              Ja   Nicht Gewählt             NA_
##   Q2_MC_julia 0.2262997 (148) 0.2586989 (171) 0.2642336 (181)
##  Q2_MC_python 0.2461774 (161) 0.2496218 (165) 0.2540146 (174)
##       Q2_MC_r 0.2522936 (165) 0.2420575 (160) 0.2554745 (175)
##     Q2_MC_sql 0.2752294 (180) 0.2496218 (165) 0.2262774 (155)
##         Total 1.0000000 (654) 1.0000000 (661) 1.0000000 (685)
```


```r
mc_long %>%
  filter(value == "Ja") %>% 
  janitor::tabyl(name) %>% 
  adorn_pct_formatting() %>% 
  adorn_totals()
```

```
##          name   n percent
##   Q2_MC_julia 148   22.6%
##  Q2_MC_python 161   24.6%
##       Q2_MC_r 165   25.2%
##     Q2_MC_sql 180   27.5%
##         Total 654       -
```

What if I want to know the Anteil of peopgle who selected a certain language? 

Your turn :)



# cut outs

```r
fav_color <- c('Green', 'Red','Orange','Blue','Purple','Grey','Black','Yellow','White','Lavender')
```
