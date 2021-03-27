---
title: "Untitled"
author: "Lisa Reiber"
output:
  html_document:
    keep_md: TRUE
theme: "flatly"
output_dir: "/docs"
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
##           Julia  77   0.154     0.1859903
##          Python  88   0.176     0.2125604
##               R 170   0.340     0.4106280
##             SQL  79   0.158     0.1908213
##            <NA>  86   0.172            NA
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
##                   R 170   41.1%
##              Python  88   21.3%
##                 SQL  79   19.1%
##               Julia  77   18.6%
```

## count & mutate

### short

```r
fake_survey %>% dplyr::count(Q1_SC_fav_lang) 
```

```
##   Q1_SC_fav_lang   n
## 1          Julia  77
## 2         Python  88
## 3              R 170
## 4            SQL  79
## 5           <NA>  86
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
## 1                  R       170       41.1           41.1%
## 2             Python        88       21.3           21.3%
## 3                SQL        79       19.1           19.1%
## 4              Julia        77       18.6           18.6%
```

we like it so we turn it into a function...


```r
fake_survey %>% gen_sc_table(type = "fancy")
```

```
##   Favourite Language Frequency Proportion Prop in Percent
## 1                  R       170       41.1           41.1%
## 2             Python        88       21.3           21.3%
## 3                SQL        79       19.1           19.1%
## 4              Julia        77       18.6           18.6%
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
## 1     Nicht Gewählt       183       53.8           53.8%
## 2                Ja       157       46.2           46.2%
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
## 1          Nicht Gewählt       172       51.7           51.7%
## 2                     Ja       161       48.3           48.3%
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
##   Q2_MC_julia 170           175 155
##  Q2_MC_python 161           172 167
##       Q2_MC_r 157           183 160
##     Q2_MC_sql 169           178 153
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
##   Q2_MC_julia 0.2587519 (170) 0.2471751 (175) 0.2440945 (155)
##  Q2_MC_python 0.2450533 (161) 0.2429379 (172) 0.2629921 (167)
##       Q2_MC_r 0.2389650 (157) 0.2584746 (183) 0.2519685 (160)
##     Q2_MC_sql 0.2572298 (169) 0.2514124 (178) 0.2409449 (153)
##         Total 1.0000000 (657) 1.0000000 (708) 1.0000000 (635)
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
##   Q2_MC_julia 170   25.9%
##  Q2_MC_python 161   24.5%
##       Q2_MC_r 157   23.9%
##     Q2_MC_sql 169   25.7%
##         Total 657       -
```

What if I want to know the Anteil of peopgle who selected a certain language? 

Your turn :)



# cut outs

```r
fav_color <- c('Green', 'Red','Orange','Blue','Purple','Grey','Black','Yellow','White','Lavender')
```
