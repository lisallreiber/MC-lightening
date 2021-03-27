---
title: "How to analyze Multiple Choice Survey Questions"
subtitle: "CorrelAid X Berlin - Lightening Talk"
author: "Lisa Reiber"
output:
  html_document:
    keep_md: TRUE
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
##           Julia  86   0.172     0.2009346
##          Python  82   0.164     0.1915888
##               R 172   0.344     0.4018692
##             SQL  88   0.176     0.2056075
##            <NA>  72   0.144            NA
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
##                   R 172   40.2%
##                 SQL  88   20.6%
##               Julia  86   20.1%
##              Python  82   19.2%
```

## count & mutate

### short

```r
fake_survey %>% dplyr::count(Q1_SC_fav_lang) 
```

```
##   Q1_SC_fav_lang   n
## 1          Julia  86
## 2         Python  82
## 3              R 172
## 4            SQL  88
## 5           <NA>  72
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
## 1                  R       172       40.2           40.2%
## 2                SQL        88       20.6           20.6%
## 3              Julia        86       20.1           20.1%
## 4             Python        82       19.2           19.2%
```

we like it so we turn it into a function...


```r
fake_survey %>% gen_sc_table(type = "fancy")
```

```
##   Favourite Language Frequency Proportion Prop in Percent
## 1                  R       172       40.2           40.2%
## 2                SQL        88       20.6           20.6%
## 3              Julia        86       20.1           20.1%
## 4             Python        82       19.2           19.2%
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
## 1                Ja       172       50.9           50.9%
## 2     Nicht Gewählt       166       49.1           49.1%
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
## 1          Nicht Gewählt       170       51.1           51.1%
## 2                     Ja       163       48.9           48.9%
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
##   Q2_MC_julia 169           180 151
##  Q2_MC_python 163           170 167
##       Q2_MC_r 172           166 162
##     Q2_MC_sql 191           159 150
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
##   Q2_MC_julia 0.2431655 (169) 0.2666667 (180) 0.2396825 (151)
##  Q2_MC_python 0.2345324 (163) 0.2518519 (170) 0.2650794 (167)
##       Q2_MC_r 0.2474820 (172) 0.2459259 (166) 0.2571429 (162)
##     Q2_MC_sql 0.2748201 (191) 0.2355556 (159) 0.2380952 (150)
##         Total 1.0000000 (695) 1.0000000 (675) 1.0000000 (630)
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
##   Q2_MC_julia 169   24.3%
##  Q2_MC_python 163   23.5%
##       Q2_MC_r 172   24.7%
##     Q2_MC_sql 191   27.5%
##         Total 695       -
```

What if I want to know the Anteil of peopgle who selected a certain language? 

Your turn :)



# cut outs

```r
fav_color <- c('Green', 'Red','Orange','Blue','Purple','Grey','Black','Yellow','White','Lavender')
```
