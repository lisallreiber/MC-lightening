How to analyze Multiple Choice Survey Questions
================================================================
CorrelAid X Berlin - Lightening Talk  
Lisa Reiber  
2021-15-03  

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

## Janitor::tabyl {.tabset}

### short

```r
fake_survey %>% janitor::tabyl(Q1_SC_fav_lang) 
```

```
##  Q1_SC_fav_lang   n percent valid_percent
##           Julia  91   0.182     0.2141176
##          Python  89   0.178     0.2094118
##               R 168   0.336     0.3952941
##             SQL  77   0.154     0.1811765
##            <NA>  75   0.150            NA
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
##                   R 168   39.5%
##               Julia  91   21.4%
##              Python  89   20.9%
##                 SQL  77   18.1%
```

## count & mutate {.tabset}

### short

```r
fake_survey %>% dplyr::count(Q1_SC_fav_lang) 
```

```
##   Q1_SC_fav_lang   n
## 1          Julia  91
## 2         Python  89
## 3              R 168
## 4            SQL  77
## 5           <NA>  75
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
## 1                  R       168       39.5           39.5%
## 2              Julia        91       21.4           21.4%
## 3             Python        89       20.9           20.9%
## 4                SQL        77       18.1           18.1%
```

we like it so we turn it into a function...


```r
fake_survey %>% gen_sc_table(type = "fancy")
```

```
##   Favourite Language Frequency Proportion Prop in Percent
## 1                  R       168       39.5           39.5%
## 2              Julia        91       21.4           21.4%
## 3             Python        89       20.9           20.9%
## 4                SQL        77       18.1           18.1%
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
## 1     Nicht Gewählt       178       51.7           51.7%
## 2                Ja       166       48.3           48.3%
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
##   Q2_MC_julia 180           144 176
##  Q2_MC_python 163           170 167
##       Q2_MC_r 166           178 156
##     Q2_MC_sql 181           173 146
```

How do I get frequencies?


```r
mc_long %>% 
  janitor::tabyl(name, value) %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_totals(where = "col") %>% 
  adorn_ns()
```

```
##          name          Ja Nicht Gewählt         NA_   Total
##   Q2_MC_julia 0.360 (180)   0.288 (144) 0.352 (176) 1 (500)
##  Q2_MC_python 0.326 (163)   0.340 (170) 0.334 (167) 1 (500)
##       Q2_MC_r 0.332 (166)   0.356 (178) 0.312 (156) 1 (500)
##     Q2_MC_sql 0.362 (181)   0.346 (173) 0.292 (146) 1 (500)
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
##   Q2_MC_julia 180   26.1%
##  Q2_MC_python 163   23.6%
##       Q2_MC_r 166   24.1%
##     Q2_MC_sql 181   26.2%
##         Total 690       -
```

What if I want to know the Anteil of peopgle who selected a certain language? 

Your turn :)



# cut outs

```r
fav_color <- c('Green', 'Red','Orange','Blue','Purple','Grey','Black','Yellow','White','Lavender')
```
