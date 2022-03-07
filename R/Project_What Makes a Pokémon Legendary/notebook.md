
## 1. Introduction
<p>In the world of Pokémon academia, one name towers above any other – Professor Samuel Oak. While his colleague Professor Elm specializes in Pokémon evolution, Oak has dedicated his career to understanding the relationship between Pokémon and their human trainers. A former trainer himself, the professor has first-hand experience of how obstinate Pokémon can be – particularly when they hold legendary status.</p>
<p>For his latest research project, Professor Oak has decided to investigate the defining characteristics of legendary Pokémon to improve our understanding of their temperament. Hearing of our expertise in classification problems, he has enlisted us as the lead researchers.</p>
<p>Our journey begins at the professor's research lab in Pallet Town, Kanto. The first step is to open up the Pokédex, an encyclopaedic guide to 801 Pokémon from all seven generations.</p>
<p><img src="https://assets.datacamp.com/production/project_712/img/legendary_pokemon.jpg" alt="Legendary Pokémon"></p>
<p><em>Source: <a href="https://www.flickr.com/photos/bagogames/">bagogames</a> on Flickr</em></p>


```R
# Load the tidyverse
library(tidyverse)

# Import the dataset and convert variables
pokedex <- read_csv("datasets/pokedex.csv", 
                    col_types = cols(name = col_factor(), 
                                     type = col_factor(),
                                     is_legendary = col_factor()))

# Look at the first six rows
head(pokedex, 6)

# Examine the structure
str(pokedex)
```


<table>
<thead><tr><th scope=col>pokedex_number</th><th scope=col>name</th><th scope=col>attack</th><th scope=col>defense</th><th scope=col>height_m</th><th scope=col>hp</th><th scope=col>percentage_male</th><th scope=col>sp_attack</th><th scope=col>sp_defense</th><th scope=col>speed</th><th scope=col>type</th><th scope=col>weight_kg</th><th scope=col>generation</th><th scope=col>is_legendary</th></tr></thead>
<tbody>
	<tr><td>1         </td><td>Bulbasaur </td><td> 49       </td><td> 49       </td><td>0.7       </td><td>45        </td><td>88.1      </td><td> 65       </td><td> 65       </td><td> 45       </td><td>grass     </td><td>  6.9     </td><td>1         </td><td>0         </td></tr>
	<tr><td>2         </td><td>Ivysaur   </td><td> 62       </td><td> 63       </td><td>1.0       </td><td>60        </td><td>88.1      </td><td> 80       </td><td> 80       </td><td> 60       </td><td>grass     </td><td> 13.0     </td><td>1         </td><td>0         </td></tr>
	<tr><td>3         </td><td>Venusaur  </td><td>100       </td><td>123       </td><td>2.0       </td><td>80        </td><td>88.1      </td><td>122       </td><td>120       </td><td> 80       </td><td>grass     </td><td>100.0     </td><td>1         </td><td>0         </td></tr>
	<tr><td>4         </td><td>Charmander</td><td> 52       </td><td> 43       </td><td>0.6       </td><td>39        </td><td>88.1      </td><td> 60       </td><td> 50       </td><td> 65       </td><td>fire      </td><td>  8.5     </td><td>1         </td><td>0         </td></tr>
	<tr><td>5         </td><td>Charmeleon</td><td> 64       </td><td> 58       </td><td>1.1       </td><td>58        </td><td>88.1      </td><td> 80       </td><td> 65       </td><td> 80       </td><td>fire      </td><td> 19.0     </td><td>1         </td><td>0         </td></tr>
	<tr><td>6         </td><td>Charizard </td><td>104       </td><td> 78       </td><td>1.7       </td><td>78        </td><td>88.1      </td><td>159       </td><td>115       </td><td>100       </td><td>fire      </td><td> 90.5     </td><td>1         </td><td>0         </td></tr>
</tbody>
</table>



    spec_tbl_df [801 x 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
     $ pokedex_number : num [1:801] 1 2 3 4 5 6 7 8 9 10 ...
     $ name           : Factor w/ 801 levels "Bulbasaur","Ivysaur",..: 1 2 3 4 5 6 7 8 9 10 ...
     $ attack         : num [1:801] 49 62 100 52 64 104 48 63 103 30 ...
     $ defense        : num [1:801] 49 63 123 43 58 78 65 80 120 35 ...
     $ height_m       : num [1:801] 0.7 1 2 0.6 1.1 1.7 0.5 1 1.6 0.3 ...
     $ hp             : num [1:801] 45 60 80 39 58 78 44 59 79 45 ...
     $ percentage_male: num [1:801] 88.1 88.1 88.1 88.1 88.1 88.1 88.1 88.1 88.1 50 ...
     $ sp_attack      : num [1:801] 65 80 122 60 80 159 50 65 135 20 ...
     $ sp_defense     : num [1:801] 65 80 120 50 65 115 64 80 115 20 ...
     $ speed          : num [1:801] 45 60 80 65 80 100 43 58 78 45 ...
     $ type           : Factor w/ 18 levels "grass","fire",..: 1 1 1 2 2 2 3 3 3 4 ...
     $ weight_kg      : num [1:801] 6.9 13 100 8.5 19 90.5 9 22.5 85.5 2.9 ...
     $ generation     : num [1:801] 1 1 1 1 1 1 1 1 1 1 ...
     $ is_legendary   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
     - attr(*, "spec")=
      .. cols(
      ..   pokedex_number = col_double(),
      ..   name = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
      ..   attack = col_double(),
      ..   defense = col_double(),
      ..   height_m = col_double(),
      ..   hp = col_double(),
      ..   percentage_male = col_double(),
      ..   sp_attack = col_double(),
      ..   sp_defense = col_double(),
      ..   speed = col_double(),
      ..   type = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
      ..   weight_kg = col_double(),
      ..   generation = col_double(),
      ..   is_legendary = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE)
      .. )
     - attr(*, "problems")=<externalptr> 



```R
library(testthat)
library(IRkernel.testthat)

soln_pokedex <- read_csv("datasets/pokedex.csv", 
                         col_types = cols(name = col_factor(), 
                                          type = col_factor(),
                                          is_legendary = col_factor()))

run_tests({
    
    test_that("packages are loaded", {
        expect_true("tidyverse" %in% .packages(), info = "Did you load the `tidyverse` package?")
    })
    
    test_that("data is loaded and formatted correctly", {
        expect_equal(pokedex, soln_pokedex,
                         info = "Did you convert `type` and `is_legendary` to factors using `col_factor()`?")
    })
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 10.243 0.248 86.146 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 2. How many Pokémon are legendary?
<p>After browsing the Pokédex, we can see several variables that could feasibly explain what makes a Pokémon legendary. We have a series of numerical fighter stats – <code>attack</code>, <code>defense</code>, <code>speed</code> and so on – as well as a categorization of Pokemon <code>type</code> (bug, dark, dragon, etc.). <code>is_legendary</code> is the binary classification variable we will eventually be predicting, tagged <code>1</code> if a Pokémon is legendary and <code>0</code> if it is not.</p>
<p>Before we explore these variables in any depth, let's find out how many Pokémon are legendary out of the 801 total, using the handy <code>count()</code> function from the <code>dplyr</code> package.</p>


```R
# Prepare the data
legendary_pokemon <- pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / sum(n))

# Print the data frame
print(legendary_pokemon)
```

    # A tibble: 2 x 3
      is_legendary     n   prop
      <fct>        <int>  <dbl>
    1 0              731 0.913 
    2 1               70 0.0874



```R
soln_legendary_pokemon <- soln_pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(soln_pokedex))

run_tests({
    
    test_that("data is prepared correctly", {
        
        expect_equal(colnames(legendary_pokemon)[1], colnames(soln_legendary_pokemon)[1],
                         info = "Did you pass `is_legendary` to the `count()` function?")
        
        expect_equal(legendary_pokemon[3], soln_legendary_pokemon[3],
                     info = "Did you divide `n` by `nrow(pokedex)`?")
    
  })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 10.453 0.252 86.36 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 3. Legendary Pokémon by height and weight
<p>We now know that there are 70 legendary Pokémon – a sizable minority at 9% of the population! Let's start to explore some of their distinguishing characteristics.</p>
<p>First of all, we'll plot the relationship between <code>height_m</code> and <code>weight_kg</code> for all 801 Pokémon, highlighting those that are classified as legendary. We'll also add conditional labels to the plot, which will only print a Pokémon's name if it is taller than 7.5m or heavier than 600kg.</p>


```R
# Prepare the plot
legend_by_heightweight_plot <- pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5 | weight_kg > 600, as.character(name), '')), 
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokemon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

# Print the plot
legend_by_heightweight_plot
```

    `geom_smooth()` using formula 'y ~ x'
    Warning message:
    "Removed 20 rows containing non-finite values (stat_smooth)."Warning message:
    "Removed 20 rows containing missing values (geom_point)."Warning message:
    "Removed 20 rows containing missing values (geom_text)."




![png](output_7_2.png)



```R
soln_legend_by_heightweight_plot <- soln_pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5|weight_kg > 600, as.character(name), '')), vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokemon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))
run_tests({
  
  test_that("the mappings are correct", {

        expect_equal(deparse(legend_by_heightweight_plot$mapping$x),
                         deparse(soln_legend_by_heightweight_plot$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `height_m`?')

        expect_equal(deparse(legend_by_heightweight_plot$mapping$y),
                         deparse(soln_legend_by_heightweight_plot$mapping$y),
                         info = 'The `y` aesthetic is incorrect. Did you map it to `weight_kg`?')

        expect_equal(legend_by_heightweight_plot$layers[[1]]$mapping,
                         soln_legend_by_heightweight_plot$layers[[1]]$mapping,
                         info = "The `col` aesthetic in `geom_point()` is incorrect. Did you map it to `is_legendary`?")
    
  })
  
  test_that("the conditional labels are correct", {
    
        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "height_m > 7.5"),
                     info = "The conditional labels are incorrect. Did you specify that `height_m` should be greater than `7.5` in the first argument of the `ifelse()` function?")

        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "weight_kg > 600"),
                    info = "The conditional labels are incorrect. Did you specify that `weight_kg` should be greater than `600` in the first argument of the `ifelse()` function?")

        expect_true(str_detect(legend_by_heightweight_plot$labels$label, "\\|"),
                    info = "The conditional labels are incorrect. Did you remember to use the conditional 'OR' operator (|)?")
    
  })
    
    test_that("the plot limits are correct", {
        
        expect_equal(ggplot_build(legend_by_heightweight_plot)$data[[4]],
                         ggplot_build(soln_legend_by_heightweight_plot)$data[[4]],
                         info = "The plot limits are incorrect. Did you expand the limit of the `x`-axis to 16?")
        
    })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 11.097 0.252 87.002 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 4. Legendary Pokémon by type
<p>It seems that legendary Pokémon are generally heavier and taller, but with many exceptions. For example, Onix (Gen 1), Steelix (Gen 2) and Wailord (Gen 3) are all extremely tall, but none of them have legendary status. There must be other factors at play.</p>
<p>We will now look at the effect of a Pokémon's <code>type</code> on its legendary/non-legendary classification. There are 18 possible types, ranging from the common (Grass / Normal / Water) to the rare (Fairy / Flying / Ice). We will calculate the proportion of legendary Pokémon within each category, and then plot these proportions using a simple bar chart.</p>


```R
# Prepare the data
legend_by_type <- pokedex %>% 
    group_by(type) %>% 
    mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
    summarise(prop_legendary = mean(is_legendary)) %>% 
    ungroup() %>% 
    mutate(type = fct_reorder(type, prop_legendary))

# Prepare the plot
legend_by_type_plot <- legend_by_type %>% 
    ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
    geom_col() +
    labs(title = "Legendary Pokemon by type") +
    coord_flip() +
    guides(fill = FALSE)

# Print the plot
legend_by_type_plot
```

    Warning message:
    "`guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead."




![png](output_10_2.png)



```R
soln_legend_by_type <- soln_pokedex %>% 
    group_by(type) %>% 
    mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
    summarise(prop_legendary = mean(is_legendary)) %>% 
    ungroup() %>% 
    mutate(type = fct_reorder(type, prop_legendary))

soln_legend_by_type_plot <- soln_legend_by_type %>% 
    ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
    geom_col() +
    labs(title = "Legendary Pokemon by type") +
    coord_flip() +
    guides(fill = FALSE)

run_tests({
  
  test_that("the data preparation is correct", {
      
      expect_equal(group_vars(legend_by_type), group_vars(soln_legend_by_type),
                   info = "Did you group by `type`?")
      
      expect_equal(legend_by_type$prop_legendary, soln_legend_by_type$prop_legendary,
                   info = "Did you set `prop_legendary` equal to the mean of `is_legendary`?")
      
      expect_equal(legend_by_type$type, soln_legend_by_type$type,
                  info = "Did you the `fct_reorder()` function to order `type` by `prop_legendary`?")

    
  })
    
    test_that("the plot preparation is correct", {
        
      expect_equal(deparse(legend_by_type_plot$mapping$y),
                       deparse(soln_legend_by_type_plot$mapping$y),
                       info = 'The `y` aesthetic is incorrect. Did you map it to `prop_legendary`?')
        
      expect_equal(deparse(legend_by_type_plot$mapping$fill),
                       deparse(soln_legend_by_type_plot$mapping$fill),
                       info = 'The `fill` aesthetic is incorrect. Did you map it to `prop_legendary`?')
    
  })
  
})
```

    Warning message:
    "`guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead."




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 11.688 0.26 87.6 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 5. Legendary Pokémon by fighter stats
<p>There are clear differences between Pokémon types in their relation to legendary status. While more than 30% of flying and psychic Pokémon are legendary, there is no such thing as a legendary poison or fighting Pokémon!</p>
<p>Before fitting the model, we will consider the influence of a Pokémon's fighter stats (<code>attack</code>, <code>defense</code>, etc.) on its status. Rather than considering each stat in isolation, we will produce a boxplot for all of them simultaneously using the <code>facet_wrap()</code> function.</p>


```R
# Prepare the data
legend_by_stats <- pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = fght_stats, value = value, -is_legendary) 

# Prepare the plot
legend_by_stats_plot <- legend_by_stats %>% 
 ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
 geom_boxplot(varwidth = TRUE) +
 facet_wrap(~fght_stats) +
 labs(title = "Pokemon fight statistics",
        x = "Legendary status") +
 guides(fill = "none")

# Print the plot
legend_by_stats_plot
```




![png](output_13_1.png)



```R
soln_legend_by_stats <- soln_pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = "fght_stats", value = "value", -is_legendary) 

soln_legend_by_stats_plot <- soln_legend_by_stats %>% 
 ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
 geom_boxplot(varwidth = TRUE) +
 facet_wrap(~fght_stats) +
 labs(title = "Pokemon fight statistics",
        x = "Legendary status") +
 guides(fill = FALSE)

run_tests({
  
  test_that("the data preparation is correct", {
      
      expect_equal(colnames(legend_by_stats)[1], colnames(soln_legend_by_stats)[1],
                       info = "Did you exclude `is_legendary` from the `gather()` process by placing it after the `-` sign?")
      
      expect_equal(colnames(legend_by_stats)[2:3], colnames(soln_legend_by_stats)[2:3],
                      info = "Did you set `\"fght_stats\"` as the `key` and `\"value\"` as the `value` when using `gather()`?")
    
  })
    
    test_that("the plot preparation is correct", {
        
      expect_equal(legend_by_stats_plot$facet$params$facets, soln_legend_by_stats_plot$facet$params$facets,
                        info = "Did you use the `facet_wrap()` function to facet by `fght_stats?`")
           
  })
})
```

    Warning message:
    "`guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead."




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 12.6 0.26 88.511 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 6. Create a training/test split
<p>As we might expect, legendary Pokémon outshine their ordinary counterparts in all fighter stats. Although we haven't formally tested a difference in means, the boxplots suggest a significant difference with respect to all six variables. Nonetheless, there are a number of outliers in each case, meaning that some legendary Pokémon are anomalously weak.</p>
<p>We have now explored all of the predictor variables we will use to explain what makes a Pokémon legendary. Before fitting our model, we will split the <code>pokedex</code> into a training set (<code>pokedex_train</code>) and a test set (<code>pokedex_test</code>). This will allow us to test the model on unseen data.</p>


```R
# Set seed for reproducibility
set.seed(1234)

# Save number of rows in dataset
n = nrow(pokedex)

# Generate 60% sample of rows
sample_rows <- sample(n, n*0.6)

# Create training set
pokedex_train <- pokedex  %>% 
  filter(row_number() %in% sample_rows)

# Create test set
pokedex_test <- pokedex  %>% 
  filter(!row_number() %in% sample_rows)
```


```R
set.seed(1234)

soln_n <- nrow(soln_pokedex)
soln_sample_rows <- sample(soln_n, 0.6 * soln_n)

soln_pokedex_train <- soln_pokedex  %>% 
  filter(row_number() %in% soln_sample_rows)

soln_pokedex_test <- soln_pokedex  %>% 
  filter(!row_number() %in% soln_sample_rows)

run_tests({
  
  test_that("the seed is correct", {
      expect_equal(sample_rows, soln_sample_rows,
                       info = "Did you set the seed to `1234`?")
  })
    
    test_that("the value for `n` is correct", {
        expect_equal(n, soln_n,
                        info = "The value for `n` is incorrect. Did you save it as the number of rows in `pokedex`?")
  })
    
    test_that("the sample is correct", {
        expect_equal(length(sample_rows), length(soln_sample_rows),
                        info = "Did you generate a 60% sample of rows?")
    })
    
    test_that("the training/test sets are correct", {
        expect_equal(pokedex_train, soln_pokedex_train,
                         info = "The training set is incorrect. Did you filter the `pokedex` using `sample_rows`?")
        
        expect_equal(pokedex_test, soln_pokedex_test,
                         info = "The test set is incorrect. Did you filter the `pokedex` using `sample_rows`?")
    })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 12.654 0.26 88.565 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 7. Fit a decision tree
<p>Now we have our training and test sets, we can go about building our classifier. But before we fit a random forest, we will fit a simple <strong>classification decision tree</strong>. This will give us a baseline fit against which to compare the results of the random forest, as well as an informative graphical representation of the model.</p>
<p>Here, and also in the random forest, we will omit incomplete observations by setting the <code>na.action</code> argument to <code>na.omit</code>. This will remove a few Pokémon with missing values for <code>height_m</code> and <code>weight_kg</code> from the training set. Remember the warning messages when we made our height/weight plot in Task 3? These are the Pokémon to blame!</p>


```R
# Load packages and set seed
library(rpart)
library(rpart.plot)

set.seed(1234)

# Fit decision tree
model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                    hp + sp_attack + sp_defense + speed + type + weight_kg,
                       data = pokedex_train,
                       method = "class",
                       na.action = na.omit)

# Plot decision tree
rpart.plot(model_tree)
```


![png](output_19_0.png)



```R
set.seed(1234)

soln_model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                    hp + sp_attack + sp_defense + speed + type + weight_kg,
                       data = soln_pokedex_train,
                       method = "class",
                       na.action = na.omit)

run_tests({
  
  test_that("packages are loaded", {
      expect_true("rpart" %in% .packages(), info = "Did you load the `rpart` package?")
      expect_true("rpart.plot" %in% .packages(), info = "Did you load the `rpart` package?")
  })
    
    test_that("the decision tree is correct", {
        
        expect_true(model_tree[["call"]][["data"]] == "pokedex_train",
                         info = "Did you train the model on `pokedex_train`?")
        
        expect_equal(model_tree[["call"]][["na.action"]], soln_model_tree[["call"]][["na.action"]],
                         info = "Did you set `na.action` to `na.omit`?")
        
    })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 13.974 0.264 89.889 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 8. Fit a random forest
<p>Each node of the tree shows the predicted class, the probability of being legendary, and the percentage of Pokémon in that node. The bottom-left node, for example – for those with <code>sp_attack &lt; 118</code> and <code>weight_kg &lt; 169</code> – represents 84% of Pokémon in the training set, predicting that each only has a 3% chance of being legendary.</p>
<p>Decision trees place the most important variables at the top and exclude any they don't find to be useful. In this case, <code>sp_attack</code> occupies node 1 while <code>attack</code>, <code>defense</code>, <code>sp_defense</code> and <code>height_m</code> are all excluded.</p>
<p>However, decision trees are unstable and sensitive to small variations in the data. It therefore makes sense to fit a <strong>random forest</strong> – an ensemble method that averages over several decision trees all at once. This should give us a more robust model that classifies Pokémon with greater accuracy.</p>


```R
# Load package and set seed
library(randomForest)

set.seed(1234)

# Fit random forest
model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                         hp + sp_attack + sp_defense + speed + type + weight_kg,
                         data = pokedex_train,
                         importance = TRUE,
                         na.action = na.omit)

# Print model output
model_forest
```


    
    Call:
     randomForest(formula = is_legendary ~ attack + defense + height_m +      hp + sp_attack + sp_defense + speed + type + weight_kg, data = pokedex_train,      importance = TRUE, na.action = na.omit) 
                   Type of random forest: classification
                         Number of trees: 500
    No. of variables tried at each split: 3
    
            OOB estimate of  error rate: 7.05%
    Confusion matrix:
        0  1 class.error
    0 411  9  0.02142857
    1  24 24  0.50000000



```R
random_state <- .Random.seed
set.seed(1234)
soln_random_state <- .Random.seed

soln_model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                         hp + sp_attack + sp_defense + speed + type + weight_kg,
                         data = soln_pokedex_train,
                         importance = TRUE,
                         na.action = na.omit)

run_tests({
  
  test_that("packages are loaded", {
      expect_true("randomForest" %in% .packages(), info = "Did you load the `randomForest` package?")
  })
    
    test_that("the random forest is correct", {
        
        expect_true(model_forest[["call"]][["data"]] == "pokedex_train",
                         info = "Did you train the model on `pokedex_train`?")
        
        expect_equal(model_forest[["call"]][["na.action"]], soln_model_forest[["call"]][["na.action"]],
                         info = "Did you set `na.action` to `na.omit`?")
        
    })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 14.601 0.276 90.527 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 9. Assess model fit
<p>Looking at the model output, we can see that the random forest has an out-of-bag (OOB) error of 7.48%, which isn't bad by most accounts. However, since there are <strong>24 true positives</strong> and <strong>24 false negatives</strong>, the model only has a recall of 50%, which means that it struggles to successfully retrieve every legendary Pokémon in the dataset.</p>
<p>In order to allow direct comparison with the decision tree, we will plot the <strong>ROC curves</strong> for both models using the <code>ROCR</code> package, which will visualize their true positive rate (TPR) and false positive rate (FPR) respectively. The closer the curve is to the top left of the plot, the higher the area under the curve (AUC) and the better the model.</p>


```R
# Load the ROCR package
library(ROCR)

# Create prediction and performance objects for the decision tree
probs_tree <- predict(model_tree, pokedex_test, type = "prob")
pred_tree <- prediction(probs_tree[,2], pokedex_test$is_legendary)
perf_tree <- performance(pred_tree, "tpr", "fpr")

# Create prediction and performance objects for the random forest
probs_forest <- predict(model_forest, pokedex_test, type = "prob")
pred_forest <- prediction(probs_forest[,2], pokedex_test$is_legendary)
perf_forest <- performance(pred_forest, "tpr", "fpr")

# Plot the ROC curves: first for the decision tree, then for the random forest
plot(perf_tree, col = "red", main = "ROC curves")
plot(perf_forest, add = TRUE, col = "blue")
legend(x = "bottomright",  legend = c("Decision Tree", "Random Forest"), fill = c("red", "blue"))
```


![png](output_25_0.png)



```R
soln_probs_forest <- predict(soln_model_forest, soln_pokedex_test, type = "prob")
soln_pred_forest <- prediction(soln_probs_forest[,2], soln_pokedex_test$is_legendary)
soln_perf_forest <- performance(soln_pred_forest, "tpr", "fpr")

run_tests({
  
  test_that("packages are loaded", {
      expect_true("ROCR" %in% .packages(), info = "Did you load the `ROCR` package?")
  })
    
    test_that("the prediction and performance objects are correct", {
        
        expect_equal(probs_forest, soln_probs_forest,
                         info = "The `probs_forest` object is incorrect. Did you remember to use `model_forest` rather than `model_tree`?")
        
        expect_equal(pred_forest, soln_pred_forest,
                         info = "The `pred_forest` object is incorrect. Did you remember to use `probs_forest` rather than `probs_tree`?")
        
        expect_equal(perf_forest, soln_perf_forest,
                         info = "The `perf_forest` object is incorrect. Did you remember to use `pred_forest` rather than `pred_tree`?")
        
  })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 14.794 0.284 90.728 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 


## 10. Analyze variable importance
<p>It's clear from the ROC curves that the random forest is a substantially better model, boasting an AUC (not calculated above) of 91% versus the decision tree's 78%. When calculating variable importance, it makes sense to do so with the best model available, so we'll use the random forest for the final part of our analysis.</p>
<p>Note that a random forest returns two measures of variable importance:</p>
<ul>
<li><code>MeanDecreaseAccuracy</code> – how much the model accuracy suffers if you leave out a particular variable</li>
<li><code>MeanDecreaseGini</code> – the degree to which a variable improves the probability of an observation being classified one way or another (i.e. 'node purity').</li>
</ul>
<p>Together, these two measures will allow us to answer our original research question – what makes a Pokémon legendary?</p>


```R
# Print variable importance measures
importance_forest <- importance(model_forest)
importance_forest

# Create a dotchart of variable importance
varImpPlot_forest <- varImpPlot(model_forest)
varImpPlot_forest
```


<table>
<thead><tr><th></th><th scope=col>0</th><th scope=col>1</th><th scope=col>MeanDecreaseAccuracy</th><th scope=col>MeanDecreaseGini</th></tr></thead>
<tbody>
	<tr><th scope=row>attack</th><td> 5.5880374</td><td> 8.5275682</td><td> 9.145018 </td><td> 6.203285 </td></tr>
	<tr><th scope=row>defense</th><td> 0.6724663</td><td>12.2083778</td><td> 8.506461 </td><td> 6.793236 </td></tr>
	<tr><th scope=row>height_m</th><td> 4.1403952</td><td> 6.7509476</td><td> 7.533695 </td><td> 6.579863 </td></tr>
	<tr><th scope=row>hp</th><td> 2.7180639</td><td>19.2406506</td><td>14.980599 </td><td> 9.499892 </td></tr>
	<tr><th scope=row>sp_attack</th><td> 6.3947972</td><td>22.5417527</td><td>18.567857 </td><td>17.415111 </td></tr>
	<tr><th scope=row>sp_defense</th><td>-0.8501697</td><td>15.1682650</td><td>10.919369 </td><td> 9.857089 </td></tr>
	<tr><th scope=row>speed</th><td> 2.2799561</td><td>21.0596369</td><td>15.838444 </td><td>11.308568 </td></tr>
	<tr><th scope=row>type</th><td> 3.3473633</td><td> 0.7726262</td><td> 3.347259 </td><td> 9.440971 </td></tr>
	<tr><th scope=row>weight_kg</th><td> 8.8857876</td><td> 6.6917782</td><td>11.343187 </td><td> 9.760121 </td></tr>
</tbody>
</table>




<table>
<thead><tr><th></th><th scope=col>MeanDecreaseAccuracy</th><th scope=col>MeanDecreaseGini</th></tr></thead>
<tbody>
	<tr><th scope=row>attack</th><td> 9.145018</td><td> 6.203285</td></tr>
	<tr><th scope=row>defense</th><td> 8.506461</td><td> 6.793236</td></tr>
	<tr><th scope=row>height_m</th><td> 7.533695</td><td> 6.579863</td></tr>
	<tr><th scope=row>hp</th><td>14.980599</td><td> 9.499892</td></tr>
	<tr><th scope=row>sp_attack</th><td>18.567857</td><td>17.415111</td></tr>
	<tr><th scope=row>sp_defense</th><td>10.919369</td><td> 9.857089</td></tr>
	<tr><th scope=row>speed</th><td>15.838444</td><td>11.308568</td></tr>
	<tr><th scope=row>type</th><td> 3.347259</td><td> 9.440971</td></tr>
	<tr><th scope=row>weight_kg</th><td>11.343187</td><td> 9.760121</td></tr>
</tbody>
</table>




![png](output_28_2.png)



```R
soln_importance_forest <- importance(soln_model_forest)
soln_varImpPlot_forest <- varImpPlot(soln_model_forest)

run_tests({
  
  test_that("the `importance` and `varImpPlot` objects are correct", {
      expect_equal(importance_forest, soln_importance_forest,
                       info = "Did you pass the random forest model to the `importance()` function?")
      expect_equal(varImpPlot_forest, soln_varImpPlot_forest,
                       info = "Did you pass the random forest model to the `varImpPlot()` function?")
  })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 14.927 0.284 90.86 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 



![png](output_29_2.png)


## 11. Conclusion
<p>According to the variable importance plot, <code>sp_attack</code> is the most important factor in determining whether or not a Pokémon is legendary, followed by <code>speed</code>. The plot doesn't tell us whether the variables have a positive or a negative effect, but we know from our exploratory analysis that the relationship is generally positive. We therefore conclude that legendary Pokémon are characterized primarily by the power of their special attacks and secondarily by their speediness, while also exhibiting higher fighting abilities across the board.</p>
<p>Congratulations on completing your research into legendary Pokémon – Professor Oak is excited to share the findings! To finish, we'll answer a few of his questions about the variable importance results.</p>


```R
# According to the MeanDecreaseAccuracy plot:

# Q1. Is the `attack` or `defense` variable more important?
answer1 <- 'attack'

# Q2. Is the `weight_kg` or `height_m` variable more important?
answer2 <- 'weight_kg'

# According to the MeanDecreaseGini plot:

# Q3. Is the `attack` or `defense` variable more important?
answer3 <- 'defense'

# Q4. Is the `weight_kg` or `height_m` variable more important?
answer4 <- 'weight_kg'
```


```R
soln_answer1 <- "attack"
soln_answer2 <- "weight_kg"
soln_answer3 <- "defense"
soln_answer4 <- "weight_kg"

run_tests({
  
  test_that("the answers are correct", {
      
      expect_equal(answer1, soln_answer1,
                      info = "The answer to Q1 is wrong. Did you check which variable was higher in the `MeanDecreaseAccuracy` plot?")
      
      expect_equal(answer2, soln_answer2,
                      info = "The answer to Q2 is wrong. Did you check which variable was higher in the `MeanDecreaseAccuracy` plot?")
      
      expect_equal(answer3, soln_answer3,
                      info = "The answer to Q3 is wrong. Did you check which variable was higher in the `MeanDecreaseGini` plot?")
      
      expect_equal(answer4, soln_answer4,
                      info = "The answer to Q4 is wrong. Did you check which variable was higher in the `MeanDecreaseGini` plot?")
      
  })
  
})
```




    <ProjectReporter>
      Inherits from: <ListReporter>
      Public:
        .context: NULL
        .end_context: function (context) 
        .start_context: function (context) 
        add_result: function (context, test, result) 
        all_tests: environment
        cat_line: function (...) 
        cat_tight: function (...) 
        clone: function (deep = FALSE) 
        current_expectations: environment
        current_file: some name
        current_start_time: 15.008 0.284 90.94 0.006 0.005
        dump_test: function (test) 
        end_context: function (context) 
        end_reporter: function () 
        end_test: function (context, test) 
        get_results: function () 
        initialize: function (...) 
        is_full: function () 
        out: 3
        results: environment
        rule: function (...) 
        start_context: function (context) 
        start_file: function (name) 
        start_reporter: function () 
        start_test: function (context, test) 

