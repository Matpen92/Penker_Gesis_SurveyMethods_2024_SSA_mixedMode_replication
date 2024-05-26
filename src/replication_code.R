library(haven)
library(tidyverse)
library(hms)
library(table1)
library(labelled)
library(flextable)
library(officer)
library(patchwork)
library(jtools)
library(gridExtra)
library(sjPlot)
library(effectsize)
library(bayestestR)
library(rstanarm)

# data data --------------------------------------------------------------------

# download data from Aussda and add to the read_sav function below
# SSA 2024 data will be available soon
# change names to own directories 
# https://aussda.at/sozialer-survey-oesterreich/

ssoe23 <- 
  read_sav("data/26603012_extern(2).sav") |> 
  mutate(across(everything(), ~replace(., . %in% c(-8, -9, 9999), NA)))

ssoe24 <- 
  read_sav("data/26603015_extern.sav") |> 
  mutate(across(everything(), ~replace(., . %in% c(-8, -9, -4, 9999), NA)))

ssoe23$quelle_fac <- factor(ssoe23$quelle, labels = c("CAWI", "Mail"))
ssoe24$quelle_fac <- factor(ssoe24$quelle, labels = c("CAWI", "Mail"))

# data transform  --------------------------------------------------------------

sum_across_vars <- function(df, vars) {
  # sum score function
  selected_vars <- df[, vars, drop = FALSE]
  row_sums <- rowSums(selected_vars, na.rm = TRUE)
  return(row_sums)
}


average_across_vars <- function(df, vars) {
  # average score function
  selected_vars <- df[, vars, drop = FALSE]
  row_avgs <- rowMeans(selected_vars, na.rm = TRUE)
  return(row_avgs)
}

categorize_variable <- function(variable) {
  # Define the breaks for the categories
  breaks <- c(-Inf, 30, 45, 60, 75, Inf)
  # Define the labels for the categories
  labels <- c("18-29", "30-44", "45-59", "60-74", "75-high")
  # Use the cut function to categorize the variable
  categories <- cut(variable, breaks=breaks, labels=labels, right=FALSE)
  return(categories)
}

categorize_variable_issp <- function(variable) {
  # Define the breaks for the categories
  breaks <- c(-Inf, 30, 40, 50, 60, 70, Inf)
  # Define the labels for the categories
  labels <- c("18-29", "30-39", "40-49", "50-59", "60-90", ">70")
  # Use the cut function to categorize the variable
  categories <- cut(variable, breaks=breaks, labels=labels, right=FALSE)
  return(categories)
}

create_barplot <- function(unadjusted_diff, adjusted_diff, plot_title) {
  df <- data.frame(
    Category = factor(c("Unadjusted", "Adjusted"), 
                      levels = c("Unadjusted", "Adjusted")),
    Percentage = c(unadjusted_diff, adjusted_diff)
  )
  
  # Create the barplot
  ggplot(df, aes(x = Category, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("Unadjusted" = "#000000", "Adjusted" = "#332288")) +
    theme_bw() +
    labs(title = plot_title, x = "", y = "N Significant Differences") +
    theme(
      legend.position = "none",
      text = element_text(size = 18),          # Increase font size for all text elements
      plot.title = element_text(size = 20), # Center the plot title
      axis.title = element_text(size = 18),    # Increase font size for axis titles
      axis.text = element_text(size = 18)      # Increase font size for axis text
    )
}


# variable count ----------------------------------------------------------

# SSA-Items

ssoe23 %>%  
  select(contains("SSOE")) %>% 
  glimpse()

ssoe24 %>%  
  select(starts_with("A")) %>% 
  dim()

# number sociodem

ssoe23 %>% 
  select(starts_with("D"), -matches(".*_opn|.*_kat")) %>% 
  glimpse

ssoe24 %>% 
  select(starts_with("D"), -matches(".*_opn|.*_kat")) %>% 
  glimpse()


## ssoe23 ----------------------------------------------------------------

ssoe23 <- 
   ssoe23 %>% 
   mutate(educ_three_cat = case_when(
     (D3_1 >= 1 & D3_1 < 6) | D3_1 == 8 ~ 1,
     D3_1 == 6 | D3_1 == 7 | D3_1 == 15 ~2,
     (D3_1 >= 9 & D3_1 <= 14 ~ 3)))


ssoe23 <-
  ssoe23 %>% 
  mutate(D27_rec = case_when(
    D27 == 1 ~ 1,
    D27 == 2 ~ 2,
    D27 == 3 ~ 3,
    D27 == 4 ~ 4,
    D27 == 5 ~ 5,
    D27 >= 6 ~ 6,
    is.na(D27) ~ 7))#

ssoe23 <- 
  ssoe23 %>% 
  mutate(D24_rec = case_when(
    D24 < 6 ~ 1,
    D24 >= 6 & D24 <= 10 ~ 2,
    D24 == 11 ~3,
    is.na(D24) ~4))

ssoe23 <- 
  ssoe23 %>% 
  mutate(D4_rec = case_when(
    D4 == 1 | D4 == 2 ~ 1,
    D4 > 2 ~2,
    is.na(D4) ~3))


ssoe23$educ_three_cat_fac <- factor(ssoe23$educ_three_cat, labels = c("No Matura", "Matura", "Univeristy"))
                                                                
ssoe23$age_cat_five <- categorize_variable(ssoe23$age)

# ssoe23$age_cat_issp <- categorize_variable_issp(ssoe23$age)

table1(~ to_factor(D1) + age_cat_five  + educ_three_cat_fac   | quelle,
       render.missing = NULL,
       data = ssoe23)

## ssoe24 -----------------------------------------------------------------

ssoe24 <- 
  ssoe24 %>% 
  mutate(educ_three_cat = case_when(
    (D3X1 >= 1 & D3X1 < 6) | D3X1 == 8 ~ 1,
    D3X1 == 6 | D3X1 == 7 | D3X1 == 9 | D3X1 == 16 ~2,
    (D3X1 >= 10 & D3X1 <= 15 ~ 3)))

ssoe24$educ_three_cat_fac <- factor(ssoe24$educ_three_cat, labels = c("No Matura", "Matura", "Univeristy"))

ssoe24$age_cat_five <- categorize_variable(ssoe24$age)

ssoe24 <-
  ssoe24 %>% 
  mutate(D29_rec = case_when(
    D29 == 1 ~ 1,
    D29 == 2 ~ 2,
    D29 == 3 ~ 3,
    D29 == 4 ~ 4,
    D29 == 5 ~ 5,
    D29 >= 6 ~ 6,
    is.na(D29) ~ 7))#

ssoe24 <- 
ssoe24 %>% 
  mutate(D26_rec = case_when(
    D26 < 6 ~ 1,
    D26 >= 6 & D26 <= 10 ~ 2,
    D26 == 11 ~3,
    is.na(D26) ~4))

ssoe24 <- 
ssoe24 %>% 
  mutate(D4_rec = case_when(
    D4 == 1 | D4 == 2 ~ 1,
    D4 > 2 ~2,
    is.na(D4) ~3))

ssoe24 <- 
ssoe24 %>% 
  mutate(educ_bin = ifelse(educ_three_cat > 1, 1, 0))


table1(~educ_three_cat_fac  + age_cat_five +  as.factor(D1)  | quelle,
       render.missing = NULL,
       data = ssoe24)

# Representation bias -----------------------------------------------------

# Define the function
calculate_normalized_abs_diff <- function(p_bar, p) {

  normalized_abs_diffs <- abs((p_bar - p) / p)
  
  average_diff <- mean(normalized_abs_diffs)
  
  return(average_diff)
}

pbar_2018 <- c(46.3, 53.6, 13.9, 22.3,28.2,24.4,11.2, 67.9,17.1, 15)

pbar_2023_cawi <- c(45.0, 54.9, 14.7, 27.0, 28.2, 24.0, 6.2, 43.8, 23.1, 30.4)

pbar_2023_mm <- c(42.0, 57.6, 12.7, 23.7, 26.9, 27.0, 9.5, 48.5, 22.1, 26.8)

pbar_2024_cawi <- c(49.1, 50.2, 18.9, 25.7, 27.4, 21.8, 6.1, 43.9, 24.6, 29.6 )

pbar_2024_mm <- c(48.6, 50.6, 16.5, 23.7, 26.5, 23.2, 9.4, 48.4, 22.5, 27.2 )

pop <-  c(49.3, 50.7, 17.0, 24.8, 26.2, 20.5, 11.4, 69.1, 15.6, 15.3)

##

bias2018 <- calculate_normalized_abs_diff(pbar_2018, pop)

##

bias2023_cawi <- calculate_normalized_abs_diff(pbar_2023_cawi, pop)
bias2023_mm <- calculate_normalized_abs_diff(pbar_2023_mm, pop)

##

bias2024_cawi <- calculate_normalized_abs_diff(pbar_2024_cawi, pop)
bias2024_mm <- calculate_normalized_abs_diff(pbar_2024_mm, pop)

bias2018

# aggregate mode-effects ------------------------------------------------------------

mode_stats_aggregate <- function(data, variables, binary_variable) {
  
  results <- 
    data %>%
    select(all_of(c(variables, binary_variable))) %>%
    pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "values") %>%
    group_by(variable, !!sym(binary_variable)) %>%
    summarize(
      mean = mean(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = binary_variable, 
      values_from = c(mean, sd),
      names_sep = "_"
    )
  
  return(results)
}


## 2023 ------------------------------------------------------------------

dep_vars_mode_resp_23 <- 
  ssoe23 %>% 
  select(f0i1:A12SSOE, -f8, -f11, -f12, -f13,-f14,-f16,
         -f18,-f22, -f25, -f26,
         -contains("_kat"), ) %>% 
  colnames()

mode_effect_23_mean_sd <- mode_stats_aggregate(ssoe23, dep_vars_mode_resp_23, "quelle_fac")

p1 <- 
  mode_effect_23_mean_sd %>% 
  ggplot(aes(x= log(mean_CAWI), log(mean_Mail))) +
  geom_point(shape = 19, color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") +
  xlab("(log) Mean CAWI") +
  ylab("(log) Mean Mail") + 
  ggtitle("SSA 2023") +
  theme_bw()

p2 <- 
  mode_effect_23_mean_sd %>% 
  ggplot(aes(x= log(sd_CAWI), log(sd_Mail))) +
  geom_point(shape = 19, color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") +
  xlab("(log) SDs CAWI") +
  ylab("(log) SDs Mail") +
  theme_bw()

mode_effect_23_mean_sd %>% 
  select(mean_CAWI, mean_Mail, sd_CAWI, sd_Mail) %>% 
  correlation::correlation()

## 2024 ------------------------------------------------------------------

dep_vars_mode_resp_24 <- 
  ssoe24 %>% 
  select(Q1:F28_5, -Q9, -Q20, -c(Q2_1:Q2_13), -c(Q23_1:Q23_5), -(Q30:Q34), -E1, -Q27, -c(F28_1:F28_5), -contains(c("_kat", "_opn"))) %>% 
  colnames()

mode_effect_24_mean_sd <- mode_stats_aggregate(ssoe24, dep_vars_mode_resp_24, "quelle_fac")

p3 <-
  mode_effect_24_mean_sd %>% 
  ggplot(aes(x= mean_CAWI, mean_Mail)) +
  geom_point(shape = 19, color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") +
  xlab("Mean CAWI") +
  ylab("Mean Mail") +
  ggtitle("SSA 2024") +
  theme_bw()

p4 <- 
  mode_effect_24_mean_sd %>% 
  ggplot(aes(x= sd_CAWI, sd_Mail)) +
  geom_point(shape = 19, color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", colour = "black")  +
  xlab("SDs CAWI") +
  ylab("SDs Mail") +
  theme_bw()

mode_effect_24_mean_sd %>% 
  select(mean_CAWI, mean_Mail, sd_CAWI, sd_Mail) %>% 
  correlation::correlation()

####

grid.arrange(p1, p3, p2, p4, ncol=2)

###

# Crude mode effect -----------------------------------------------

estimate_crude_mode_effects <- function(data, mode_var, dep_vars) {
  
  # initialize an empty data frame to store results
  results <- data.frame()
  
  # loop over all dependent variables
  for (var in dep_vars) {
    # extract the variable label
    label <- label(data[[var]])
    
    # standardize the data
    data[var] <- scale(data[[var]])
    data[mode_var] <- scale(data[[mode_var]])
    # construct the formula for the lm() function
    formula <- as.formula(paste(var, "~", mode_var))
    
    # run the linear regression
    regression <- lm(formula, data=data)
    
    # get the estimated coefficient for the mode variable
    coef <- coefficients(regression)[mode_var]
    
    # get the p-value for the mode variable
    summary_reg <- summary(regression)
    p_value <- summary_reg$coefficients[mode_var, 4]
    
    # check if variable is significant
    significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
    
    # create a data frame row with the results
    results_row <- data.frame("Dependent.Variable" = var, 
                              "Coefficient_crude" = coef, 
                              "P-value_crude" = p_value, 
                              "Significance_crude" = significance)
    
    return(results_row)
  }
  
  # return the results data frame
  return(results)
}


## 2023 --------------------------------------------------------------------

results_crude_23 <- purrr::map_dfr(dep_vars_mode_resp_23, estimate_crude_mode_effects, 
                                data = ssoe23 , mode_var = "quelle") %>% 
  as_tibble()


## 2024 --------------------------------------------------------------------

results_crude_24 <- purrr::map_dfr(dep_vars_mode_resp_24, estimate_crude_mode_effects, 
                                   data = ssoe24 , mode_var = "quelle") %>% 
  as_tibble()

# net mode effect ---------------------------------------------------------

estimate_mode_effect <- function(var, data, mode_var, predictors, factors) {
  
  # extract the variable label
  label <- label(data[[var]])
  
  # extract value labels if var is a factor
  if("haven_labelled" %in% class(data[[var]])) {
    val_labels_var <- val_labels(data[[var]])
    if(length(val_labels_var) > 0){
      value_labels <- paste("high =", names(val_labels_var)[length(val_labels_var)])
    } else {
      value_labels <- NA
    }
  } else if(is.factor(data[[var]])) {
    value_labels <- paste(levels(data[[var]]), collapse = ", ")
  } else {
    value_labels <- NA
  }
  
  
  # convert specified variables to factors
  data[factors] <- lapply(data[factors], as.factor)
  
  # standardize the non-factor data
  non_factor_predictors <- setdiff(predictors, factors)
  data[var] <- scale(data[[var]])
  data[non_factor_predictors] <- lapply(data[non_factor_predictors], scale)
  
  # construct the formula for the lm() function
  predictor_str <- paste(predictors, collapse=" + ")
  formula <- as.formula(paste(var, "~", predictor_str))
  
  # run the linear regression
  regression <- lm(formula, data=data)
  
  
  # get the estimated coefficient for the mode variable
  coef <- coefficients(regression)[mode_var]
  
  # get the 95% confidence intervals for the mode variable
  conf_int <- confint(regression)[mode_var, ]
  
  # get the p-value for the mode variable
  summary_reg <- summary(regression)
  p_value <- summary_reg$coefficients[mode_var, 4]
  
  
  significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
  
  
  results_row <- data.frame("Dependent Variable" = var, 
                            "Variable Label" = label,
                            "Value Labels" = value_labels,
                            "Standardized Coefficient" = coef,
                            "95% CI Lower" = conf_int[1],
                            "95% CI Upper" = conf_int[2],
                            # "direction Effect" = coef_sign,
                            "P-value" = p_value, 
                            "Significance" = significance)
  
}


## 2023 --------------------------------------------------------------------

predictors_hard_23 <- 
  ssoe23 %>% 
  select(quelle, D1, educ_three_cat_fac, age, D4_rec,
         D27_rec, D24_rec, D45 , D46,  E1) %>% 
  colnames()


factors_23 <- 
  ssoe23 %>% 
  select(educ_three_cat_fac,D4_rec, D27_rec, D24_rec, D45,D46) %>% 
  colnames()


results_net_23 <- purrr::map_dfr(dep_vars_mode_resp_23, estimate_mode_effect, 
                              data = ssoe23 , mode_var = "quelle", 
                              predictors = predictors_hard_23, factors = factors_23) %>% 
  as_tibble()


###

table(results_crude_23$Significance_crude)
table(results_net_23$Significance)

68 / (68  + 69)
18 / (119 + 18)

###

results_combined_23 <- 
  results_net_23 %>% 
  select(Dependent.Variable,Standardized.Coefficient ) %>% 
  left_join(., results_crude_23, by = "Dependent.Variable") %>% 
  select(Standardized.Coefficient, Coefficient_crude)


df_long_23 <-
  results_combined_23 %>%
  pivot_longer(
    cols = everything(), # Convert all columns, adjust if you have more columns
    names_to = "Variable",
    values_to = "Value")

p5 <-  
  ggplot(df_long_23, aes(x = Value, fill = Variable)) + 
  geom_density(alpha = 0.6, adjust = 1) + 
  scale_fill_manual(values = c("#000000",  "#332288"), 
                    labels = c("Unadjusted Mode Difference", "Adjusted Mode Difference"))+ 
  labs( 
    x = "Standardized Coefficients", 
    y = "Density", 
    title = "Social Survey Austria 2023") +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.20, 0.35)) + 
  theme(
    legend.title = element_blank() 
  )

p5a <- create_barplot(68, 19, "Social Survey Austrian 2023")

## 2024 --------------------------------------------------------------------

predictors_hard_24 <- 
  ssoe24 %>% 
  select(quelle, D1 ,educ_three_cat_fac, age, D4_rec,
         D26_rec, D29_rec, D34 , D35 , Q1 ) %>% 
  colnames()

factors_24 <- 
  ssoe24 %>% 
  select(educ_three_cat_fac, D26_rec, D29_rec, D35, D4_rec) %>% 
  colnames()


results_net_24 <- purrr::map_dfr(dep_vars_mode_resp_24, estimate_mode_effect, 
                              data = ssoe24 , mode_var = "quelle", 
                              predictors = predictors_hard_24, factors = factors_24) %>% 
  as_tibble()

###

table(results_crude_24$Significance_crude)
table(results_net_24$Significance)

59 / (79  + 59) 

28 / (110 + 28)

###

results_combined_24 <- 
  results_net_24 %>% 
  select(Dependent.Variable,Standardized.Coefficient ) %>% 
  left_join(., results_crude_24, by = "Dependent.Variable") %>% 
  select(Standardized.Coefficient, Coefficient_crude)


df_long_24 <-
  results_combined_24 %>%
  pivot_longer(
    cols = everything(), # Convert all columns, adjust if you have more columns
    names_to = "Variable",
    values_to = "Value")

p6 <-  
  ggplot(df_long_24, aes(x = Value, fill = Variable)) + 
  geom_density(alpha = 0.6, adjust = 1) + 
  scale_fill_manual(values = c("#000000",  "#332288"), 
                    labels = c("Unadjusted Mode Difference", "Adjusted Mode Difference"))+ 
  labs( 
    x = "Standardized Coefficients", 
    y = "Density", 
    title = "Social Survey Austria 2024") +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.20, 0.35)) + 
  theme(
    legend.title = element_blank() 
  )

results_net_24 %>% 
  filter(Significance == "Significant") %>% view()

p6a <- create_barplot(59, 28, "Social Survey Austrian 2024")

###

###

p00 <- p5 / p6 + plot_layout(guides = 'collect') 
p0 <- (p5a / p6a)
p00 | p0


###

###

# Item-non_response  -----------------------------------------------

ssoe23 <- read_sav("data/26603012_extern(2).sav") |> 
  mutate(across(everything(), ~replace(., . %in% c(-8, -9, 999), NA)))

average_non_response <- function(data, mode_var, dependent_vars) {
  # Convert the variable names to symbols for tidy evaluation
  mode_var <- ensym(mode_var)
  
  # Transform dependent variables to indicators
  data[dependent_vars] <- lapply(data[dependent_vars], function(x) ifelse(is.na(x), 1, 0))
  
  # Calculate the average non-response proportion for each mode
  results <- data %>%
    group_by(!!mode_var) %>%
    summarise(across(all_of(dependent_vars), mean)) %>%
    # Reshape the data
    pivot_longer(cols = all_of(dependent_vars), names_to = "item", values_to = "mean_value") %>%
    pivot_wider(names_from = !!mode_var, values_from = mean_value)
  
  
  results <- 
    results %>% 
    rename(CAWI = `1`,
           Mail = `2`)
  
  return(results)
}

## 2023 --------------------------------------------------------------------

dep_vars_mode_non_resp_23 <-
  ssoe23 %>% 
  select(f0i1:f16, f22:A12SSOE, -contains("_kat")) %>% 
  names()

item_non_response_rates_23 <- average_non_response(ssoe23, "quelle", dep_vars_mode_non_resp_23)

item_non_response_rates_23_cor <-
  item_non_response_rates_23 %>% 
  mutate(number = row_number())

item_non_response_rates_23_cor %>% 
  correlation::correlation(.)

item_non_response_rates_23 %>% summary()

p8 <- 
  item_non_response_rates_23 %>% 
  ggplot(aes(x= CAWI, Mail)) +
  geom_point(shape = 19, color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") +
  xlab("CAWI: Item non-response rate") +
  ylab("Mail: Item non-response rate") + 
  ggtitle("Social Survey Austria 2023") +
  theme_bw()


y_breaks <- list(c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50))

long_data_23 <- item_non_response_rates_23 %>% 
  pivot_longer(cols = -item, names_to = "mode", values_to = "mean_value")

p9 <- 
  ggplot(long_data_23, aes(x = mode, y = mean_value, fill = mode, alpha = 0.5)) +
  geom_boxplot() +  
  scale_fill_manual(values = c(CAWI = "#E69F00", Mail = "#0072B2")) +
  theme_minimal() +
  scale_alpha(guide = "none") +
  labs(title = "Social Survey Austria 2023" ,y= "" , x = "") +
  theme(legend.title = element_blank()) +
  coord_flip()+
  scale_y_continuous(breaks = y_breaks[[1]], limits = c(0, 0.35))  # Set y-axis breaks and limits

p9

## 2024 --------------------------------------------------------------------

# load data again since average_non_response2 works with 
# (-8, -9, -4, 999999) and not NAs -->  not the smartest decision
# but too late for modification :)

ssoe24 <- read_sav("data/26603015_extern.sav", user_na = FALSE)

average_non_response2 <- function(data, mode_var, dependent_vars) {
  mode_var <- ensym(mode_var)
  
  data[dependent_vars] <- lapply(data[dependent_vars], function(x) {
    to_recode <- x %in% c(-8, -9, -4, 999999)
    recoded <- ifelse(to_recode, 1, ifelse(is.na(x), NA, 0))
    return(recoded)
  })
  
  results <- data %>%
    group_by(!!mode_var) %>%
    summarise(across(all_of(dependent_vars), ~mean(., na.rm = TRUE))) %>%
    pivot_longer(cols = all_of(dependent_vars), names_to = "item", values_to = "mean_value") %>%
    pivot_wider(names_from = !!mode_var, values_from = mean_value)
  
  results <- 
    results %>% 
    rename(CAWI = `1`,
           Mail = `2`)
  
  return(results)
}

dep_vars_mode_non_resp_24 <- 
  ssoe24 %>% 
  select(Q1, Q6_1:Q8,Q10_1:F27_5,-F26a, -F26b, -E1, -contains("_kat")) %>% 
  colnames()

item_non_response_rates_24 <- average_non_response2(ssoe24, "quelle", dep_vars_mode_non_resp_24)
item_non_response_rates_24

item_non_response_rates_24_cor <-
  item_non_response_rates_24 %>% 
   mutate(number = row_number())

item_non_response_rates_24_cor %>% 
  correlation::correlation(.)

p10 <- 
  item_non_response_rates_24 %>% 
  ggplot(aes(x= CAWI, Mail)) +
  geom_point(shape = 19, color = "black", alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") +
  xlab("CAWI: Item non-response rate") +
  ylab("Mail: Item non-response rate") + 
  ggtitle("Social Survey Austria 2024") +
  theme_bw()

p10

y_breaks <- list(c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50))

long_data_24 <- item_non_response_rates_24 %>% 
  pivot_longer(cols = -item, names_to = "mode", values_to = "mean_value")


p11 <- 
  ggplot(long_data_24, aes(x = mode, y = mean_value, fill = mode, alpha = 0.5)) +
  geom_boxplot() +  
  scale_fill_manual(values = c(CAWI = "#E69F00", Mail = "#0072B2")) +
  theme_minimal() +
  scale_alpha(guide = "none") +
  labs(title = "Social Survey Austria 2024",y= "" , x = "") +
  theme(legend.title = element_blank()) +
  coord_flip()+
  scale_y_continuous(breaks = y_breaks[[1]], limits = c(0, 0.35))  # Set y-axis breaks and limits

###

p8 / p10

p9 / p11 +  plot_layout(guides = 'collect') 

###

## Test-sig_ non_response --------------------------------------------------

### 2023 --------------------------------------------------------------------

inr_23_long <-
item_non_response_rates_23 %>% 
  select(CAWI, Mail) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "variable", 
    values_to = "value") 

# %>% 

t.test(value ~ variable, data = inr_23_long)
cohens_d(value ~ variable, data = inr_23_long)

### 2024 --------------------------------------------------------------------

inr_24_long <-
  item_non_response_rates_24 %>% 
  select(CAWI, Mail) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "variable", 
    values_to = "value") 

t.test(value ~ variable, data =inr_24_long)
cohens_d(value ~ variable, data = inr_24_long)

## Logistic regression -----------------------------------------------------

### 2023 --------------------------------------------------------------------

estimate_mode_effect_nonresponse_bayesian <- function(var, data, predictors, factors) {
  print("Start of function")
  
  # Set multi-core processing
  options(mc.cores = parallel::detectCores())
  
  # Convert specified variables to factors
  data[factors] <- lapply(data[factors], as.factor)
  
  # Transform 'var' into binary indicating missingness
  data[[var]] <- ifelse(is.na(data[[var]]), 1, 0)
  
  # Prepare the data for modeling, remove rows with any missing values in predictors or the target variable
  relevant_data <- na.omit(data[c(predictors, var)])
  
  # Construct the formula for the model
  formula <- as.formula(paste(var, "~", paste(predictors, collapse = " + ")))
  
  # Check for perfect separation specifically for 'quelle'
  if(any(table(relevant_data[[var]], relevant_data$quelle) == 0)) {
    print(warning("Perfect separation detected for 'quelle'. Model fitting not performed."))
    return(NULL)
  }
  
  t_prior <- student_t(df = 7, location = 0, scale = 1)
  
  # Fit the Bayesian logistic regression model
  fit <- stan_glm(formula, 
                  data = relevant_data, 
                  family = binomial(), 
                  prior = t_prior, 
                  prior_intercept = cauchy(0, 10),
                  chains = 4,
                  seed = 222,
                  iter = 3000, 
                  control = list(adapt_delta = 0.9999))
  
  print(paste("Model fitted for variable:", var))
  
  # Extract coefficient for 'quelle' and compute odds ratio
  quelle_coef_estimate <- fit$coefficients["quelle"]
  exp_quelle_coef_estimate <- exp(quelle_coef_estimate)
  
  # Compute posterior diagnostics
  p_d <- pd(fit) %>% 
    filter(Parameter == "quelle") 
  p_direction_check <- p_d$pd
  freq_p_val <- pd_to_p(p_direction_check)
  
  # Prepare results row
  results_row <- data.frame(
    DependentVariable = var, 
    EstimateOddsRatio = exp_quelle_coef_estimate, 
    p_direction = p_direction_check,
    p_val = freq_p_val
  )
  
  return(results_row)
}
results_23_non_resp  <- purrr::map_dfr(dep_vars_mode_non_resp_23, estimate_mode_effect_nonresponse_bayesian, 
                      data = ssoe23 , 
                      predictors = predictors_hard_23, factors = factors_23
) %>% as_tibble()

write_rds(results_23_non_resp, "output/präsis/gesis_präsi/ssoe23_bayes_non_resp.rds")

p12 <- 
  results_23_non_resp %>%
  ggplot(aes(x = round(EstimateOddsRatio, digits = 3))) + 
  geom_density(alpha = 0.6, adjust = 1, fill = "#7D3C98") +  # Adjusted for purple fill
  labs(
    x = "Odds Ratios (1= missing, 0 = no missing)", 
    y = "Density", 
    title = "Social Survey Austria 2023: "
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(NA, 10)) + # Set the upper limit of x-axis to 10
  theme(legend.title = element_blank())

###

results_23_non_resp <- 
  results_23_non_resp %>% 
  mutate(sig = ifelse(p_direction > 0.975, "sig", "n.s.")) 

results_23_non_resp %>% 
  filter(sig == "sig")

results_23_non_resp %>% 
  select(EstimateOddsRatio) %>% 
  summary()

### 2024 --------------------------------------------------------------------

estimate_mode_effect_nonresponse_bayesian_24 <- function(var, data, predictors, factors) {
  
  print("Start of function")
  
  options(mc.cores = parallel::detectCores())
  data[factors] <- lapply(data[factors], function(x) factor(x))
  
  data[[var]] <- ifelse(data[[var]] %in% c(-8, -9, -4, 999999), 1, 0)
  
  relevant_data <- na.omit(data[c(predictors, var)])
  
  formula <- as.formula(paste(var, "~", paste(predictors, collapse = " + ")))
  
  # Check for perfect separation
  if(any(table(relevant_data[[var]], relevant_data$quelle) == 0)) {
    print(warning("Perfect separation detected. Model fitting not performed."))
    return(NULL)
  }
  
  t_prior <- student_t(df = 7, location = 0, scale = 1)
  
  fit <- stan_glm(formula, 
                  data = relevant_data, 
                  family = binomial(), 
                  prior = t_prior, 
                  prior_intercept = cauchy(0, 10),
                  chains = 4, 
                  seed = 222,
                  iter = 3000, 
                  control = list(adapt_delta = 0.9999))
  
  
  print(paste("Model fitted for variable:", var))
  
  quelle_coef_estimate <- fit$coefficients["quelle"]
  
  exp_quelle_coef_estimate <- exp(quelle_coef_estimate)  # Convert to odds ratio
  
  p_d <- 
    pd(fit) %>% 
    filter(Parameter == "quelle") 
  
  p_direction_check <- p_d$pd
  
  freq_p_val <- pd_to_p(p_direction_check)
  
  results_row <- data.frame(
    DependentVariable = var, 
    EstimateOddsRatio = exp_quelle_coef_estimate, 
    p_direction = p_direction_check,
    p_val = freq_p_val
  )
  
  return(results_row)
}

results_24_non_resp <- purrr::map_dfr(dep_vars_mode_non_resp_24, estimate_mode_effect_nonresponse_bayesian_24, 
                             data = ssoe24 , 
                             predictors = predictors_hard_24, factors = factors_24
) %>% as_tibble()

write_rds(results_24_non_resp, "output/präsis/gesis_präsi/ssoe24_bayes_non_resp.rds")

p13 <- 
  results_24_non_resp %>%
  ggplot(aes(x = round(EstimateOddsRatio, digits = 3))) + 
  geom_density(alpha = 0.6, adjust = 1, fill = "#7D3C98") +  # Adjusted for purple fill
  labs(
    x = "Odds Ratios (1= missing, 0 = no missing)", 
    y = "Density", 
    title = "Social Survey Austria 2024"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(NA, 10)) + # Set the upper limit of x-axis to 10
  theme(legend.title = element_blank())

###

results_24_non_resp %>% 
  select(EstimateOddsRatio) %>% 
  summary()

results_24_non_resp <- 
  results_24_non_resp %>% 
  mutate(sig = ifelse(p_direction > 0.975, "sig", "n.s.")) 

results_24_non_resp %>% 
  filter(sig == "sig")

###

grid.arrange(p12, p13, ncol=1)

###

# heterogenous mode effects: Age  -----------------------------------------------

## 2023 --------------------------------------------------------------------

estimate_mode_effect_nonresponse_bayesian_hetero <- function(var, data) {
  print("Start of function")
  
  options(mc.cores = parallel::detectCores())
  
  # no factor for qulle or function breaks 
  # Convert the necessary variables to factors within the data argument directly
  data$educ_three_cat_fac <- factor(data$educ_three_cat_fac)
  data$D4_rec <- factor(data$D4_rec)
  data$D27_rec <- factor(data$D27_rec)
  data$D24_rec <- factor(data$D24_rec)
  
  # Transform 'var' into binary indicating missingness
  data[[var]] <- ifelse(is.na(data[[var]]), 1, 0)
  
  # Remove rows with any missing values in the variables used in the model
  relevant_variables <- c("quelle", "D1", "educ_three_cat_fac", "age", "D4_rec", "D27_rec", "D24_rec", "D45", var)
  relevant_data <- na.omit(data[relevant_variables])
  
  # Check for perfect separation
  if(any(table(relevant_data[[var]], relevant_data$quelle) == 0)) {
    print(warning("Perfect separation detected. Model fitting not performed."))
    return(NULL)
  }
  
  # Construct the formula for the model with interactions
  formula_str <- paste(var, "~ quelle*age+  quelle + D1 + educ_three_cat_fac + age + as.factor(D4_rec) +
                       as.factor(D27_rec) + as.factor(D24_rec) + D45")
  formula <- as.formula(formula_str)
  
  t_prior <- student_t(df = 7, location = 0, scale = 1)
  
  # Fit the Bayesian logistic regression model
  fit <- stan_glm(formula, 
                  data = relevant_data, 
                  family = binomial(), 
                  prior = t_prior, 
                  prior_intercept = cauchy(0, 10),
                  chains = 4, iter = 3000, 
                  control = list(adapt_delta = 0.9999))
  
  print(paste("Variable:", var))
  quelle_coef_estimate <- fit$coefficients["quelle:age"]
  print(paste("Quelle Coef Estimate:", quelle_coef_estimate))
  
  exp_quelle_coef_estimate <- exp(quelle_coef_estimate)  # Convert to odds ratio
  print(paste("Exp Quelle Coef Estimate:", exp_quelle_coef_estimate))
  
  # Assuming placeholders for these functions; add print statements similarly if they exist
  p_d <- 
    pd(fit) %>% 
    filter(Parameter == "quelle:age") 
  p_direction_check <- p_d$pd
  
  freq_p_val <- pd_to_p(p_direction_check)
  print(paste("Frequency P Value:", freq_p_val))
  
  # Create a dataframe row with the results
  results_row <- data.frame(
    DependentVariable = var, 
    EstimateOddsRatio = exp_quelle_coef_estimate, 
    p_direction = p_direction_check,
    p_val = freq_p_val
  )
  
  return(results_row)
}

results_23_het_age <- purrr::map_dfr(dep_vars_mode_non_resp_23, estimate_mode_effect_nonresponse_bayesian_hetero, data = ssoe23) %>% 
  as_tibble()

write_rds(results_23_het_age, "output/präsis/gesis_präsi/ssoe23_bayes_non_resp_het_age.rds")

results_23_het_age <- readRDS("output/präsis/gesis_präsi/ssoe23_bayes_non_resp_het_age.rds")

results_23_het_age %>% 
  filter(p_val < 0.05)

8 / 118

p14 <- 
  results_23_het_age %>%
  ggplot(aes(x = round(EstimateOddsRatio, digits = 3))) + 
  geom_density(alpha = 0.6, adjust = 1, fill = "#7D3C98") +  # Adjusted for purple fill
  labs(
    x = "Odds Ratios (1= missing, 0 = no missing)", 
    y = "Density", 
    title = "Social Survey Austria 2023: Age x Survey Mode"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.75, 1.5)) + 
  theme(legend.title = element_blank())

ssoe23$quelle <- factor(ssoe23$quelle)

###

## 2024 --------------------------------------------------------------------

library(rstanarm)

estimate_mode_effect_nonresponse_bayesian_hetero_24 <- function(var, data) {
  print("Start of function")
  
  options(mc.cores = parallel::detectCores())
  
  # no factor for qulle or function breaks 
  # Convert the necessary variables to factors within the data argument directly
  data$educ_three_cat_fac <- factor(data$educ_three_cat_fac)
  data$D4_rec <- factor(data$D4_rec)
  data$D26_rec <- factor(data$D26_rec)
  data$D29_rec <- factor(data$D29_rec)
  
  # Transform 'var' into binary indicating missingness
  data[[var]] <- ifelse(data[[var]] %in% c(-8, -9, -4, 999999), 1, 0)
  
  # Remove rows with any missing values in the variables used in the model
  relevant_variables <- c("quelle", "D1", "educ_three_cat_fac", "age", "D4_rec", "D26_rec", "D29_rec", "D34", "Q1", var)
  relevant_data <- na.omit(data[relevant_variables])
  
  # Check for perfect separation
  if(any(table(relevant_data[[var]], relevant_data$quelle) == 0)) {
    print(warning("Perfect separation detected. Model fitting not performed."))
    return(NULL)
  }
  
  # Construct the formula for the model with interactions
  formula_str <- paste(var, "~ quelle*age+  quelle + D1 + educ_three_cat_fac + age + as.factor(D4_rec) +
                       as.factor(D26_rec) + as.factor(D29_rec) + D34 + Q1")
  formula <- as.formula(formula_str)
  
  t_prior <- student_t(df = 7, location = 0, scale = 1)
  
  # Fit the Bayesian logistic regression model
  fit <- stan_glm(formula, 
                  data = relevant_data, 
                  family = binomial(), 
                  prior = t_prior, 
                  prior_intercept = cauchy(0, 10),
                  chains = 4, iter = 3000, 
                  control = list(adapt_delta = 0.9999))
  
  print(paste("Variable:", var))
  quelle_coef_estimate <- fit$coefficients["quelle:age"]
  print(paste("Quelle Coef Estimate:", quelle_coef_estimate))
  
  exp_quelle_coef_estimate <- exp(quelle_coef_estimate)  # Convert to odds ratio
  print(paste("Exp Quelle Coef Estimate:", exp_quelle_coef_estimate))
  
  # Assuming placeholders for these functions; add print statements similarly if they exist
  p_d <- 
    pd(fit) %>% 
    filter(Parameter == "quelle:age") 
  p_direction_check <- p_d$pd
  
  print(paste("P Direction Check:", p_direction_check))
  
  freq_p_val <- pd_to_p(p_direction_check)
  print(paste("Frequency P Value:", freq_p_val))
  
  # Create a dataframe row with the results
  results_row <- data.frame(
    DependentVariable = var, 
    EstimateOddsRatio = exp_quelle_coef_estimate, 
    p_direction = p_direction_check,
    p_val = freq_p_val
  )
  
  return(results_row)
}

results_24_het_age <- purrr::map_dfr(dep_vars_mode_non_resp_24,
                                 estimate_mode_effect_nonresponse_bayesian_hetero_24, 
                                 data = ssoe24) %>% 
  as_tibble()

results_24_het_age %>% 
  filter(p_val < 0.05)

p15 <- 
  results_24_het_age %>%
  ggplot(aes(x = round(EstimateOddsRatio, digits = 3))) + 
  geom_density(alpha = 0.6, adjust = 1, fill = "#7D3C98") +  # Adjusted for purple fill
  labs(
    x = "Odds Ratios (1= missing, 0 = no missing)", 
    y = "Density", 
    title = "Social Survey Austria 2024: Age x Survey Mode"
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.75, 1.5)) + 
  theme(legend.title = element_blank())

###

p14 / p15

###

