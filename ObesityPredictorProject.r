# =============================================================================
# DATA 491 - Socioeconomic Predictors of County-Level Obesity
# Author: Priyanjana Chaudhary
# =============================================================================

library(tidyverse)
library(janitor)
library(corrplot)
library(broom)
library(car)
library(lmtest)
library(scales)
library(randomForest)

# STEP 1: PROJECT STRUCTURE

getwd()
dir.create("data",   showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("plots",  showWarnings = FALSE)

# STEP 2: LOAD RAW DATA

places_raw <- read_csv("data/PLACES_county_2024.csv") |> clean_names()
chr_raw    <- read_csv("data/CHR_analytic_data2025.csv") |> clean_names()

glimpse(places_raw)
glimpse(chr_raw)
dim(places_raw)
dim(chr_raw)

# STEP 3: EXPLORE & UNDERSTAND THE DATA

places_raw |>
  distinct(measure_id, short_question_text) |>
  print(n = 50)

summary(places_raw)
colSums(is.na(places_raw))

names(chr_raw)
summary(chr_raw)
colSums(is.na(chr_raw))

# Distribution of the outcome variable
obesity_preview <- places_raw |>
  filter(measure_id == "OBESITY")

ggplot(obesity_preview, aes(x = data_value)) +
  geom_histogram(bins = 40, fill = "#378ADD", color = "white", alpha = 0.85) +
  labs(
    title = "Distribution of County-Level Adult Obesity Rates",
    subtitle = "CDC PLACES 2024",
    x = "Obesity Rate (% of adults)",
    y = "Number of Counties"
  ) +
  theme_minimal(base_size = 13)

ggsave("plots/01_obesity_distribution.png", width = 8, height = 5, dpi = 300)

# STEP 4: PREPARE & CLEAN DATA

# Extract obesity rate from PLACES
obesity_df <- places_raw |>
  filter(measure_id == "OBESITY") |>
  select(location_id, data_value) |>
  rename(fips = location_id, obesity_pct = data_value) |>
  filter(!is.na(obesity_pct))

nrow(obesity_df)
summary(obesity_df$obesity_pct)

# Extract socioeconomic predictors from CHR
chr_clean <- chr_raw |>
  select(
    x5_digit_fips_code,
    poverty_pct        = children_in_poverty_raw_value,
    median_income      = median_household_income_raw_value,
    hs_completion_pct  = high_school_completion_raw_value
  ) |>
  mutate(
    poverty_pct       = as.numeric(poverty_pct),
    median_income     = as.numeric(median_income),
    hs_completion_pct = as.numeric(hs_completion_pct)
  )

# CHR usually stores rates as 0-1 (proportions), but always check.
cat("Range of hs_completion_pct:\n")
print(summary(chr_clean$hs_completion_pct))

# If max <= 1, treat as proportion. Otherwise treat as percentage on 0-100.
hs_max <- max(chr_clean$hs_completion_pct, na.rm = TRUE)
chr_clean <- chr_clean |>
  mutate(
    no_hs_diploma = if (hs_max <= 1) 1 - hs_completion_pct else 100 - hs_completion_pct
  ) |>
  select(-hs_completion_pct)

head(chr_clean)
summary(chr_clean)

# Standardize FIPS codes
chr_clean <- chr_clean |> rename(fips = x5_digit_fips_code)
head(obesity_df$fips)
head(chr_clean$fips)

# Join the two datasets
analysis_df <- obesity_df |>
  left_join(chr_clean, by = "fips")

nrow(analysis_df)
sum(is.na(analysis_df$poverty_pct))

# Drop any rows with missing values in modeling columns
model_df <- analysis_df |>
  drop_na(obesity_pct, poverty_pct, median_income, no_hs_diploma)

cat("Counties in final analysis dataset:", nrow(model_df), "\n")
colSums(is.na(model_df[, c("obesity_pct", "poverty_pct",
                           "median_income", "no_hs_diploma")]))

write_csv(model_df, "output/model_ready_data.csv")

# STEP 5: EXPLORATORY SCATTERPLOTS

p1 <- ggplot(model_df, aes(x = poverty_pct, y = obesity_pct)) +
  geom_point(alpha = 0.25, color = "#378ADD", size = 1.2) +
  geom_smooth(method = "lm", color = "#A32D2D", se = TRUE, linewidth = 1) +
  labs(title = "Poverty Rate vs. Adult Obesity by County",
       x = "Poverty Rate (%)", y = "Obesity Rate (%)") +
  theme_minimal(base_size = 13)
ggsave("plots/02_poverty_vs_obesity.png", p1, width = 7, height = 5, dpi = 300)

p2 <- ggplot(model_df, aes(x = median_income, y = obesity_pct)) +
  geom_point(alpha = 0.25, color = "#1D9E75", size = 1.2) +
  geom_smooth(method = "lm", color = "#A32D2D", se = TRUE, linewidth = 1) +
  scale_x_continuous(labels = label_dollar(scale = 1/1000, suffix = "K")) +
  labs(title = "Median Household Income vs. Adult Obesity by County",
       x = "Median Household Income", y = "Obesity Rate (%)") +
  theme_minimal(base_size = 13)
ggsave("plots/03_income_vs_obesity.png", p2, width = 7, height = 5, dpi = 300)

p3 <- ggplot(model_df, aes(x = no_hs_diploma, y = obesity_pct)) +
  geom_point(alpha = 0.25, color = "#D85A30", size = 1.2) +
  geom_smooth(method = "lm", color = "#A32D2D", se = TRUE, linewidth = 1) +
  labs(title = "% Without HS Diploma vs. Adult Obesity by County",
       x = "Adults Without HS Diploma (%)", y = "Obesity Rate (%)") +
  theme_minimal(base_size = 13)
ggsave("plots/04_education_vs_obesity.png", p3, width = 7, height = 5, dpi = 300)

# STEP 6: CHECK MULTICOLLINEARITY

predictor_cors <- model_df |>
  select(poverty_pct, median_income, no_hs_diploma) |>
  cor(use = "complete.obs")

print(round(predictor_cors, 3))

png("plots/05_correlation_matrix.png", width = 1000, height = 800, res = 150)
corrplot(predictor_cors,
         method = "color", type = "upper", addCoef.col = "black",
         tl.cex = 0.9, number.cex = 0.9,
         title = "Predictor Correlation Matrix",
         mar = c(0, 0, 2, 0))
dev.off()

# STEP 7: TRAIN/TEST SPLIT

n_total   <- nrow(model_df)
train_idx <- sample(seq_len(n_total), size = floor(0.8 * n_total))

train_df <- model_df[train_idx, ]
test_df  <- model_df[-train_idx, ]

cat("Training set size:", nrow(train_df), "\n")
cat("Test set size:    ", nrow(test_df),  "\n")

# STEP 8: BUILD THE MULTIPLE LINEAR REGRESSION MODEL (on TRAIN)

model <- lm(
  obesity_pct ~ poverty_pct + median_income + no_hs_diploma,
  data = train_df
)

summary(model)

model_coefs <- tidy(model)
print(model_coefs)
model_stats <- glance(model)
print(model_stats)

write_csv(model_coefs, "output/model_coefficients.csv")
write_csv(model_stats, "output/model_statistics.csv")

# STEP 9: VARIANCE INFLATION FACTOR

vif_values <- vif(model)
print(vif_values)

vif_df <- data.frame(predictor = names(vif_values), vif = vif_values)

ggplot(vif_df, aes(x = reorder(predictor, vif), y = vif)) +
  geom_col(fill = "#378ADD", width = 0.5) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "#A32D2D") +
  annotate("text", x = 0.6, y = 5.3, label = "VIF = 5 threshold",
           color = "#A32D2D", size = 3.5, hjust = 0) +
  coord_flip() +
  labs(title = "Variance Inflation Factors", x = NULL, y = "VIF") +
  theme_minimal(base_size = 13)

ggsave("plots/06_vif.png", width = 7, height = 4, dpi = 300)

# STEP 10: DIAGNOSTIC PLOTS

png("plots/07_regression_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
dev.off()

bp_test <- bptest(model)
print(bp_test)

# STEP 11: STANDARDIZED COEFFICIENTS

train_df_scaled <- train_df |>
  mutate(across(c(obesity_pct, poverty_pct, median_income, no_hs_diploma),
                ~ as.numeric(scale(.))))

model_scaled <- lm(
  obesity_pct ~ poverty_pct + median_income + no_hs_diploma,
  data = train_df_scaled
)
summary(model_scaled)

std_coefs <- tidy(model_scaled) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = case_when(
      term == "poverty_pct"   ~ "Poverty Rate",
      term == "median_income" ~ "Median Income",
      term == "no_hs_diploma" ~ "No HS Diploma",
      TRUE                    ~ term
    ),
    direction = ifelse(estimate > 0, "Positive", "Negative")
  )

print(std_coefs)
write_csv(std_coefs, "output/standardized_coefficients.csv")

p_coef <- ggplot(std_coefs,
                 aes(x = reorder(term, abs(estimate)),
                     y = estimate, fill = direction)) +
  geom_col(width = 0.55) +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error),
                width = 0.15, color = "gray40") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#378ADD",
                               "Negative" = "#D85A30"),
                    name = "Direction") +
  labs(title = "Standardized Regression Coefficients",
       subtitle = "Relative strength of each predictor on obesity rate",
       x = NULL, y = "Standardized Coefficient (β)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("plots/08_standardized_coefficients.png", p_coef,
       width = 7, height = 5, dpi = 300)


# STEP 12: RANDOM FOREST (machine-learning model)

# Hyperparameters: ntree = 500 (standard), mtry defaults to p/3 for regression,
# importance = TRUE so we get the permutation importance scores.

rf_model <- randomForest(
  obesity_pct ~ poverty_pct + median_income + no_hs_diploma,
  data       = train_df,
  ntree      = 500,
  importance = TRUE
)

print(rf_model)

# Variable importance (% IncMSE = how much MSE rises if a variable is permuted
# at random — the bigger the rise, the more the model relies on that variable).
rf_imp <- importance(rf_model) |>
  as.data.frame() |>
  rownames_to_column("predictor") |>
  arrange(desc(`%IncMSE`))

print(rf_imp)
write_csv(rf_imp, "output/rf_variable_importance.csv")

p_rf <- ggplot(rf_imp,
               aes(x = reorder(predictor, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "#7B5EA7", width = 0.55) +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       subtitle = "Higher %IncMSE = more important predictor",
       x = NULL, y = "% Increase in MSE when permuted") +
  theme_minimal(base_size = 13)

ggsave("plots/09_rf_variable_importance.png", p_rf,
       width = 7, height = 5, dpi = 300)

# STEP 13: EVALUATE BOTH MODELS ON THE HELD-OUT TEST SET

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae  <- function(actual, predicted) mean(abs(actual - predicted))
r2   <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - ss_res / ss_tot
}

lm_test_pred <- predict(model,    newdata = test_df)
rf_test_pred <- predict(rf_model, newdata = test_df)

actual <- test_df$obesity_pct

comparison <- tibble(
  Model     = c("Linear Regression", "Random Forest"),
  Test_R2   = c(r2(actual,   lm_test_pred), r2(actual,   rf_test_pred)),
  Test_RMSE = c(rmse(actual, lm_test_pred), rmse(actual, rf_test_pred)),
  Test_MAE  = c(mae(actual,  lm_test_pred), mae(actual,  rf_test_pred))
)

cat("\n--- Test-set model comparison ---\n")
print(comparison)
write_csv(comparison, "output/model_comparison.csv")

comparison_long <- comparison |>
  pivot_longer(cols = c(Test_R2, Test_RMSE, Test_MAE),
               names_to = "Metric", values_to = "Value") |>
  mutate(Metric = case_when(
    Metric == "Test_R2"   ~ "R² (higher = better)",
    Metric == "Test_RMSE" ~ "RMSE (lower = better)",
    Metric == "Test_MAE"  ~ "MAE (lower = better)",
    TRUE ~ Metric
  ))

p_compare <- ggplot(comparison_long,
                    aes(x = Model, y = Value, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.4, size = 4) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c("Linear Regression" = "#378ADD",
                               "Random Forest"     = "#7B5EA7")) +
  labs(title = "Linear Regression vs. Random Forest on the Held-Out Test Set",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0))

ggsave("plots/10_model_comparison.png", p_compare,
       width = 9, height = 5, dpi = 300)

# STEP 14: SIDE-BY-SIDE VARIABLE IMPORTANCE COMPARISON

lm_importance <- std_coefs |>
  mutate(metric = abs(estimate)) |>
  select(predictor = term, lm_importance = metric)

rf_importance <- rf_imp |>
  mutate(predictor = case_when(
    predictor == "poverty_pct"   ~ "Poverty Rate",
    predictor == "median_income" ~ "Median Income",
    predictor == "no_hs_diploma" ~ "No HS Diploma",
    TRUE ~ predictor
  )) |>
  select(predictor, rf_importance = `%IncMSE`)

importance_compare <- lm_importance |>
  inner_join(rf_importance, by = "predictor") |>
  mutate(
    lm_norm = lm_importance / max(lm_importance),
    rf_norm = rf_importance / max(rf_importance)
  ) |>
  select(predictor, "Linear (|β|)" = lm_norm, "Random Forest (%IncMSE)" = rf_norm) |>
  pivot_longer(cols = -predictor, names_to = "Model", values_to = "Importance")

print(importance_compare)
write_csv(importance_compare, "output/importance_comparison.csv")

p_imp_compare <- ggplot(importance_compare,
                        aes(x = reorder(predictor, Importance),
                            y = Importance, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("Linear (|β|)"            = "#378ADD",
                               "Random Forest (%IncMSE)" = "#7B5EA7")) +
  labs(title = "Do both models agree on which predictor matters most?",
       subtitle = "Each metric scaled to its own maximum (1.0 = most important predictor in that model)",
       x = NULL, y = "Relative importance") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("plots/11_importance_comparison.png", p_imp_compare,
       width = 8, height = 5, dpi = 300)

# STEP 15: ACTUAL VS. PREDICTED (both models, on the test set)

test_results <- test_df |>
  mutate(
    lm_pred     = lm_test_pred,
    rf_pred     = rf_test_pred,
    lm_residual = obesity_pct - lm_pred,
    rf_residual = obesity_pct - rf_pred
  )

# Long format for facet plotting
test_long <- test_results |>
  select(fips, obesity_pct, lm_pred, rf_pred) |>
  pivot_longer(cols = c(lm_pred, rf_pred),
               names_to = "Model", values_to = "Predicted") |>
  mutate(Model = case_when(
    Model == "lm_pred" ~ "Linear Regression",
    Model == "rf_pred" ~ "Random Forest",
    TRUE ~ Model
  ))

p_fit <- ggplot(test_long, aes(x = Predicted, y = obesity_pct, color = Model)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              color = "#A32D2D", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Model) +
  scale_color_manual(values = c("Linear Regression" = "#378ADD",
                                "Random Forest"     = "#7B5EA7")) +
  labs(title = "Actual vs. Predicted Obesity Rate (test set)",
       subtitle = "Dashed line = perfect prediction; tighter clusters = better fit",
       x = "Predicted Obesity Rate (%)",
       y = "Actual Obesity Rate (%)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("plots/12_actual_vs_predicted_both.png", p_fit,
       width = 10, height = 5, dpi = 300)

p_resid <- ggplot(test_results, aes(x = lm_residual)) +
  geom_histogram(bins = 40, fill = "#1D9E75", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, color = "#A32D2D", linetype = "dashed") +
  labs(title = "Distribution of Linear Regression Residuals (test set)",
       x = "Residual (Actual − Predicted)", y = "Count") +
  theme_minimal(base_size = 13)

ggsave("plots/13_residuals_distribution.png", p_resid,
       width = 7, height = 5, dpi = 300)

# STEP 16: WRITE UP RESULTS

cat("\n============================================================\n")
cat("MODEL RESULTS SUMMARY\n")
cat("============================================================\n\n")

cat("LINEAR REGRESSION (training set)\n")
cat(sprintf("  R-squared:           %.3f\n",  glance(model)$r.squared))
cat(sprintf("  Adjusted R-squared:  %.3f\n",  glance(model)$adj.r.squared))
cat(sprintf("  F-statistic p-value: %.4f\n",  glance(model)$p.value))

cat("\nLINEAR REGRESSION (test set)\n")
cat(sprintf("  R²:    %.3f\n",  comparison$Test_R2[1]))
cat(sprintf("  RMSE:  %.3f\n",  comparison$Test_RMSE[1]))
cat(sprintf("  MAE:   %.3f\n",  comparison$Test_MAE[1]))

cat("\nRANDOM FOREST (test set)\n")
cat(sprintf("  R²:    %.3f\n",  comparison$Test_R2[2]))
cat(sprintf("  RMSE:  %.3f\n",  comparison$Test_RMSE[2]))
cat(sprintf("  MAE:   %.3f\n",  comparison$Test_MAE[2]))

cat("\n--- Coefficient interpretations (linear model) ---\n\n")
coefs <- coef(model)

cat(sprintf(
  "Poverty Rate: a 1-percentage-point increase in poverty is associated\n  with a %.2f-percentage-point change in obesity rate, holding other\n  variables constant.\n\n",
  coefs["poverty_pct"]
))
cat(sprintf(
  "Median Income: a $1,000 increase in median household income is\n  associated with a %.4f-percentage-point change in obesity rate,\n  holding other variables constant.\n\n",
  coefs["median_income"] * 1000
))
cat(sprintf(
  "No HS Diploma: a 1-percentage-point increase in adults without a\n  HS diploma is associated with a %.2f-percentage-point change in\n  obesity rate, holding other variables constant.\n\n",
  coefs["no_hs_diploma"]
))
cat("============================================================\n\n")

# STEP 17: SAVE FINAL OUTPUTS

# Full predictions for the test set
full_results <- test_results |>
  select(fips, obesity_pct,
         poverty_pct, median_income, no_hs_diploma,
         lm_pred, rf_pred,
         lm_residual, rf_residual)

write_csv(full_results, "output/full_results_with_predictions.csv")

saveRDS(model,    "output/lm_model.rds")
saveRDS(rf_model, "output/rf_model.rds")

cat("All outputs saved to output/ and plots/ folders.\n")
cat("Script complete.\n")
