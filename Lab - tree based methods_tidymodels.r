library(tidymodels)   
library(ISLR2)        
library(rpart.plot)   
library(bonsai) 


## ------------- Regression Trees
# We will use the Major League Baseball Data for the demonstration of regression trees. In this example, we are interested in predicting the log-transformed salary.


hitters <- ISLR2::Hitters |>
  dplyr::filter(!is.na(Salary)) |>
  dplyr::mutate(lgSalary = log(Salary))

hitters_simple <- hitters |>
  dplyr::select(lgSalary, Years, Hits)

hitters2 <- hitters |>
  dplyr::select(lgSalary, Years, Hits, HmRun, RBI, PutOuts, Walks, Runs)


reg_tree_spec <- decision_tree(
  mode = "regression",
  cost_complexity = 1e-8,      # basically no penality
  tree_depth = 15,
  min_n = 5) |>
  set_engine("rpart")

reg_tree_fit <- reg_tree_spec |>
  fit(lgSalary ~ Years + Hits, data = hitters_simple)

# plot the regression tree
reg_tree_engine <- extract_fit_engine(reg_tree_fit)
rpart.plot::rpart.plot(reg_tree_engine, main = "Regression tree", roundint = F)

# max number of terminal nodes
reg_tree_engine$cptable
max(reg_tree_engine$cptable[, "nsplit"]) + 1

# prune the tree. Only keep 3 terminal nodes
reg_cp <- as.data.frame(reg_tree_engine$cptable)
print(reg_cp[, c("CP", "nsplit", "xerror")])

which_cp <- which(reg_cp$nsplit == 2)
reg_pruned <- rpart::prune(reg_tree_engine, cp = reg_cp$CP[which_cp])
rpart.plot::rpart.plot(reg_pruned, main = "Regression tree after pruning", roundint = F)


## Cost complexity pruning of regression trees through CV

set.seed(42)
reg_train_id <- sample(seq_len(nrow(hitters2)), size = 132, replace = F)
reg_train <- hitters2[reg_train_id, ]
reg_test  <- hitters2[-reg_train_id, ]

set.seed(123)
reg_folds <- vfold_cv(reg_train, v = 6)

# prepare the tidymodel workflow for tuning

reg_cv_spec <- decision_tree(
  mode = "regression",
  cost_complexity = tune(),  # subject to tuning
  tree_depth = 15,           # fixed during tuning
  min_n = 5                  # fixed during tuning
) |>
  set_engine("rpart")

reg_cv_wf <- workflow() |>
  add_formula(lgSalary ~ .) |>
  add_model(reg_cv_spec)

# define the grid for cp
reg_cp_grid <- tibble(cost_complexity = 10^seq(-5, -0.5, length.out = 25))

# conduct grid search
set.seed(42)
reg_cv_results <- tune_grid(
  reg_cv_wf,
  resamples = reg_folds,
  grid = reg_cp_grid,
  metrics = metric_set(mse, rmse),
  control = control_grid(save_pred = T))

reg_cv_metrics <- collect_metrics(reg_cv_results)

ggplot(reg_cv_metrics |> dplyr::filter(.metric == "mse"),
       aes(x = cost_complexity, y = mean)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(title = "6-fold CV", x = "log cp",
       y = "Cross-validated MSE")

# selected cp value 
(reg_best_cp <- select_best(reg_cv_results, metric = "mse"))

# final fitted model
reg_best_fit <- finalize_workflow(reg_cv_wf, reg_best_cp) |>
  fit(data = reg_train)

rpart.plot::rpart.plot(extract_fit_engine(reg_best_fit),
                       main = "CV-selected regression tree", roundint = F)


# test MSE
pred <- predict(reg_best_fit, new_data = reg_test)$.pred
(test_mse <- mean((reg_test$lgSalary - pred)^2))



## ------------ Classification Trees

heart_url <- "https://raw.githubusercontent.com/JWarmenhoven/ISLR-python/master/Notebooks/Data/Heart.csv"

heart <- readr::read_csv(heart_url, show_col_types = F) |>
  dplyr::select(-1) |>  # remove CSV row-number column (not a predictor)
  dplyr::mutate(
    AHD = factor(AHD, levels = c("No", "Yes")),
    ChestPain = factor(ChestPain),
    Thal = factor(Thal)
  ) |>
  tidyr::drop_na()


set.seed(124)
heart_train_id <- sample(seq_len(nrow(heart)), size = 150, replace = F)
heart_train <- heart[heart_train_id, ]
heart_test  <- heart[-heart_train_id, ]


# rpart can use Gini or information gain
heart_tree_spec <- decision_tree(
  mode = "classification",
  cost_complexity = 1e-8,
  tree_depth = 15,
  min_n = 5) |>
  set_engine("rpart", parms = list(split = "information"))

heart_tree_fit <- heart_tree_spec |>
  fit(AHD ~ ., data = heart_train)

heart_tree_engine <- extract_fit_engine(heart_tree_fit)

rpart.plot::rpart.plot(heart_tree_engine, extra = 104,
                       main = "classification tree", roundint = F)


# cross validation 

set.seed(124)
heart_folds <- vfold_cv(heart_train, v = 10, strata = AHD)

# prepare the workflow
heart_cv_spec <- decision_tree(
  mode = "classification",
  cost_complexity = tune(),
  tree_depth = 15,
  min_n = 5) |>
  set_engine("rpart", parms = list(split = "information"))

heart_cv_wf <- workflow() |>
  add_formula(AHD ~ .) |>
  add_model(heart_cv_spec)

# define the grid for search
heart_cp_grid <- tibble(cost_complexity = 10^seq(-5, -0.5, length.out = 25))

# conduct grid search
set.seed(1212)
heart_cv <- tune_grid(
  heart_cv_wf,
  resamples = heart_folds,
  grid = heart_cp_grid,
  metrics = metric_set(mn_log_loss, accuracy),
  control = control_grid(save_pred = T))

heart_cv_metrics <- collect_metrics(heart_cv)

ggplot(heart_cv_metrics |> dplyr::filter(.metric == "mn_log_loss"),
       aes(x = cost_complexity, y = mean)) +
  geom_line() + geom_point() + scale_x_log10() +
  labs(title = "CV log-loss", x = "log cp", y = "Log loss")


ggplot(heart_cv_metrics |> dplyr::filter(.metric == "accuracy"),
       aes(x = cost_complexity, y = 1 - mean)) +
  geom_line() + geom_point() + scale_x_log10() +
  labs(title = "CV misclassification rate", x = "log cp",
       y = "Misclassification rate")

# select model complexity 
(heart_best_accuracy <- select_best(heart_cv, metric = "accuracy"))
(heart_best_logloss <- select_best(heart_cv, metric = "mn_log_loss"))

# final fitted model
heart_cv_fit <- finalize_workflow(heart_cv_wf, heart_best_accuracy) |>
  fit(data = heart_train)

# Test performance
class_preds <- predict(heart_cv_fit, new_data = heart_test, type = "class")
prob_preds <- predict(heart_cv_fit, new_data = heart_test, type = "prob")

heart_pred <- cbind(heart_test, class_preds, prob_preds)

accuracy(heart_pred, truth = AHD, estimate = .pred_class)
mn_log_loss(heart_pred, truth = AHD, .pred_No)    # first level is coded No



## ---------- Bagging

set.seed(234)
forest_train_id <- sample(seq_len(nrow(hitters2)), size = 132, replace = F)
forest_train <- hitters2[forest_train_id, ]
forest_test  <- hitters2[-forest_train_id, ]

# prepare the workflow 
bag_rf_spec <- rand_forest(
  mode = "regression",
  mtry = 7,       # all 7 predictors == bagging
  trees = 500,   
  min_n = 5) |>
  set_engine("randomForest", importance = T, replace = T)

set.seed(234)
bag_rf_fit <- fit(bag_rf_spec, lgSalary ~ ., data = forest_train)
plot(extract_fit_engine(bag_rf_fit), main = "Bagging: OOB error")


# alternative engine, ranger

bag_ranger_spec <- rand_forest(
  mode = "regression",
  mtry = 7,
  trees = 500,
  min_n = 5) |>
  set_engine("ranger", importance = "impurity", replace = T,
             sample.fraction = 1, seed = 234)

bag_ranger_fit <- fit(bag_ranger_spec, lgSalary ~ ., data = forest_train)
extract_fit_engine(bag_ranger_fit)$prediction.error

bag_ranger_fit



## ---------- random forest 

rf_rf_spec <- rand_forest(
  mode = "regression",
  mtry = 3,
  trees = 500,
  min_n = 5
) |>
  set_engine("randomForest", importance = T, replace = T)

set.seed(234)
rf_rf_fit <- fit(rf_rf_spec, lgSalary ~ ., data = forest_train)


# use ranger instead 
rf_ranger_spec <- rand_forest(
  mode = "regression",
  mtry = 3,
  trees = 500,
  min_n = 5
) |>
  set_engine("ranger", importance = "impurity", replace = T,
             sample.fraction = 1, seed = 234)

rf_ranger_fit <- fit(rf_ranger_spec, lgSalary ~ ., data = forest_train)
print(rf_ranger_fit)

# test results, altogether 
data.frame(
  model = c("Bagging: randomForest", "Bagging: ranger",
            "RF: randomForest", "RF: ranger"),
  holdout_MSE = c(
    mean((forest_test$lgSalary - predict(bag_rf_fit, forest_test)$.pred)^2),
    mean((forest_test$lgSalary - predict(bag_ranger_fit, forest_test)$.pred)^2),
    mean((forest_test$lgSalary - predict(rf_rf_fit, forest_test)$.pred)^2),
    mean((forest_test$lgSalary - predict(rf_ranger_fit, forest_test)$.pred)^2)))


## --------- variable importance 

rf_imp <- randomForest::importance(extract_fit_engine(rf_rf_fit))
rf_impurity <- sort(rf_imp[, "IncNodePurity"], decreasing = T)
barplot(rf_impurity, horiz = T, las = 1,
        main = "Impurity importance (randomForest)")


ranger_impurity <- sort(ranger::importance(extract_fit_engine(rf_ranger_fit)),
                        decreasing = T)
barplot(ranger_impurity, horiz = T, las = 1,
        main = "Impurity importance (ranger)")


