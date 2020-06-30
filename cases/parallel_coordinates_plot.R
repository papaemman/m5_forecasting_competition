#################################
#                               #
#   Visualize hyperparamters    #
#                               #
#################################

# Load packages
library(plotly)


# Load dataset
optObj <- readRDS("final_submission/hyperparameter_tuning_results/optObj_dept_FOODS_1.rds")

# Correlations and plots
score_results <- optObj$scoreSummary 


## 1. Parallel coordinates plot

glimpse(score_results)
colnames(score_results)

fig <- score_results %>%
  plot_ly(width = 1500, height = 600) 

fig <- fig %>% add_trace(type = 'parcoords',
                         
                         line = list(color = ~ rmse,
                                     colorscale = 'Jet',
                                     showscale = TRUE,
                                     reversescale = TRUE
                         ),
                         
                         dimensions = list(
                           
                           
                           # Epoch
                           list(tickvals = score_results$Epoch %>% unique(),
                                ticktext = score_results$Epoch %>% unique() ,
                                label = 'Epoch', values = ~Epoch),
                           
                           # learning_rate
                           list(range = c(~min(learning_rate),~max(learning_rate)),
                                label = 'Learning rate', values = ~learning_rate),
                           
                           # num_leaves
                           list(range = c(~min(num_leaves), ~max(num_leaves)),
                                label = 'num_leaves', values = ~num_leaves),
                           
                           # max_depth
                           list(range = c(~min(max_depth), ~max(max_depth)),
                                label = 'max_depth', values = ~max_depth),
                           
                           # min_data_in_leaf
                           list(range = c(~min(min_data_in_leaf), ~max(min_data_in_leaf)),
                                label = 'min_data_in_leaf', values = ~min_data_in_leaf),
                           
                           # bagging_fraction
                           list(range = c(~min(bagging_fraction), ~max(bagging_fraction)),
                                label = 'bagging_fraction', values = ~bagging_fraction),
                           
                           # bagging_freq
                           list(range = c(~min(bagging_freq), ~max(bagging_freq)),
                                label = 'bagging_freq', values = ~bagging_freq),
                           
                           # feature_fraction
                           list(range = c(~min(feature_fraction), ~max(feature_fraction)),
                                label = 'feature_fraction', values = ~feature_fraction),
                           
                           # feature_fraction_bynode
                           list(range = c(~min(feature_fraction_bynode), ~max(feature_fraction_bynode)),
                                label = 'feature_fraction_bynode', values = ~feature_fraction_bynode),
                           
                           # lambda_l1
                           list(range = c(~min(lambda_l1), ~max(lambda_l1)),
                                label = 'lambda_l1', values = ~lambda_l1),
                           
                           # lambda_l2
                           list(range = c(~min(lambda_l2), ~max(lambda_l2)),
                                label = 'lambda_l2', values = ~lambda_l2),
                           
                           # tweedie_variance_power
                           list(range = c(~min(tweedie_variance_power), ~max(tweedie_variance_power)),
                                label = 'tweedie_variance_power', values = ~tweedie_variance_power),
                           
                           # Score
                           list(range = c(~min(rmse), ~max(rmse)),
                                label = 'RMSE score', values = ~rmse)
                           
                         )
)


fig

DT::datatable(score_results)


## 2. Scatter plots

p <- ggplot(score_results) + geom_point(aes(max_depth, rmse))
plotly::ggplotly(p)
