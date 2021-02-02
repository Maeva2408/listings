#########################################################################################
#  Maeva's Data Analysis
# Assignment 1 DA3 : Predicting Airbnb Apartment Prices : Sydney
#########################################################################################



# ------------------------------------------------------------------------------------------------------

# CLEAR MEMORY
rm(list=ls())

# Library
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(ranger)
library(Hmisc)
library(kableExtra)
library(huxtable)

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

dir<-"Data"

#location folders
data_in  <- paste0(dir,"/Clean/")
data_out <- paste0(dir,"/Clean/")

output <- paste0(dir,"/output/")
create_output_if_doesnt_exist(output)

options(digits = 3)



########################################
# PART I. Load DATA                    #
########################################


# Used area
area <- "SydneyB"
data <-
  read_csv(paste0(data_in, "airbnb_", area, "_neighbour_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)


######################
# Quick look at data #
######################
glimpse(data)
skim(data)

#################################################
# Business logic- define our prediction problem #
#################################################

# that's gonna be our sample
skimr::skim(data)

# save workfile
write.csv(data, paste0(data_out, "airbnb_sydneyB_neigbhour_work.csv"), row.names = F)


#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))


summary <- data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

pander(summary)

Hmisc::describe(data$price)


# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price<400)


# Distribution of price by type below 400

# Histograms
# price
Price.fig1 <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  #  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (AUD dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
Price.fig1
save_fig("Price.fig1", output, "small")


## Boxplot of price by room type
Room.fig2 <- ggplot(data = datau, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c(color[2],color[1], color[3]), fill = c(color[2],color[1], color[3]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (AUD dollars)")+
  theme_bg()
Room.fig2
save_fig("Room.fig2", output, "small")

# Boxplot
Property.fig3 <- ggplot(datau, aes(x = factor(accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1])) +
  labs(x = "Accomodates (Persons)",y = "Price (AUD dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
Property.fig3
save_fig("Property.fig3", output, "small")


########################################
# PART II. Setting up models           #
########################################

#######################
# Choice of variables #
#######################

# Basic Variables
basic_lev  <- c("accommodates", "beds", "f_property_type", "f_room_type")

# other variables
facilities <- c("bathrooms", "bedrooms", "d_kitchen", "d_gardenorbackyard")
Accessibility <- c("f_minimum_nights", "d_freeparking_agg")
reviews <- c("f_number_of_reviews","review_scores_rating", "flag_review_scores_rating")

# create dummy vars
dummies <- names(data)[seq(27,28)]
dummies2 <- names(data)[seq(30,95)]
dummies <- append(dummies,dummies2)
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
data <- data %>% select(-dummies)
# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)
# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)


#################################################
# OLS
################################################

#########################
# Look for interactions #
#########################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "room_type","d_waterfront", "Room type", "waterfront")
p2 <- price_diff_by_variables2(data, "room_type","d_freeparking_agg", "Room type", "free parking")
p3 <- price_diff_by_variables2(data, "room_type", "d_gardenorbackyard", "Room type", "Garden or backyard")
p4 <- price_diff_by_variables2(data, "room_type", "d_beachfront", "Room type", "Beachfront")

#Look up property type
p5 <- price_diff_by_variables2(data, "f_minimum_nights","f_property_type", "minimum_nights" , "property_type")# add
p6 <- price_diff_by_variables2(data, "f_property_type","d_paidparking_agg", "Proprety type", "paid parking")
p7 <- price_diff_by_variables2(data,"accommodates", "f_property_type","Number of people", "proprety type")
p8 <- price_diff_by_variables2(data,"d_kitchen", "f_property_type","Number of kitchen", "proprety type")

g_interactions <- plot_grid(p1,p2,p3, p4,nrow=2, ncol=2)
g_interactions

#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("Interaction-Room",output,"verylarge")

## No need to add interaction for any of them

g_interactions2 <- plot_grid(p5,p6,p7,p8,nrow=2, ncol=2)
g_interactions2

#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("Interaction-Property",output,"verylarge")

## Add interaction ony for the minimum nights

# dummies suggested by graphs
X1  <- "f_minimum_nights*f_property_type"


# Create models in levels models: 1-7
modellev1 <- " ~ accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, facilities),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,facilities,Accessibility),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,facilities,Accessibility,reviews),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,facilities,Accessibility,reviews,X1),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,facilities,Accessibility,reviews,X1,amenities),collapse = " + "))

#################################
# Separate hold-out set         #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)

##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:7)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")
library(pander)
pander(t1)

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(85, 95), breaks = seq(85,95,2)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
save_fig("airbnb-model-result-levels-OLS_fig4",output, "small")



#################################
#           LASSO               #
#################################

# take model 7 without interactions 
vars_model_7 <- c("price", basic_lev,facilities,Accessibility,reviews,X1,amenities)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 5 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_7, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

#################
#RANDOM FORESTS #  
#################

## Predictors
# Same as my model5
predictors_1 <- c(basic_lev,facilities,Accessibility,reviews)

## same as my model7
predictors_2 <- c(basic_lev,facilities,Accessibility,reviews, amenities)



# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# Predictors_1
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_1

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2

# auto tuning first
set.seed(1234)
system.time({
rf_model_2auto <- train(
formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
data = data_train,
method = "ranger",
trControl = train_control,
importance = "impurity"
)})


rf_model_2auto
#rf_model_2auto <-rf_model_2





# evaluate random forests

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2,
    model_2b = rf_model_2auto
    
  )
)
summary(results)

# Save outputs 

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(output,"rf_tuning_modelB.tex"))


# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_2auto$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size,
  rf_model_2auto$finalModel$min.node.size
  
),
nrow=3, ncol=2,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_turning_choices.tex"))

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`),
                     mean(results$values$`model_2b~RMSE`)
),
nrow=3, ncol=1,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_rmse.tex"))



########################################
# PART III. Diagnostics
########################################



####################
# OLS : Diagnostic #
####################
model5_level <- model_results_cv[["modellev5"]][["model_work_data"]]


# look at holdout RMSE
model5_level_work_rmse <- mse_lev(predict(model5_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model5_level_holdout_rmse <- mse_lev(predict(model5_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model5_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 800, yend =800), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 800), ylim = c(0, 800)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 800), breaks=seq(0, 800, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 800), breaks=seq(0, 800, by=50)) +
  labs(y = "Price (AUD dollars)", x = "Predicted price  (AUD dollars)") +
  theme_bg() 
level_vs_pred
save_fig("level-vs-pred", output, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev5_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (AUD dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate
save_fig("ch14-figure-8b-ci-accomodate", output, "small")


##############################
# RANDOM forest ; Diagnostic #
##############################


#############################
# Variable Importance Plots # 
#############################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


#######################################
# 1) full varimp plot, above a cutoff #
#######################################

# to have a quick look
plot(varImp(rf_model_2))

cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot
#save_fig("rf_varimp1",output, "verylarge")
save_fig("rf-varimp-base",output, "verylarge")

####################################
# 2) full varimp plot, top 10 only #
####################################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot_b
#save_fig("rf_varimp1_b",output, "verylarge")
save_fig("rf-varimp-top10",output, "verylarge")


##########################
# 2) varimp plot grouped #
##########################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_minimum_nights_varnames <- grep("f_minimum_nights",varnames, value = TRUE)
f_number_of_reviews_varnames <- grep("f_number_of_reviews",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_minimum_nights=f_minimum_nights_varnames,
               f_number_of_reviews = f_number_of_reviews_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               accommodates = "accommodates",
               bathrooms = "bathrooms",
               bedrooms = "bedrooms",
               Tv = "d.hdtvtv_agg",
               beds = "beds")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=4))
rf_model_2_var_imp_grouped_plot
#save_fig("rf_varimp_grouped1",output, "small")
save_fig("rf-varimp-group",output, "small")
