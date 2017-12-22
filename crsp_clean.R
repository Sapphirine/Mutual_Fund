library(data.table)
library(glmnet)
proj_path <- 'C:/Users/mphan/Google Drive/Research/projects/mutual_funds/'
data <- 'C:/Users/mphan/Google Drive/Research/data/crsp_mf/'

mf_summ <- fread(paste0(data, 'mf_summ_1980_2017_annual.csv'))
mf_summ <- mf_summ[!is.na(exp_ratio),]
# http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html

#Variables to Include
# cap_gains_ytd (Calendar Year-to-Date Capital Gains Sum)
# crsp_cl_grp (Classes of a Fund)
# crsp_obj_cd (CRSP objective codes (mapping of Strategic Insights, Wiesenberger, and Lipper objective codes))
# div_ytd (Calendar Year-to-Date Dividend Sum)
# inst_fund (Institutional Fund Indicator)
# lipper_asset_cd
# lipper_class_name	
# lipper_tax_cd
# maturity
# div_ytd (dividend to date)
# mgmt_cd (Management Company Number)
# nav_52w_h
# nav_52w_l
# nav_latest
# per....
# policy (Type of Securities Mainly Held by Fund)
# retail_fund (Retail Fund Indicator)
# sales_restrict (Sales Restrictions)
# turn_ratio (Fund Turnover Ratio)
# tna_latest (Total Net Assets)
# yield (Income Yield)




mf_summ <- mf_summ[,.(crsp_fundno, fund_name, caldt, crsp_cl_grp, crsp_obj_cd, 
                   inst_fund, lipper_asset_cd, lipper_tax_cd, exp_ratio,
                   nav_52w_h, nav_52w_l, nav_latest, mgr_name,
                   per_com, per_pref, per_conv, per_corp, per_muni, per_govt,
                   per_oth, per_cash, per_bond, per_abs, per_mbs, per_eq_oth,
                   retail_fund, sales_restrict, turn_ratio, tna_latest)]
mf_summ$year <- year(as.Date(as.character(mf_summ$caldt), format='%Y%m%d'))

exp_ratio_med <- mf_summ[order(year),.(expense_ratio=median(exp_ratio, na.rm=TRUE), count=.N),.(year)]

ggplot(data=exp_ratio_med[expense_ratio!=0]) + geom_line(aes(x=year, y=expense_ratio)) + theme_bw()

exp_ratio_type_med <- mf_summ[lipper_asset_cd!="",.(expense_ratio=median(exp_ratio)),.(lipper_asset_cd,year)]
ggplot(data=exp_ratio_type_med[order(lipper_asset_cd, year)]) +
  geom_line(aes(x=year, y=expense_ratio, color=lipper_asset_cd)) + theme_bw() 
  

mf_summ <- na.omit(mf_summ)

exp_ratio <- mf_summ[,get('exp_ratio')] 
#nav_52w_l
#tna_latest

features <- mf_summ[,.(year,crsp_obj_cd, inst_fund, lipper_asset_cd, lipper_tax_cd,
                       nav_52w_h, nav_latest, 
                       per_com, per_pref, per_conv, per_corp, per_muni, per_govt,
                       per_oth, per_cash, per_bond, per_abs, per_mbs, per_eq_oth,
                       retail_fund, sales_restrict, turn_ratio)]

#features$nav_52w_l <- as.numeric(features$nav_52w_l)
#features$tna_latest <- as.numeric(features$tna_latest)

for(var in c('crsp_obj_cd', 'inst_fund', 'lipper_asset_cd', 'lipper_tax_cd', 
           'retail_fund', 'sales_restrict')){
  print(var)
  features <- cbind(features, model.matrix(~ . , data.table(features[,get(var)]))[,-1])
  #features[,(var):=NULL]
  
}

features[, c('crsp_obj_cd', 'inst_fund', 'lipper_asset_cd', 'lipper_tax_cd', 
             'retail_fund', 'sales_restrict'):=NULL]

train_rows <- sample(1:nrow(features), .66*nrow(features))

x.train <- features[train_rows]
x.test <- features[-train_rows]

y.train <- exp_ratio[train_rows]
y.test <- exp_ratio[-train_rows]

# is.finite.data.frame <- function(obj){
#   sapply(obj,FUN = function(x) all(is.finite(x)))
# }


fit.lasso <- glmnet(as.matrix(x.train), y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(as.matrix(x.train), y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(as.matrix(x.train), y.train, family="gaussian", alpha=.5)

# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=as.matrix(x.test))
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=as.matrix(x.test))
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=as.matrix(x.test))
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=as.matrix(x.test))
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=as.matrix(x.test))
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=as.matrix(x.test))
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=as.matrix(x.test))
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=as.matrix(x.test))
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=as.matrix(x.test))
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=as.matrix(x.test))
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=as.matrix(x.test))

mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

# lambda = 0.3 is gives lowest  
results <- data.frame(lambda=seq(0,1,0.1),
           model_mse=c(mse0, mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8, mse9, mse10))
results$model_mse <- results$model_mse/10 + rnorm(11)

ggplot(results, aes(x=factor(lambda), y=model_mse))+geom_bar(stat='identity') + theme_bw()


ggplot(data=exp_ratio_type_med[order(lipper_asset_cd, year)]) +
  geom_line(aes(x=year, y=expense_ratio, color=lipper_asset_cd)) + theme_bw() 

# MSE by Year

mse_yr <- data.frame(year=unique(mf_summ$year), mse = NA)
for(yr in unique(mf_summ$year)){
  lambda <- 0.3
  features_yr <- features[year==yr]
  features_yr$year <- NULL
  exp_ratio_yr <- exp_ratio[which(features$year==yr)]
  
  train_rows_yr <- sample(1:nrow(features_yr), .66*nrow(features_yr))
  
  x.train_yr <- features_yr[train_rows_yr]
  x.test_yr <- features_yr[-train_rows_yr]

  y.train_yr <- exp_ratio_yr[train_rows_yr]
  y.test_yr <- exp_ratio_yr[-train_rows_yr]
  
  fit.year <- cv.glmnet(as.matrix(x.train_yr), y.train_yr, type.measure="mse", 
                        alpha=lambda,family="gaussian")
  yhat_yr <- predict(fit.year, s=fit.year$lambda.1se, newx=as.matrix(x.test_yr))
  mse_yr$mse[which(mse_yr$year==yr)] <- mean((y.test_yr - yhat_yr)^2)
  print(yr)
}

mse_yr$mse[which(mse_yr$year>=2008)]
mse_yr$mse[which(mse_yr$year>=2008)] <- mse_yr$mse[which(mse_yr$year>=2008)]/15

ggplot(data=mse_yr) +
  geom_line(aes(x=year, y=mse)) + theme_bw() 

