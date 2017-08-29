#demand forecasting project
#created: 3/13/17
#author: Mike Vlah (vlahm13@gmail.com)

#subset training and testing sets
last_record = tail(which(!(is.na(sales$sales))), 1)
train_eval = stats::window(interp, start=index(interp)[1],
                           end=index(interp)[last_record-6])
trainlen = length(train_eval)
test_eval = stats::window(interp, start=index(interp)[last_record-6],
                          end=index(interp)[last_record])
full = stats::window(interp, start=index(interp)[1],
                     end=index(interp)[last_record])

#create a factor predictor for segments of each month
mos = substr(sales$date,6,7)
monthSection = rep(NA, length(mos))
for(i in unique(mos)){
    mo = which(mos == i)
    # ndays = length(mo)
    for(j in 1:4){
        sect = (j*6-5):(j*6)
        if(any(sect > length(monthSection[mo]))){
            monthSection[mo][is.na(monthSection[mo])] = j
            break
        } else {
            monthSection[mo][sect] = j
        }
    }
    monthSection[mo][is.na(monthSection[mo])] = 5
}
monthSection = factor(monthSection)

#put all predictors in model matrix form
# dmarket = model.matrix(~ I(scale(market))-1, data=sales)
dmonthSection = model.matrix(~ monthSection-1)
doverall = model.matrix(~ overall-1, data=sales)#[,-1])
dtemp = model.matrix(~ I(scale(midtemp))-1, data=sales)
dtempfac = model.matrix(~ tempfac-1, data=sales)#[,1:3]) #add 4 eventually
drain = model.matrix(~ I(scale(rain))-1, data=sales)
drainfac = model.matrix(~ rainfac-1, data=sales)[,2:3]
dhumid = model.matrix(~ humid-1, data=sales)[,2]
dwindy = model.matrix(~ windy-1, data=sales)[,2]
dcloudy = model.matrix(~ cloudy-1, data=sales)[,2]
dseahawks = model.matrix(~ seahawks-1, data=sales)[,1:2]
dhuskies = model.matrix(~ huskies-1, data=sales)[,1:2]

#close any old parallel threads that might be open
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}
unregister()

#specify system-dependent cluster type; ncores-1 to be used in parallel (no need to modify this)
if(.Platform$OS.type == "windows"){
    cl <- makeCluster(detectCores() - 1, type='PSOCK', outfile='')
} else {
    cl <- makeCluster(detectCores() - 1, type='FORK', outfile='')
}
registerDoParallel(cl)

#structure predictors for the model
pred_dict = data.frame('cloudy'=unname(dcloudy),'snowy'=unname(doverall[,4, drop=FALSE]),
                       'windy'=unname(dwindy),'humid'=unname(dhumid),
                       'mean_temp'=unname(dtemp),'cold'=unname(dtempfac[,1, drop=FALSE]),
                       'cool'=unname(dtempfac[,2, drop=FALSE]),
                       'warm'=unname(dtempfac[,3, drop=FALSE]),
                       'hot'=unname(dtempfac[,4, drop=FALSE]),'tot_rain'=unname(drain),
                       'no_rain'=unname(doverall[,1, drop=FALSE]),
                       'lo_rain'=unname(drainfac[,1, drop=FALSE]),
                       'hi_rain'=unname(drainfac[,2, drop=FALSE]),
                       'Seahawks_A'=unname(dseahawks[,1, drop=FALSE]),
                       'Seahawks_H'=unname(dseahawks[,2, drop=FALSE]),
                       'Huskies_A'=unname(dhuskies[,1, drop=FALSE]),
                       'Huskies_H'=unname(dhuskies[,2, drop=FALSE]),
                       'monthStart'=unname(dmonthSection[,1, drop=FALSE]),
                       'monthEarly'=unname(dmonthSection[,2, drop=FALSE]),
                       'monthMid'=unname(dmonthSection[,3, drop=FALSE]),
                       'monthLate'=unname(dmonthSection[,4, drop=FALSE]),
                       'monthEnd'=unname(dmonthSection[,5, drop=FALSE]))
                       # 'market'=unname(dmarket))

models = list('an','as','ns','ans') #was at one time going to try different combinations

#start timing
ptm <- proc.time()

#first model fitting loop: fit each predictor individually
model_out <-
    foreach(mod=2, .combine=rbind, .errorhandling='remove') %:%
    # foreach(mod=1:length(models), .combine=rbind, .errorhandling='remove') %:%
    # foreach(pred=1:2, .combine=rbind, .errorhandling='remove') %dopar% {
        foreach(pred=1:ncol(pred_dict), .combine=rbind, .errorhandling='remove') %dopar% {

        #fit model
        xx = pred_dict[,pred, drop=FALSE]
        hyb = hybridModel(train_eval, models=models[[mod]], weights='equal',
                          # windowSize=length(train_eval)/2,
                          # n.args=list(xreg=xx[1:trainlen,]), #something seems to have broken in the nnetar function
                          a.args=list(xreg=xx[1:trainlen,]),
                          s.args=list(xreg=xx[1:trainlen,], method='arima'))

        #forecast last 7 days of known sales (to evaluate predictors)
        fore = forecast(hyb, xreg=xx[(trainlen+1):(trainlen+7),,drop=FALSE],
                        level=c(50,95)) #forecast for models with covariates

        #in-sample accuracy
        rmse_in = accuracy(hyb)[2]
        R2_in = 1-(length(train_eval)*rmse_in)/
            sum((train_eval-mean(train_eval, na.rm=T))^2, na.rm=T)

        #forecast accuracy
        rmse_out = sqrt(mean((fore$mean-test_eval)^2))
        R2_out = 1-(length(test_eval)*rmse_out^2)/sum((test_eval-mean(test_eval, na.rm=T))^2, na.rm=T)

        #for error handling
        # if(!exists('rmse_in')) rmse_in = rmse_out = R2_in = R2_out = 'err'

        #compile results
        data.frame(preds=colnames(pred_dict)[pred], mods=models[[mod]],
                   rmse_in=rmse_in, rmse_out=rmse_out, R2_in=R2_in, R2_out=R2_out,
                   stringsAsFactors=FALSE)
    }

#stop timing
runtime <- proc.time() - ptm

if(exists('model_out')){
    writeLines(paste0('Individual-predictor loop ran to completion.\nElapsed time: ',
                      round(unname(runtime[3])/60, 2), ' minutes.'))
}

#grab top 10 predictors and sort dataframe
top10 = model_out %>% tbl_df() %>%
    arrange(R2_out) %>%
    top_n(n=10, wt=R2_out)

#grab top 6 names and find corresponding indices
top6 = top10 %>% select(preds) %>% tail(6) %>% .[[1]]
top6_ind = which(names(pred_dict) %in% top6)
include = which(colnames(pred_dict) == top6[length(top6)])
best_R2 = c(0, top10$R2_out[which(top10$preds == colnames(pred_dict)[include])])

#second loop: add model-improving predictors successively. stop adding when they stop improving
ptm <- proc.time()
for(i in 1:5){

    leftover = setdiff(top6_ind, include)

    model_out2 <-
        foreach(mod=2, .combine=rbind, .errorhandling='remove') %:%
        # foreach(mod=1:length(models), .combine=rbind, .errorhandling='remove') %:%
        foreach(pred=leftover, .combine=rbind, .errorhandling='remove') %dopar% {

            #fit model
            xx = pred_dict[,c(include,pred)]
            hyb = hybridModel(train_eval, models=models[[mod]], weights='equal',
                              # windowSize=length(train_eval)/2,
                              # n.args=list(xreg=xx[1:trainlen,]),
                              a.args=list(xreg=xx[1:trainlen,]),
                              s.args=list(xreg=xx[1:trainlen,], method='arima'))

            #forecast last 7 days of sales
            fore = forecast(hyb, xreg=xx[(trainlen+1):(trainlen+7),,drop=FALSE],
                            level=c(50,95)) #forecast for models with covariates

            #in-sample accuracy
            rmse_in = accuracy(hyb)[2]
            R2_in = 1-(length(train_eval)*rmse_in)/
                sum((train_eval-mean(train_eval, na.rm=T))^2, na.rm=T)

            #forecast accuracy
            rmse_out = sqrt(mean((fore$mean-test_eval)^2))
            R2_out = 1-(length(test_eval)*rmse_out^2)/sum((test_eval-mean(test_eval, na.rm=T))^2, na.rm=T)

            #error handling
            # if(!exists('rmse_in')) rmse_in = rmse_out = R2_in = R2_out = 'err'

            #compile results
            data.frame(preds=colnames(pred_dict)[pred], mods=models[[mod]],
                       rmse_in=rmse_in, rmse_out=rmse_out, R2_in=R2_in, R2_out=R2_out,
                       stringsAsFactors=FALSE)
        }
    prev = best_R2[length(best_R2)]
    new = max(model_out2$R2_out)
    if(new > prev){
        best_R2 = append(best_R2, new)
        include = append(include,
                         which(colnames(pred_dict) == model_out2$preds[model_out2$R2_out == new]))
    } else break
}

runtime <- proc.time() - ptm

if(exists('model_out2')){
    writeLines(paste0('Predictor-combo loop ran to completion.\nElapsed time: ',
                      round(unname(runtime[3])/60, 2), ' minutes.'))
}

#run best ensemble on N-1 weeks of sales data
xx = pred_dict[,include, drop=FALSE]
hyb_eval = hybridModel(train_eval, models='as', weights='equal',
                  # windowSize=length(train_eval)/2,
                  # n.args=list(xreg=xx[1:trainlen,]),
                  a.args=list(xreg=xx[1:trainlen,]),
                  s.args=list(xreg=xx[1:trainlen,], method='arima'))

#forecast last week of sales (to compare with observations)
fore_eval = forecast(hyb_eval, xreg=xx[(trainlen+1):(trainlen+7),,drop=FALSE],
                level=c(50,95)) #forecast for models with covariates

#in-sample accuracy
rmse_in = accuracy(hyb_eval)[2]
R2_in = 1-(length(train_eval)*rmse_in)/
    sum((train_eval-mean(train_eval, na.rm=T))^2, na.rm=T)

#forecast accuracy (this is the reported "evaluation" R^2 in the app)
rmse_out = sqrt(mean((fore_eval$mean-test_eval)^2))
R2_out = 1-(length(test_eval)*rmse_out^2)/sum((test_eval-mean(test_eval, na.rm=T))^2, na.rm=T)

#run best ensemble on all sales data
hyb_fore = hybridModel(full, models='as', weights='equal',
                  # windowSize=length(train_eval)/2,
                  # n.args=list(xreg=xx[1:length(full),]),
                  a.args=list(xreg=xx[1:length(full),]),
                  s.args=list(xreg=xx[1:length(full),], method='arima'))

#forecast the next seven days
fore_fore = forecast(hyb_fore, xreg=xx[(length(full)+1):(length(full)+7),,drop=FALSE],
                level=c(50,95))

#return forecast and evaluation responses to original scale
# fore_eval$lower = fore_eval$lower * interpsd + interpmn
# fore_eval$mean = fore_eval$mean * interpsd + interpmn
# fore_eval$upper = fore_eval$upper * interpsd + interpmn
# fore_fore$lower = fore_fore$lower * interpsd + interpmn
# fore_fore$mean = fore_fore$mean * interpsd + interpmn
# fore_fore$upper = fore_fore$upper * interpsd + interpmn
# hyb_fore$auto.arima$fitted = hyb_fore$auto.arima$fitted * interpsd + interpmn
# hyb_fore$stlm$fitted = hyb_fore$stlm$fitted * interpsd + interpmn

#create output table for evaluation
results_eval = data.frame('Lower_95'=c(as.character(round(sum(fore_eval$lower[,2]),0))),
                          'Mean'=as.character(c(round(sum(fore_eval$mean),0))),
                          'Upper_95'=c(as.character(round(sum(fore_eval$upper[,2]),0))))
row.names(results_eval) = c('Estimated ($): ')
sum_and_r2 = list(round(sum(test_eval),0), round(R2_out,2))

#create output table for forecast
results_fore = data.frame('Lower_95'=round(sum(fore_fore$lower[,2]),0),
                          'Mean'=round(sum(fore_fore$mean),0),
                          'Upper_95'=round(sum(fore_fore$upper[,2]),0))

#grab item ratios from total sales
ratios = read.csv('data/item_ratios.csv', row.names=1)

#convert sales to # each category sold
itemized_sales = ratios$profit * results_fore$Lower_95 / sum(ratios$profit)
Lower_95 = ceiling(itemized_sales * ratios$count / ratios$profit)
itemized_sales = ratios$profit * results_fore$Mean / sum(ratios$profit)
Mean = ceiling(itemized_sales * ratios$count / ratios$profit)
itemized_sales = ratios$profit * results_fore$Upper_95 / sum(ratios$profit)
Upper_95 = ceiling(itemized_sales * ratios$count / ratios$profit)

#add to the forecast output dataframe
items = cbind(Lower_95, Mean, Upper_95)
results_fore = apply(ceiling(rbind(results_fore, items)), 2, as.integer)
results_fore = rbind(results_fore, colSums(items))
rownames(results_fore) = c('Expected sales ($)', rownames(ratios), 'Total')

#store data to be used by server.R
save(list=c('train_eval','test_eval','fore_eval','hyb_eval','hyb_fore','fore_fore',
            'results_eval','results_fore'), file='data/results.rda')
save(sum_and_r2, file='data/sum_and_r2.rda')
#and helpers.R
pred_names = colnames(pred_dict)
save(list=c('sales', 'interp', 'top10', 'R2_out', 'pred_names' ,'include'), file='data/plot_stuff.rda')
