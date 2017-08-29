#demand forecasting project
#created: 3/13/17
#author: Mike Vlah (vlahm13@gmail.com)

#main plot (but just the black trend line)
obs_trend = function(xmin, xmax, ...){

    #get last day of actual data
    last_record = tail(which(!(is.na(sales$sales))), 1)

    #create time series from first to last day of data
    tsf = stats::window(sales$sales, start=index(sales$sales)[1],
                        end=index(sales$sales)[last_record])

    #do the same for the interpolation
    interpf = stats::window(interp, start=index(interp)[1],
                            end=index(interp)[last_record])

    #subset the whole dataset too
    tblf = sales[1:(last_record+7),]

    #get number of mondays, firsts of each week and month, etc
    nMon = floor(nrow(tblf)/7)
    month_ord = c(month.abb[11:12],month.abb[1:10])
    firsts = which(tblf$dayofmonth=='01')
    Firsts = which(tblf$dayofmonth=='01' & substr(tblf$date,6,7)=='01')
    yrs = seq(2017, length.out=length(Firsts))

    #get the x limits for the plot space
    grandmin = as.numeric(as.POSIXct('2016/10/27', origin='1970-01-01', tz='PT'))
    grandmax = as.numeric(as.POSIXct(tblf$date[last_record], origin='1970-01-01', tz='PT')+11*60*60*24)
    inputmin = as.numeric(as.POSIXct(xmin, origin='1970-01-01', tz='PT'))
    inputmax = as.numeric(as.POSIXct(xmax, origin='1970-01-01', tz='PT'))
    indx = index(tsf)
    xrng = rescale(c(grandmin, inputmin, inputmax, grandmax), c(0.428571, indx[length(indx)]+(1+4/7)))

    #plot the trend line
    plot.ts(tsf, xlab='', ylab='Net Sales ($)', type='n', xaxt='n', las=2,
            xlim=c(xrng[2], xrng[3]), xaxs='i')
    axis(1, at=1:(nMon+1), c('1',sprintf('%d',tblf$dayofmonth[which(tblf$dayofweek=='Mon')])),
         tcl=-.3, padj=-1.3, cex.axis=.8)
    axis(1, at=rescale(firsts, c(0,firsts[length(firsts)]/7)+1),
         rep(month_ord, length.out=length(unique(substr(tblf$date,1,7)))),
         tcl=-.4, lwd.ticks=2, padj=.4, cex.axis=.9)
    axis(1, at=rescale(c(1,Firsts),
                       c(1,Firsts[length(Firsts)]/7+1))[2:(length(Firsts)+1)],
         labels=yrs, tcl=-.5, lwd.ticks=4, padj=1.8, cex.axis=.9)
    abline(v=1:(nMon+1), lty=2, col='gray')
    lines(tsf, ...)
}

#plot of top 10 individual predictors
top10_plot = function(){
    par(mar=c(7,5,2,1))
    cols = rep('red', 10)
    cols[which(top10$preds %in% pred_names[include])] = 'blue'
    plot(top10$R2_out, type='p', ylab=expression(paste('Forecast ', R^2)),
         xlab='', main='Top 10 individual predictors', las=1, xaxt='n',
         col=cols, pch=20, cex=2)
    axis(1,at=1:10,labels=top10$preds, las=3)
    legend('topleft',legend=c('Not included','Included'), pch=20,
           col=c('red','blue'), bty='n')
# top10 %>%
#     ggplot(aes(x=factor(preds, levels=unique(as.character(preds))), y=R2_out,
#                colour=rep(c('Not included','Included'), times=c(4,6)))) +
#     geom_point() +
#     theme(axis.text.x = element_text(angle = 90)) +
#     coord_cartesian(ylim = c(0, 1)) +
#     labs(x='Predictor',y=expression(paste('Forecast ', R^2))) +
#     text('Top 10 individual predictors')
}

#overlays evaluation results on main plot
eval_plot = function(train_eval,test_eval,fore_eval,hyb_eval){

    fore_ind = seq((length(train_eval))/7+1, (length(train_eval))/7+2, length.out=8)[-1]
    obs_ind = seq((length(train_eval)-1)/7+1, (length(train_eval)-1)/7+2, length.out=8)[-1]
    lines(obs_ind[-1], test_eval[-1], lwd=3, col='white') #cover observed
    polygon(x=c(fore_ind, rev(fore_ind)), y=c(fore_eval$upper[,2], rev(fore_eval$lower[,2])),
            border=NA, col=adjustcolor('darkred',alpha.f=0.3))
    polygon(x=c(fore_ind, rev(fore_ind)), y=c(fore_eval$upper[,1], rev(fore_eval$lower[,1])),
            border=NA, col=adjustcolor('darkred',alpha.f=0.6))
    lines(fore_ind, fore_eval$mean, col='darkred', lwd=3)

    lines(hyb_eval$auto.arima$fitted, col='blue') #aa
    # lines(hyb_eval$ets$fitted, col='orange') #e smooth
    # lines(hyb_eval$thetam$fitted, col='blue') #theta
    # lines(hyb_eval$nnetar$fitted, col='green') #nn
    lines(hyb_eval$stlm$fitted, col='purple') #stlm
    # lines(hyb_eval$tbats$fitted, col='pink') #tbats
    lines(obs_ind[-1], test_eval[-1], lwd=2, lty=2, col='black') #replot observed

    # final_preds = paste0('Predictors included: ',
                         # paste(colnames(pred_dict)[include], collapse=', '), '.')
    legend('topleft', legend=c('Estimated','50%','95%','Observed'),
           col=c('darkred',adjustcolor('darkred',alpha.f=0.6),adjustcolor('darkred',alpha.f=0.3),'black'),
           lty=c(1,1,1,2), lwd=2, bty='n', cex=0.8)
}

#overlays forecast results on main plot
fore_plot = function(train_eval,fore_fore,hyb_fore){

    fore_ind = seq((length(train_eval))/7+2, (length(train_eval))/7+3, length.out=8)[-1]
    polygon(x=c(fore_ind, rev(fore_ind)), y=c(fore_fore$upper[,2], rev(fore_fore$lower[,2])),
            border=NA, col=adjustcolor('darkred',alpha.f=0.3))
    polygon(x=c(fore_ind, rev(fore_ind)), y=c(fore_fore$upper[,1], rev(fore_fore$lower[,1])),
            border=NA, col=adjustcolor('darkred',alpha.f=0.6))
    lines(fore_ind, fore_fore$mean, col='darkred', lwd=3)

    lines(hyb_fore$auto.arima$fitted, col='blue') #aa
    # lines(hyb_fore$ets$fitted, col='orange') #e smooth
    # lines(hyb_fore$thetam$fitted, col='blue') #theta
    # lines(hyb_fore$nnetar$fitted, col='green') #nn
    lines(hyb_fore$stlm$fitted, col='purple') #stlm
    # lines(hyb_fore$tbats$fitted, col='pink') #tbats

    legend('topleft', legend=c('Forecast','50%','95%'),
           col=c('darkred',adjustcolor('darkred',alpha.f=0.6),adjustcolor('darkred',alpha.f=0.3)),
           lty=c(1,1,1), lwd=2, bty='n', cex=0.8)
}
