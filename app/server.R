#demand forecasting project
#created: 3/13/17
#author: Mike Vlah (vlahm13@gmail.com)
# options(shiny.error = browser)

#load packages needed to render the app
require(plotrix)
require(dplyr)
require(shiny)
require(forecast)
require(ggplot2)
require(zoo)

#read in data
load('data/results.rda')
load('data/sum_and_r2.rda')

#I/O handled here
shinyServer(
    function(input, output) {

        #read radio button inputs and render appropriate main plot
        output$OBS_TREND = renderPlot({
            obs_trend(xmin=input$range[1], xmax=input$range[2], lwd=2)
            mode_choice <- switch(input$fore_eval_choice,
                           'Evaluation results' = eval_plot(train_eval=train_eval, test_eval=test_eval,
                                                            fore=fore_eval, hyb=hyb_eval),
                           'Raw sales' = NULL,
                           'Forecast results' = fore_plot(train_eval=train_eval,
                                                          fore=fore_fore, hyb=hyb_fore))
        })

        #generate "top 10 individual predictors" plot
        output$TOP_10 = renderPlot({
            if(input$fore_eval_choice == 'Evaluation results') top10_plot()
        })

        #generate table based on next chunk
        output$RESULTS = renderTable({results_table()}, rownames=TRUE, align='c',
                                     digits=0)

        #generate appropriate table, depending on radio button choice
        results_table = reactive({
            switch(input$fore_eval_choice,
                   'Evaluation results' = results_eval,
                   'Forecast results' = results_fore)
        })
        output$RES_TEXT <- renderUI({
            str1 = paste0('Observed sales ($): ', sum_and_r2[[1]])
            str2 = paste0('R^2: ', sum_and_r2[[2]])
            switch(input$fore_eval_choice,
                   'Evaluation results' = HTML(paste(str1, str2, sep='<br/>')),
                   'Forecast results' = '')
        })
    }
)

