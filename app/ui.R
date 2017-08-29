#demand forecasting project
#created: 3/13/17
#author: Mike Vlah (vlahm13@gmail.com)

#load data
load('data/plot_stuff.rda', envir=.GlobalEnv)

#load plot functions
source("helpers.R")

#visual rendering performed here, inputs received and passed to server.R
shinyUI(fluidPage(
    titlePanel("Demand Forecast"),
    sidebarLayout(
        sidebarPanel(

            helpText('Choose which layer to view.'),

            radioButtons("fore_eval_choice",
                         label = NULL,
                         choices = list('Forecast results', 'Evaluation results', 'Raw sales'),
                         selected = 'Forecast results'),
            sliderInput("range",
                        label='Range of interest:',
                        min=as.Date('2016/10/27'), max=sales$date[tail(which(!(is.na(sales$sales))),1)]+11,
                        value=c(as.Date('2016/10/27'), sales$date[tail(which(!(is.na(sales$sales))),1)]+11),
                        ticks=TRUE)
            ),

        mainPanel(
            plotOutput('OBS_TREND'),
            tableOutput('RESULTS'),
            htmlOutput('RES_TEXT'),
            plotOutput('TOP_10')
        )
    )
))
