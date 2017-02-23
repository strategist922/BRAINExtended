library(shiny)
library(ggplot2)
library(plotly)

load('data/shinydata.Rdata')

shinyServer(function(input, output){

  test.data.react <- reactive({
    test.data$mci.baseline <- with(test.data, factor(iqcode.score.e > as.numeric(input$mci.iqcode),
                                                     labels = c('Normal', 'MCI')))
    if(input$age.cat == 'All patients'){
      test.data$age.cat <- factor(1, labels = 'All Patients')
    } else if(input$age.cat == '<=65 / >65'){
      test.data$age.cat <- with(test.data, factor(ifelse(is.na(age.enroll), NA,
                                                  ifelse(age.enroll <= 65, 1, 2)),
                                                  levels = 1:2, labels = c('<=65', '>65')))
    } else{
      test.data$age.cat <- with(test.data, factor(ifelse(is.na(age.enroll), NA,
                                                  ifelse(age.enroll <= 50, 1,
                                                  ifelse(age.enroll <= 65, 2, 3))),
                                                  levels = 1:3, labels = c('<=50', '50-65', '>65')))
    }
    test.data
  })

  output$testscore_plot <- renderPlot({
    ggplot(data = test.data.react(), aes(x = time.pt, y = test.score, group = id)) +
      facet_grid(age.cat ~ mci.baseline) +
      annotate(geom = 'rect', ymin = 0, ymax = 78, xmin = 0, xmax = 365*7,
               fill = '#B85C00', alpha = 0.1) +
      annotate(geom = 'rect', ymin = 78, ymax = 122, xmin = 0, xmax = 365*7,
               fill = '#007D51', alpha = 0.1) +
      geom_line(alpha = 0.2, colour = '#003D79') +
      scale_x_continuous(breaks = c(90, 365, 365*4, 365*6),
                         labels = c('3M', '12 Months', '4 Years', '6 Years'),
                         name = NULL) +
      scale_y_continuous(name = 'Test Score') +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank())
  })
})


