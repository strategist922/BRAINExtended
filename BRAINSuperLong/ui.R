library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(

  headerPanel('Test Scores by Age and Baseline Cognition'),

  sidebarPanel(
    selectInput('mci.iqcode', 'IQCODE Cutoff for MCI:', choices = c('3', '3.3', '3.6')),
    selectInput('age.cat',
                'Age Categorization:',
                choices = c('All patients', '<=65 / >65', '<50 / 50-65 / >65')),
    p("Area shaded in green indicates normal test scores +/- 1.5 SD. Area shaded in red indicates cognitive impairment (>1.5 SD below normal).")
  ),

  mainPanel(
    plotOutput('testscore_plot')
  )
))
