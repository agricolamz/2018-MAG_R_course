library(shiny)

shinyUI(fluidPage(
    
    titlePanel('Fundamental Probability Distributions'),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(type='tabs', id='dist_tabs',
                tabPanel('Beta',
                    sliderInput('beta_alpha', 'shape 1:', min=0.1, max=50, value=0.5, step=1),
                    sliderInput('beta_beta', 'shape 2:',  min=0.1, max=50, value=0.5, step=1)
                )
            )
        ),
        mainPanel(
            textOutput('header'),
            plotOutput('pdf', width='500px', height='250px'),
            plotOutput('cdf', width='500px', height='250px')
        )
    )
    
))

