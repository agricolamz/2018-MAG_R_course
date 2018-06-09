library(shiny)
library(ggplot2)


shinyServer(function(input, output) {
    
    output$header <- renderText({
        switch(input$dist_tabs,
            'Beta' = sprintf('Beta distribution with shape parameters: (%.1f, %.1f)', input$beta_alpha, input$beta_beta)
        )

    })
    
    
    output$pdf <- renderPlot({
        dat <- switch(input$dist_tabs,
            'Beta' = data.frame(x=seq(0, 1, length=200),
                                y=dbeta(seq(0, 1, length=200), input$beta_alpha, input$beta_beta))
        )
        
        ggplot(dat, aes(x, y)) + geom_line(size=1.2) + theme_bw() + xlab('x') + ylab('') + ggtitle('Probability Density/Mass Function') 
    })
    

    output$cdf <- renderPlot({
        dat <- switch(input$dist_tabs,
            'Beta' = data.frame(x=seq(0, 1, length=200),
                                y=pbeta(seq(0, 1, length=200), input$beta_alpha, input$beta_beta)))
        
        ggplot(dat, aes(x, y)) + geom_line(size=1.2) + theme_bw() + xlab('x') + ylab('') + ggtitle('Cumulative Distibution Function') 
    })
    
})

