library(shiny)

ui <- fluidPage(
    withMathJax(),
    titlePanel("Effect, Sample Size and p-value"),
    p("Use the sliders to adjust the effect and sample size and observe the
    p-value."),
    p("The plot shows the distributions of sample means under the null
    hypothesis as well as the alternative hypothesis."),

    sidebarPanel(
        sliderInput("effect", "Effect Size", 0, 1.2, .2, .01),
        sliderInput("N", "Sample Size", 5, 100, 50),
        checkboxInput("bothSides", "Two sided hypothesis", FALSE),

        verbatimTextOutput("pval")
    ),

    mainPanel(
        wellPanel(plotOutput("plot"))
        )

)

server <- function(input, output) {

    output$plot <- renderPlot({
        curve(dnorm(x, sd = (1/sqrt(input$N))), xlim = c(-1, 1.5), ylab =
            "", xlab = "Effect")

        polygon(c(input$effect, seq(input$effect, 1.5, length.out = 101),
            1.5), c(0, dnorm(seq(input$effect, 1.5, length.out = 101), sd =
            (1/sqrt(input$N))), 0), col = "lightblue")

        if (input$bothSides) {
            polygon(c(-1, seq(-1, -input$effect, length.out = 101),
                -input$effect), c(0, dnorm(seq(-1, -input$effect, length.out
                = 101), sd = (1/sqrt(input$N))), 0),
                col = "lightblue")
        }

        curve(dnorm(x, sd = (1/sqrt(input$N)), mean = input$effect), add =
            TRUE, col = "blue")

        abline(v = 0)
        abline(v = input$effect, col = "blue")

        mtext("H0", 1, 2, at = 0)
        mtext("H1", 1, 2, at = input$effect, col = "blue")
    })

    output$pval <- renderPrint({
        pval <-  pnorm(input$effect*sqrt(input$N), lower.tail = FALSE)
        
        if(input$bothSides)
            pval <- pval*2

        cat(paste("p-value: ", pval))
    })

}

shinyApp(ui, server)
