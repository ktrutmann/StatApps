library(shiny)

ui <- fluidPage(
    withMathJax(),
    titlePanel("Goodness of Fit"),
    p("You are conducting an experiment using six sided dice.
    Use the sliders to adjust how likely it is to roll a six,
    and how many dice you rolled in your experiment."),
    p("The probability of it being a fair die is calculated via the Pearson
    teststatistic:
    $$\\chi^2 = \\sum_{k=1}^r \\frac{(Y_k-np_k)^2}{np_k}$$"),

    sidebarPanel(
        sliderInput("slider6", "\\(P(six):\\)",   0, .5, .1666),
        sliderInput("sliderN", "\\(N:\\)", 10, 500, 100),
        actionButton("resample", "Draw Sample"),
        actionButton("fair", "Set to Fair")
    ),

    mainPanel(
        wellPanel(plotOutput("bars")),
        wellPanel(verbatimTextOutput("chiOut"))
    )

)

server <- function(input, output, session){

    resample <- function(N = input$sliderN, prob6 = input$slider6) {
        as.table(rmultinom(n = 1, size = N,
        prob=c(rep((1 - prob6) / 5, 5), prob6))[,1])
    }
  
    expected <- reactive(input$sliderN/6)

    test <- reactive(chisq.test(wuerfe())) 
    
    wuerfe <- eventReactive(input$resample, resample(), ignoreNULL = FALSE)

    observeEvent(input$fair, updateSliderInput(session, "slider6",
        value = .166666))

    output$bars <- renderPlot({

        par(mfrow = c(1,2))
                
        barplot(wuerfe(), ylim = c(0, 100), ylab = paste("N = ",
            input$sliderN), main = "Sample distribution", col = "lightblue",
            names.arg = c("One", "Two", "Three", "Four", "Five", "Six"))
        abline(a = expected(), b = 0, col = "red")
        mtext("expected", 4, 0, at = expected(), col = "red")

        plot(1, xlim = c(0, 20), ylim = c(0, .15), type = "n", axes =
            TRUE, xlab = "df = 5", ylab = "",
            main = expression(chi^2-distribution))
        polygon(c(test()$statistic, seq(test()$statistic, 20,
            length.out=101), 20),
            c(0, dchisq(seq(test()$statistic, 20, length.out=101), df = 5), 0),
            border=NA, col="lightblue")
        abline(v = test()$statistic, col = "red")
        curve(dchisq(x, df = 5), add = TRUE)
    })

    output$chiOut <- renderPrint({
       print(data.frame(Chi.square = test()$statistic,
                  p.value = test()$p.value), row.names=FALSE)
    })
}

shinyApp(ui, server)


