rm(list = ls(all = TRUE))
library(generator)
library(dplyr)
library(styler)
library(lmtest)
#### ----GENERATE RANDOM DATA----
x1 <- rep(0, 100)
x1a <- sample(0:1, 100, replace = T, prob = c(.95, .05))
x1b <- sample(0:1, 100, replace = T, prob = c(.85, .15))
x1c <- sample(0:1, 100, replace = T, prob = c(.75, .25))
x1d <- sample(0:1, 100, replace = T, prob = c(.65, .35))
x1e <- sample(0:1, 100, replace = T, prob = c(.55, .45))

x2 <- sort(rnorm(100, 5.05, 1.29), decreasing = T) + rnorm(100, -.5, .5)
x3 <- rweibull(100, 7.05, 7.03)
x4 <- rpois(100, 1.23)
x5_list <- list("a", "b", "c")
x5 <- as.character(sample(x5_list, 100, replace = T, prob = c(.15, .45, .4)))
df1 <- cbind.data.frame(x1, x1a, x1b, x1c, x1d, x1e, x2, x3, x4, x5)

x1 <- rep(1, 233)
x1a <- sample(0:1, 233, replace = T, prob = c(.05, .95))
x1b <- sample(0:1, 233, replace = T, prob = c(.15, .85))
x1c <- sample(0:1, 233, replace = T, prob = c(.25, .75))
x1d <- sample(0:1, 233, replace = T, prob = c(.35, .65))
x1e <- sample(0:1, 233, replace = T, prob = c(.45, .55))

x2 <- sort(rnorm(233, 1.9, 1.26)) + rnorm(233, .5, 1)
x3 <- rweibull(233, 5.05, 12.03)
x4 <- rpois(233, 1.23)
x5_list <- list("a", "b", "c")
x5 <- as.character(sample(x5_list, 233, replace = T, prob = c(.55, .25, .2)))
df2 <- cbind.data.frame(x1, x1a, x1b, x1c, x1d, x1e, x2, x3, x4, x5)
data <- rbind(df1, df2)




library(shiny)
library(shinythemes)
library(ggplot2)
#### ----
ui <- shinyUI(fluidPage(
  theme = shinytheme("slate"), titlePanel("LOGIT"),

  fluidRow(
    column(2, selectInput("dep_var",
      "Dependant Variable",
      choices = c(
        "x1" = "x1",
        "x1a" = "x1a",
        "x1b" = "x1b",
        "x1c" = "x1c",
        "x1d" = "x1d",
        "x1e" = "x1e"
      ),
      selected = "x1"
    )),
    column(2, selectInput("ind_vars",
      "Independant Variables",
      choices = c(
        "x2" = "x2",
        "x3" = "x3",
        "x4" = "x4",
        "x5" = "x5"
      ), multiple = TRUE,
      selected = "x2"
    )),
    column(2, selectInput("lr_var",
      "LR-Test (Select Vars to Exclude)",
      choices = c(
        "x2" = "x2",
        "x3" = "x3",
        "x4" = "x4",
        "x5" = "x5"
      ), multiple = TRUE,
      selected = NULL
    )),
    column(2, selectInput("plot_var",
      "Plotting Vars",
      choices = c(
        "x2" = "x2",
        "x3" = "x3",
        "x4" = "x4",
        "x5" = "x5"
      ), selected = "x2"
    )),
    column(2, selectInput("facet_var",
      "Facet Vars",
      choices = c(
        "x1" = "x1",
        "x1a" = "x1a",
        "x1b" = "x1b",
        "x1c" = "x1c",
        "x1d" = "x1d",
        "x1e" = "x1e",
        "x5" = "x5"
      ), selected = "x5"
    )),
    fluidRow(
      column(
        12,
        plotOutput("plot1")
      )
    ),
    fluidRow(
      column(
        6,
        verbatimTextOutput("results")
      ),
      column(
        3,
        verbatimTextOutput("lr_test")
      )
    )
  )
))

server <- function(input, output, session) {
  output$dep_var <- renderUI({
    selectInput(inputId = "dep_var", label = "DEP VAR")
  })
  output$ind_vars <- renderUI({
    selectInput(inputId = "ind_vars", label = "IND VAR")
  })

  logit <- reactive({
    depv <- as.character(as.list(input$dep_var))
    list <- as.character(as.list(input$ind_vars))
    form <- as.formula(paste(depv, paste(list, sep = "", collapse = " + "), sep = " ~ "))
    model <- glm(form, data = data, family = binomial())
    dt <- summary(model)
    return(dt)
  })

  plot <- reactive({
    p <- ggplot(data, aes(
      y = eval(parse(text = input$dep_var)), x = eval(parse(text = input$plot_var)),
      color = eval(parse(text = input$facet_var))
    )) +
      stat_smooth(
        method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
        alpha = 0.2, size = 2, aes(fill = eval(parse(text = input$facet_var)))
      ) +
      geom_point(position = position_jitter(height = 0.03, width = 0))
    p <- p + facet_grid(. ~ eval(parse(text = input$facet_var)))
    return(p)
  })

  lrtest <- reactive({
    depv <- as.character(as.list(input$dep_var))
    list <- as.character(as.list(input$ind_vars))
    form <- as.formula(paste(depv, paste(list, sep = "", collapse = " + "), sep = " ~ "))
    model1 <- glm(form, data = data, family = "binomial")

    depv2 <- as.character(as.list(input$dep_var))
    list2 <- as.character(as.list(input$lr_var))
    list2 <- list[!list %in% list2]
    form2 <- as.formula(paste(depv2, paste(list2, sep = "", collapse = " + "), sep = " ~ "))
    model2 <- glm(form2, data = data, family = "binomial")
    test2 <- lmtest::lrtest(model1, model2)
    return(test2)
  })
  output$results <- renderPrint({
    logit()
  })
  output$lr_test <- renderPrint({
    lrtest()
  })
  output$plot1 <- renderPlot({
    plot()
  })
}
shinyApp(ui = ui, server = server)
