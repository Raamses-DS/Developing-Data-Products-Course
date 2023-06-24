library(shiny)
library(HistData)
data(GaltonFamilies)
library(dplyr)
library(ggplot2)

# Run the application 
gafa <- GaltonFamilies
#transformation of units
gafa <- gafa %>% mutate(father=father*2.54,
                    mother=mother*2.54,
                    childHeight=childHeight*2.54)

# linear model
model1 <- lm(childHeight ~ father + mother + gender, data=gafa)

shinyServer(function(input, output) {
  output$pText <- renderText({
    paste("Father's height is",
          strong(round(input$inFh, 1)),
          "cm, and mother's height is",
          strong(round(input$inMh, 1)),
          "cm, then:")
  })
  output$pred <- renderText({
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gafa$gender)))
    ch <- predict(model1, newdata=df)
    kid <- ifelse(
      input$inGen=="female",
      "DAUGTHER",
      "SON"
    )
    paste0(em(strong(kid)),
           "'s predicted height is going to be around ",
           em(strong(round(ch))),
           " cm"
    )
  })
  output$Plot <- renderPlot({
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gafa$gender)))
    ch <- predict(model1, newdata=df)
    yvals <- c("FATHER", kid, "MOTHER")
    df <- data.frame(
      x = factor(yvals, levels = yvals, ordered = TRUE),
      y = c(input$inFh, ch, input$inMh))
    ggplot(df, aes(x=x, y=y, color=c("darkcyan", "orange", "black"), fill=c("darkcyan", 
                                                                            "orange", 
                                                                          "black"))) +
      geom_bar(stat="identity", width=0.7) +
      xlab("") +
      ylab("HEIGHT (cm)") +
      theme_minimal() +
      theme(legend.position="none")
  })
})
