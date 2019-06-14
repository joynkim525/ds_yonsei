install.packages("shiny")
library(shiny)

shiny::runGitHub('shinyLecture2', 'cardiomoon', subdir='inst/app0')
#download the apps from the server


#minimal frame
ui = fluidPage()
server = function(input, output){

}

shinyApp(ui, server)

##전체 space를 12개의 column으로 간주 
##shiny는 웹 상에서 html 파일을 r에서 작성하기 편하게 하는 것


sliderInput("obs", "Number of observations", min=0, max=100, value=500)
##sliderInput(id, label, additional options)
##input의 아이디는 개발자만 알아볼 정도로 간단하게
##input의 label은 사용자가 알아볼 수 있을 정도로 구체적으로 

##output function 중에서 uiOutput이 제일 중요하고 많이 쓰임

#basic slider histogram

ui = fluidPage(
  sliderInput("obs", "Number of observations", min=0, max=100, value=500),
  plotOutput("plot")
)
server = function(input, output){
  output$plot=renderPlot(
      hist(rnorm(input$obs), main=input$title))
}

shinyApp(ui, server)

#reactivity
ui = fluidPage(
  sliderInput("obs", "Number of observations", min=0, max=100, value=500),
  textInput("title", "Title", value="Histogram"),
  plotOutput("plot")
)
erver = function(input, output){
  output$plot=renderPlot(
    hist(rnorm(input$obs), main=input$title))
}

shinyApp(ui, server)

#isolate the reactivity


ui = fluidPage(
  sliderInput("obs", "Number of observations", min=0, max=100, value=500),
  textInput("title", "Title", value="Histogram"),
  actionButton("makePlot", "Make Plot"),
  plotOutput("plot")
)
server = function(input, output){
  output$plot=renderPlot({
    input$makePlot
    isolate({
      hist(rnorm(input$obs), main=input$title)  
    })
  })
}

shinyApp(ui, server)

##plot과 관련된 data 측면에서만 reactivity가 있어야 유용
##all input에 대해 reactivity가 발생할 경우 isolate로 끊어줄 필요가 있음

#add summary - 여기부터 뭔가 놓침..

ui = fluidPage(
  sliderInput("obs", "Number of observations", min=0, max=1000, value=500),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)
server = function(input, output){
  output$summary=renderPrint({
    summary(rnorm(input$obs))
  })
  output$plot=renderPlot(
    hist(rnorm(input$obs), main=input$title)
    )
}

shinyApp(ui, server)

##서로 다른 난수에 대해 summary와 plot -> 원하는 게 아님

# add summary for same random number
##반응성 객체

ui = fluidPage(
  sliderInput("obs", "Number of observations", min=0, max=1000, value=500),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server = function(input, output){
  data = reactive(rnorm(input$obs))
  output$summary=renderPrint({
    summary(data())
  })
  output$plot=renderPlot(
    hist(data(), main=input$title)
  )
}

shinyApp(ui, server)

#linear regression
## basic r function
fit <- lm(mpg~wt, data=mtcars)
summary(fit)

library(ggplot2)
ggplot(data=mtcars, aes(x=wt, y=mpg))+
  geom_point() + stat_smooth(method='lm')



ui = fluidPage(
  selectInput("x", "Select independent variable",
              choices=setdiff(colnames(mtcars), "mpg")),
  actionButton("anlaysis", "Analysis"),
  verbatimTextOutput("result"),
  plotOutput("plot")
)

server = function(input, output){
  lmequation = reactive({
    paste0("lm(mpg~", input$x, ",data=mtcars)")
  })
  
  output$result=renderPrint({
    
    input$analysis
    isolate({
      fit = eval(parse(text=lmequation()))
      summary(fit)
    })
  })

  output$plot=renderPlot({
    input$analysis
    isolate({
      
      fit = eval(parse(text=lmequation()))
      equation = paste0(round(fit$coef[2], 2), input$x,
                        ifelse(fit$coef[1] >= 0, "+", "-"), abs(round(fit$coef[1], 2)))
      
      ggplot(data=mtcars, aes_string(x=input$x, y="mpg"))+
        geom_point() + stat_smooth(method='lm')+labs(title=equation)
    })
  })
}


shinyApp(ui, server)

##setdiff: 차집합 

#Multiple linear regression
library(DT)

ui = fluidPage(
  column(3,
         radioButtons("dataname", "Select data",
               choices=c("mtcars", "iris")),
         selectInput("y", 'select dependent variable',
                     choices = ""),
         selectInput("x", 'select independent variable',
                     choices = "", multiple = T)
         ),
  
  column(9,
         checkboxInput('showtable', 'Show table'),
         conditionalPanel(condition='input.showtable == true',
                          DT::dataTableOutput("table"))
         )
  
  
)

server = function(input, output){
  data=reactive({
    eval(parse(text=input$dataname))
  })
  
  observeEvent(input$dataname, {
    updateSelectInput(session, "y", choices=colnames(data()))
  })
  
  output$table=DT::renderDataTable(DT::datatable(
    data()
  ))
  
  
  
}

shinyApp(ui, server)


## conditionapanel: condition을 만족할 때 결과창을 보여줌
## condition에 대해서는 javascript 문법에 따라 작성 필요

