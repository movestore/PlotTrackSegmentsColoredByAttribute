library(shiny)
library(move)
library(ggplot2)
library(shinyWidgets)
library(colourpicker)
library(htmltools)
library(ggforce)

## ToDo?: 
# make it possible to change linesize
# make zoom-in tool
# give possibility to select/unselect all individuals with one click
# give option to only display one individual, ie, when another is selected, the previous one is unselected
# if variable is factor, give color palette options?
# make multipanel with aspect ratio==1, look into cowplot pkg
# add costline, or something that may give orientation on where the tracks are



data <- readRDS("sixtyindiv.rds") ## check if there is a possibility to load data before running shiny app. These data are just used for the UI. They than again have to be loaded in the Shiny module as for any other shiny app.....


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Plot track(s) colored by attribute"),
    fluidRow(
      # column(3,uiOutput(ns('uiAttributeL'))),
      column(3,selectInput(ns("attributeL"), "Select attribute", choices=colnames(data@data[, colSums(is.na(data@data)) != nrow(data@data)]))),
      column(3,selectInput(ns("panels"), "Choose display mode", choices=c("Single panel","Multipanel"), selected="Single panel", multiple=F)),
      column(2,colourInput(ns("colmin"), "Select colour: low", "blue")),
      column(2,colourInput(ns("colmid"), "mid", "yellow")),
      column(2,colourInput(ns("colmax"), "high", "red"))),
    uiOutput(ns('uiIndivL')),
    span(textOutput(ns("warningtext")),
         
    plotOutput(ns("plot")), style="color:red"),
    fluidRow(
      # column(2,numericInput(ns("linesize"), "Width of line in mm", value=0.5, min = 0.1, max = 10, step=0.1)), # does not work for now
      column(2, style = "margin-top: 25px;", downloadButton(ns('savePlot'), 'Save Plot')),
      column(3,style = "margin-top: 15px;",helpText("OPTIONAL: save plot with personalized width and height:",style="color:black;font-style: italic"),offset=3),
      column(2, numericInput(ns("widthmm"), "Width (mm)", value=NA), style = "font-size: 12px; font-style: italic"),
      column(2, numericInput(ns("heightmm"), "Height (mm)", value=NA),style = "font-size: 12px; font-style: italic"),#
    )
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()
  configuration
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  # output$uiAttributeL <- renderUI({
  #  dataCC <- data@data[, colSums(is.na(data@data)) != nrow(data@data)] ## maybe look for a more efective way of doing this in case data set is very large
  #   selectInput(ns("attributeL"), "Select attribute", choices=colnames(dataCC))})

  output$uiIndivL <- renderUI({
    # checkboxGroupInput(ns("indivL"), "Select individuals", choices=namesIndiv(data), selected=namesIndiv(data)[1], inline=TRUE)
    checkboxGroupButtons(ns("indivL"), "Select individuals", size="sm", choices=namesIndiv(data), selected=namesIndiv(data)[1],status="default",checkIcon = list(
      # yes = icon("check-square"), no = icon("square-o")))
      yes = icon("ok",lib = "glyphicon"))) #, no = icon("remove",lib = "glyphicon")
  })
  
  output$plot <- renderPlot({
    mDF <- data.frame(long=coordinates(data)[,1],lat=coordinates(data)[,2],attribute=data@data[,input$attributeL], indiv=trackId(data))
    mDF <- mDF[mDF$indiv %in% c(input$indivL),]
    if(input$panels=="Single panel"){
      output$warningtext <- NULL
      if(is.numeric(mDF[, "attribute"])){ 
        minattr <- min(mDF[, "attribute"],na.rm=T)
        maxattr <- max(mDF[, "attribute"],na.rm=T)
        mpt <- (minattr+maxattr)/2
        ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
          scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                 # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
          )+
          coord_fixed()+
          labs(x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }else{ 
        ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
          scale_color_discrete(name=input$attributeL)+
          coord_fixed()+
          labs(x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }
    }else if(input$panels=="Multipanel"){
      output$warningtext <- renderText({"WARNING: Aspect ratio of plots is distorted and not 1/1"})
      if(nrow(mDF)==0){ ## if plot is empty
        ggplot(mDF)+labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
      }else{
        if(is.numeric(mDF[, "attribute"])){ 
          minattr <- min(mDF[, "attribute"],na.rm=T)
          maxattr <- max(mDF[, "attribute"],na.rm=T)
          mpt <- (minattr+maxattr)/2
          ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
            facet_wrap(~indiv, scales="free")+
            scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                   # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
            )+
            # coord_fixed()+
            labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
        }else{ 
          ggplot(mDF) + geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
            facet_wrap(~indiv, scales="free")+
            scale_color_discrete(name=input$attributeL)+
            # coord_fixed()+
            labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
        }
      }
    }
  })     

  ### save plot ###
  output$savePlot <- downloadHandler(
    filename = "ColorSegmentsByAttributePlot.png",
    content = function(file) {
      ggsave(file, device = "png", units="mm", width = input$widthmm, height = input$heightmm)
    }
  )
  return(reactive({ current() }))
}

