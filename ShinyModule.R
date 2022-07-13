library(shiny)
library(move)
library(ggplot2)
library(shinyWidgets)
library(colourpicker)
library(htmltools)
library(ggforce)
library(units)
library(lubridate)
# library(cowplot)

## ToDo?: 
# make it possible to change linesize
# give possibility to select/unselect all individuals with one click
# give option to only display one individual, ie, when another is selected, the previous one is unselected
# if variable is factor, give color palette options?
# make multipanel with aspect ratio==1, look into cowplot pkg
# add costline, or something that may give orientation on where the tracks are
# when a single individual is plotted the legendcode does not appear !?

## Sarahs suggestions:
# Add the results to the MoveStack. For example, add something like an attribute 'color' containing the hexadecimal code for the color and an attribute 'color-legend' containing the value used to define the color. This would allow the results to be saved as an RDS or passed on to a subsequent app to create a shapefile, kml, etc.
# Provide color ramps for categories
# One part of her request was to be able to distinguish both individual tracks + the attribute of interest. She said when they have maps with 100+ animals, they use different color ramps (e.g., blues and reds). Even though colors are reused, if they are not adjacent it is still effective. I also though of exploring the possibility of coloring by 2 different attributes.
# Offer some standard background map options.
# Offer a points option with different symbols. dropdown to choose pch symbols and define color/line/alpha/size
# a select/unselect all (indiv) would be helpful.
# ckeck tmap and mapview


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Plot track(s) colored by attribute"),
    fluidRow(
      column(3,uiOutput(ns('uiAttributeL'))),
      column(3,selectInput(ns("panels"), "Choose display mode", choices=c("Single panel","Multipanel"), selected="Single panel", multiple=F)),
      column(2,colourInput(ns("colmin"), "Select colour: low", "blue")),
      column(2,colourInput(ns("colmid"), "mid", "yellow")),
      column(2,colourInput(ns("colmax"), "high", "red"))),
    uiOutput(ns('uiIndivL')),
    # actionButton(ns("selectall"), label="Select/Deselect all individuals"),
    span(textOutput(ns("warningtext")),
         plotOutput(ns("plot"), dblclick = ns("plot_dblclick"), brush = brushOpts(id =ns("plot_brush"),resetOnNew = TRUE), height = "65vh"), style="color:red"), 
    fluidRow(
      # column(2,numericInput(ns("linesize"), "Width of line in mm", value=0.5, min = 0.1, max = 10, step=0.1)), # does not work for now
      column(2, style = "margin-top: 25px;", downloadButton(ns('savePlot'), 'Save Plot')),
      column(3,style = "margin-top: 15px;",helpText("OPTIONAL: save plot with personalized width and height:",style="color:black;font-style: italic"),offset=3),
      column(2, numericInput(ns("widthmm"), "Width (mm)", value=NA), style = "font-size: 12px; font-style: italic"),
      column(2, numericInput(ns("heightmm"), "Height (mm)", value=NA),style = "font-size: 12px; font-style: italic"),#
    )
  )
}

# shinyModuleConfiguration <- function(id, input) {
#   ns <- NS(id)
#   configuration <- list()
#   configuration
# }

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  output$uiAttributeL <- renderUI({
    dataCC <- data@data[, colSums(is.na(data@data)) != nrow(data@data)] ## maybe look for a more effective way of doing this in case data set is very large
    dataCC <- dataCC[, sapply(dataCC, is.POSIXt) != TRUE] ## removing timestamp columns as these cannot be currently plotted
    dataCC <- dataCC[, sapply(dataCC, class) != "Date"]
    selectInput(ns("attributeL"), "Select attribute", choices=colnames(dataCC))})
  
  output$uiIndivL <- renderUI({
    # checkboxGroupInput(ns("indivL"), "Select individuals", choices=namesIndiv(data), selected=namesIndiv(data)[1], inline=TRUE)
    checkboxGroupButtons(ns("indivL"), "Select individuals", size="sm", choices=namesIndiv(data), selected=namesIndiv(data)[1],status="default",checkIcon = list(
      # yes = icon("check-square"), no = icon("square-o")))
      yes = icon("ok",lib = "glyphicon"))) #, no = icon("remove",lib = "glyphicon")
  })
  
  # observeEvent(input$selectall,{
  # observe({
  #       updateCheckboxGroupButtons(session=session, 
  #                                  ns("indivL"), 
  #                                  # "Select individuals", size="sm", 
  #                                  choices=namesIndiv(data),
  #                                  # selected=c(namesIndiv(data))
  #                                  selected=if (input$selectall) c(namesIndiv(data))
  #                                  # ,status="default",checkIcon = list(yes = icon("ok",lib = "glyphicon"))
  #                                  )
  #     # } else {
  #     #   updateCheckboxGroupInput(session=session, 
  #     #                            ns("indivL"), 
  #     #                            # "Select individuals", size="sm", 
  #     #                            # choices=namesIndiv(data), 
  #     #                            selected=c()
  #     #                            # ,status="default",checkIcon = list(yes = icon("ok",lib = "glyphicon"))
  #     #                            )
  #     # }}
  # })
  
  ## zoom into plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x_range <- c(brush$xmin, brush$xmax)
      ranges$y_range <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x_range <- NULL
      ranges$y_range <- NULL
    }
  })
  
  output$plot <- renderPlot({
    if(is.null(input$attributeL)){
      mDF <- 1 #data.frame(long=coordinates(data)[,1],lat=coordinates(data)[,2],attribute=data@data[,1], indiv=trackId(data))
    }else{
      mDF <- data.frame(long=coordinates(data)[,1],lat=coordinates(data)[,2],attribute=data@data[,input$attributeL], indiv=trackId(data))
      mDF <- mDF[mDF$indiv %in% c(input$indivL),]
      
      if(input$panels=="Single panel"){
        output$warningtext <- NULL
        if(is.numeric(mDF[, "attribute"])){ 
          minattr <- min(mDF[, "attribute"],na.rm=T)
          maxattr <- max(mDF[, "attribute"],na.rm=T)
          mpt <- (minattr+maxattr)/2
          
          if(class(mDF[, "attribute"])=="units"){
            ggplot(mDF) + 
              geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
              geom_path(aes(x=long, y=lat, colour=as.numeric(attribute), group=indiv))+ #, size=input$linesize
              scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=as.numeric(mpt), name=paste0(input$attributeL," (",deparse_unit(mDF[, "attribute"]),")")
                                     # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
              )+
              labs(x ="Longitude", y = "Latitude")+ 
              coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
          }else{
            ggplot(mDF) + 
              geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
              geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
             scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                     # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
             )+
              labs(x ="Longitude", y = "Latitude")+
              coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
          }
          
        }else{ 
          ggplot(mDF) + 
            geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
            geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
            scale_color_discrete(name=input$attributeL)+
            coord_fixed()+
            labs(x ="Longitude", y = "Latitude")+ 
            coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
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
            if(class(mDF[, "attribute"])=="units"){
              ggplot(mDF) + 
                geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
                geom_path(aes(x=long, y=lat, colour=as.numeric(attribute), group=indiv))+ #, size=input$linesize
                facet_wrap(~indiv, scales="free")+
                scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=as.numeric(mpt), name=paste0(input$attributeL," (",deparse_unit(mDF[, "attribute"]),")")
                                       # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
                )+
                # coord_fixed()+
                labs( x ="Longitude", y = "Latitude")+ #title=input$attributeL,
                coord_cartesian(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
            }else{
              ggplot(mDF) +
                geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
                geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
                facet_wrap(~indiv, scales="free")+
                scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
                                       # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
                )+
                # coord_fixed()+
                labs( x ="Longitude", y = "Latitude")+
                coord_cartesian(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
              
              ## to enable ratio 1/1, but if many individuals, than  useless for now
              # plotL <- lapply(split(mDF, mDF$indiv), function(x){
              #   p <- ggplot(x) + 
              #     geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
              #     geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+ #, size=input$linesize
              #     scale_colour_gradient2(low=input$colmin, mid=input$colmid, high=input$colmax, midpoint=mpt, name=input$attributeL
              #                            # , breaks=round(seq(minattr,maxattr,length.out=3),2),labels=round(seq(minattr,maxattr,length.out=3),2)
              #     )+
              #     labs(x ="Longitude", y = "Latitude", title=unique(x$indiv))+
              #     coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
              #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              #           panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
              #   return(p)
              # })
              # 
              # plot_grid(plotlist=plotL)
            }
          }else{ 
            ggplot(mDF) + 
              geom_point(aes(x=long, y=lat), colour="black", size=0.5,shape=20)+
              geom_path(aes(x=long, y=lat, colour=attribute, group=indiv))+
              facet_wrap(~indiv, scales="free")+
              scale_color_discrete(name=input$attributeL)+
              # coord_fixed()+
              labs( x ="Longitude", y = "Latitude")+ 
              coord_cartesian(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
          }
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

