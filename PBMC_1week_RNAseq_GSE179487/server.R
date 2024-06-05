library(shiny)	
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(shinyjs)

expression.mat <- readRDS("expression.rds")
meta.df <- readRDS("metadata.rds")

server <- function(input, output, session)
{
  output$gene <- renderUI({
    selectInput("gene", "Select Gene to Display", choices = rownames(expression.mat))
  })
  
  output$group <- renderUI({
    if(input$plotType == "Distribution Plot"){
      selectInput("group", "Select Treatment", choices = c("Treatment","Spatial"))
    }
  })
  
  scatter.plot <- reactive({
    scatter.plot <- NULL
    if(!is.null(input$gene)){
      gene.idx <- which(rownames(expression.mat) == input$gene)
      plot.df <- suppressWarnings(meta.df %>% dplyr::left_join(data.frame(Sample=colnames(expression.mat),value=expression.mat[gene.idx,]),by=c("Sample"="Sample")))
      scatter.plot <- suppressWarnings(plotly::plot_ly(marker=list(size=3),type='scatter',mode="markers",color=plot.df$value,x=plot.df$x,y=plot.df$y,showlegend=F,colors=colorRamp(c("lightgray","darkred"))) %>%
                                         plotly::layout(title=input$gene,xaxis=list(zeroline=F,showticklabels=F,showgrid=F),yaxis=list(zeroline=F,showticklabels=F,showgrid=F)) %>%
                                         plotly::colorbar(limits=c(min(plot.df$value,na.rm=T),max(plot.df$value,na.rm=T)),len=0.4,title="Expression (CPM)"))
    }
    return(scatter.plot)
  })
  
  distribution.plot <- reactive({
    distribution.plot <- NULL
    if(!is.null(input$gene) & !is.null(input$group)){
      gene.idx <- which(rownames(expression.mat) == input$gene)
      plot.df <- suppressWarnings(meta.df %>% dplyr::left_join(data.frame(Sample=colnames(expression.mat),value=expression.mat[gene.idx,]),by=c("Sample"="Sample")))
      if(input$group == "Treatment"){
        distribution.plot <- suppressWarnings(plotly::plot_ly(x=plot.df$Treatment,y=plot.df$value,split=plot.df$Treatment,type='box',box=list(visible=T),points=T,color=plot.df$Treatment,showlegend=F) %>%
                                                plotly::layout(title=input$gene,xaxis=list(title=input$group,zeroline=F),yaxis=list(tickfont = list(size = 15),title="Expression (CPM)",zeroline=F)))
      } else{
        plot.df <- plot.df %>% dplyr::mutate(Spatial=Spatial) %>% dplyr::arrange(Spatial)
        plot.df$Spatial <- factor(plot.df$Spatial,levels=unique(plot.df$Spatial))
        distribution.plot <- suppressWarnings(ggplot(plot.df,aes(x=Spatial,y=value)) +
                                                geom_boxplot(aes(color=Treatment)) +
theme_minimal() + ylab(paste0(input$gene," Expression (CPM)"))+theme(legend.title=element_blank()))
      }
    }
    return(distribution.plot)
  })
  
  output$out.plot_plotly <- plotly::renderPlotly({
    if(input$plotType == "Scatter Plot"){
      scatter.plot()
    } else {
      req(input$group)
      if (input$plotType == "Distribution Plot" && input$group != "Spatial"){
        distribution.plot()
      }
    }
  })
  
  output$out.plot_plot <- renderPlot({
    req(input$group)
    if (input$plotType == "Distribution Plot" && input$group == "Spatial") {
      distribution.plot()
    }
  })
  
  observeEvent(c(input$group, input$plotType), {
    req(input$group)
    if (input$group == "Spatial" && input$plotType == "Distribution Plot") {
      hide("out.plot_plotly")
      show("out.plot_plot")
    } else {
      hide("out.plot_plot")
      show("out.plot_plotly")
    }
  })
}