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
      selectInput("group", "Select Condition", choices = c("Condition","Source", colnames(meta.df)))
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
  req(input$gene, input$group)  # Make sure both gene and group are selected

  gene.idx <- which(rownames(expression.mat) == input$gene)
  plot.df <- meta.df %>%
    left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))

  if (input$group == "Condition") {
    distribution.plot <- plot_ly(data = plot.df, x = ~Condition, y = ~value, color = ~Condition, type = "box", boxmean = "sd") %>%
      layout(title = input$gene, xaxis = list(title = "Condition"), yaxis = list(title = "Expression (CPM)"))
  } else if (input$group == "Source") {
    plot.df <- plot.df %>%
      mutate(Source = as.factor(Source)) %>%
      arrange(Source)
    distribution.plot <- ggplot(plot.df, aes(x = Source, y = value, color = Condition)) +
      geom_boxplot() +
      labs(x = "Source", y = "Expression (CPM)", title = input$gene) +
      theme_minimal()
  } else {
    distribution.plot <- ggplot(plot.df, aes_string(x = input$group, y = "value", color = "Condition")) +
      geom_boxplot() +
      labs(x = input$group, y = "Expression (CPM)", title = input$gene) +
      theme_minimal()
  }

  return(distribution.plot)
})
  
  output$out.plot_plotly <- plotly::renderPlotly({
    if(input$plotType == "Scatter Plot"){
      scatter.plot()
    } else {
      req(input$group)
      if (input$plotType == "Distribution Plot" && input$group != "Source"){
        distribution.plot()
      }
    }
  })
  
  output$out.plot_plot <- renderPlot({
    req(input$group)
    if (input$plotType == "Distribution Plot" && input$group == "Source") {
      distribution.plot()
    }
  })
  
  observeEvent(c(input$group, input$plotType), {
    req(input$group)
    if (input$group == "Source" && input$plotType == "Distribution Plot") {
      hide("out.plot_plotly")
      show("out.plot_plot")
    } else {
      hide("out.plot_plot")
      show("out.plot_plotly")
    }
  })
}