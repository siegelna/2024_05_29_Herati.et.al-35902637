library(shiny)  
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(shinyjs)

expression.mat <- readRDS("expression.rds")
meta.df <- readRDS("metadata.rds")

server <- function(input, output, session) {
  
  output$gene <- renderUI({
    selectInput("gene", "Select Gene to Display", choices = rownames(expression.mat))
  })
  
  output$group <- renderUI({
      selectInput("group", "Select Condition", choices = c("Condition", setdiff(colnames(meta.df), "Sample")))
  })
  
  output$y_axis <- renderUI({
    selectInput("y_axis", "Select Y-Axis Column", choices = c("Condition",  setdiff(colnames(meta.df), "Sample")))
  })

  observe({
    if (!is.null(input$group)) {
      updateSelectInput(session, "y_axis", selected = "Condition")
    }
  })
  
  output$facet <- renderUI({
    if(input$plotType == "Distribution Plot" && !is.null(input$group)) {
      choices <- c("None", setdiff(colnames(meta.df), c("Sample", "Condition")))
      selectInput("facet", "Select Grouping Variable", choices = choices, selected = "Cell_Type")
    }
  })
  
  scatter.plot <- reactive({
    scatter.plot <- NULL
    if (!is.null(input$gene)) {
      gene.idx <- which(rownames(expression.mat) == input$gene)
      plot.df <- suppressWarnings(
        meta.df %>%
          left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))
      )
      scatter.plot <- plotly::plot_ly(data = plot.df,
                                      type = 'scatter',
                                      mode = "markers",
                                      color = ~value,
                                      x = ~value,
                                      y = plot.df[[input$y_axis]], 
                                      showlegend = FALSE,
                                      colors = colorRamp(c("lightgray", "darkred")),
                                      marker = list(size = 3))
    }
    return(scatter.plot)
  })

  
  distribution.plot <- reactive({
    req(input$gene, input$group, input$facet)  # Make sure gene, group, and facet are selected
    
    gene.idx <- which(rownames(expression.mat) == input$gene)
    plot.df <- meta.df %>%
      left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))
    
    plot.df <- plot.df %>%
      mutate_at(vars(input$group), factor) %>%
      arrange(.data[[input$group]])
    
    distribution.plot <- ggplot(plot.df, aes(x = .data[[input$group]], y = value, fill = .data[[input$group]])) +
      geom_boxplot(color = "black") +  # Add color borders
      labs(y = "Expression (CPM)", title = input$gene) +
      theme_minimal() +
      theme(legend.position = "top", axis.text.x = element_text(angle = 35, hjust = 1)) +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.x = element_text(size = 10),  axis.text.y = element_text(size = 10))
    
    if (input$facet != "None") {
      distribution.plot <- distribution.plot + 
        facet_wrap(vars(.data[[input$facet]]), scales = "free_x", labeller = label_both, nrow = 1)
    }
    
    return(distribution.plot)
  })
  
  output$out.plot_plotly <- plotly::renderPlotly({
    if(input$plotType == "Scatter Plot"){
      scatter.plot()
    } else {
      req(input$group)
      distribution.plot()
    }
  })
  
  observeEvent(c(input$group, input$plotType), {
    req(input$group)
    if (input$plotType == "Distribution Plot") {
      hide("out.plot_plotly")
      show("out.plot_plot")
    } else {
      hide("out.plot_plot")
      show("out.plot_plotly")
    }
  })
}