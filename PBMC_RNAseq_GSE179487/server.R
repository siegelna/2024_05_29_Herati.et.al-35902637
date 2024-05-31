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
  
output$fill <- renderUI({
  if(input$plotType == "Distribution Plot" && !is.null(input$group)) {
    choices <- c("None", "Condition", setdiff(colnames(meta.df), "Sample"))
    selectInput("fill", "Select Fill", choices = choices, selected = "None")
  }
})

observe({
  if (!is.null(input$group)) {
    updateSelectInput(session, "fill", selected = "Condition")
  }
})

output$facet <- renderUI({
  if(input$plotType == "Distribution Plot" && !is.null(input$group)) {
    choices <- c("None", "Condition", setdiff(colnames(meta.df), "Sample"))
    selectInput("facet", "Select Grouping Variable", choices = choices, selected = "None")
  }
})

observe({
  if (!is.null(input$group)) {
    updateSelectInput(session, "facet", selected = "Condition")
  }
})

###
output$facet2 <- renderUI({
  if(input$plotType == "Distribution Plot" && !is.null(input$group)) {
    choices <- c("None", "Condition", setdiff(colnames(meta.df), "Sample"))
    selectInput("facet2", "Select Additional Grouping Variable", choices = choices, selected = "None")
  }
})

###

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
  req(input$gene, input$group, input$fill, input$facet)  # Make sure gene, group, and fill are selected
  
  gene.idx <- which(rownames(expression.mat) == input$gene)
  plot.df <- meta.df %>%
    left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))
  
  plot.df <- plot.df %>%
    mutate_at(vars(input$group), factor) %>%
    arrange(.data[[input$group]])
  
  distribution.plot <- ggplot(plot.df, aes(x = .data[[input$group]], y = value, fill = .data[[input$fill]])) +
    geom_boxplot(color = "black") +  # Add color borders
    labs(y = "Expression (CPM)", title = input$gene) +
    theme_minimal() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 35, hjust = 1)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(size = 10),  axis.text.y = element_text(size = 10))
  
  # Conditionally include facet2 if it is not None
  if (input$facet2 != "None") {
    distribution.plot <- distribution.plot + 
      facet_wrap(vars(reorder(.data[[input$facet2]], .data[[input$facet]])), scales = "free_x", labeller = label_value, nrow = 1)
  } else {
    distribution.plot <- distribution.plot + 
      facet_wrap(vars(.data[[input$facet]]), scales = "free_x", labeller = label_both, nrow = 1)
  }
  
  # Define a custom function to remove specific text from x-axis labels
custom_label <- function(x) {
  remove_patterns <- c(input$facet, input$facet2)
  
  # Iterate over each pattern to remove
  for (pattern in remove_patterns) {
    if (pattern %in% names(plot.df)) {
      unique_values <- unique(plot.df[[pattern]])
      
      # Escape special characters in unique values
      unique_values_escaped <- gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\[\\{\\\\\\|])", "\\\\\\1", unique_values, perl = TRUE)
      
      # Construct the regular expression pattern
      pattern_regex <- paste(unique_values_escaped, collapse = "|")
      
      # Remove the pattern from x
      x <- gsub(pattern_regex, "", x)
    }
  }
  
  # Remove underscores
  x <- gsub("_", " ", x)
  
  return(x)
}
  
  # Apply the custom label function to x-axis
  distribution.plot <- distribution.plot + 
    scale_x_discrete(labels = custom_label)
  
  return(distribution.plot)
})
  
  output$out.plot_plotly <- plotly::renderPlotly({
    if(input$plotType == "Scatter Plot"){
      scatter.plot()
    } else {
      req(input$group, input$fill)
      if (input$plotType == "Distribution Plot"){
        distribution.plot()
      }
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
