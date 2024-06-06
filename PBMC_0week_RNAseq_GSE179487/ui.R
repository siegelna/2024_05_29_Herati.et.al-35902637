# ui.R

ui <- fluidPage(
  titlePanel("Cancer Anti-PD1 Flu vaccine PBMC RNA-seq "),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(
          HTML(
            ".multicol {
                  -webkit-column-count: 3; /* Chrome, Safari, Opera */
                  -moz-column-count: 3; /* Firefox */
                  column-count: 3;
            }"
          )
        ),
        tags$style(
          type = "text/css",
          "#loadmessage {
                  position: fixed;
                  top: 0px;
                  left: 0px;
                  width: 100%;
                  padding: 5px 0px 5px 0px;
                  text-align: center;
                  font-weight: bold;
                  font-size: 100%;
                  color: #000000;
                  background-color: #CCFF66;
                  z-index: 105;
          }"
        ),
        tags$style(
          type = "text/css",
          ".shiny-output-error { 
              visibility: hidden; 
          }",
          ".shiny-output-error:before { 
              visibility: hidden; 
          }"
        ),
        tags$script("
          $(document).ready(function(){
            $('select[multiple]').on('click', 'option', function (e) {
              $(this).prop('selected', !$(this).prop('selected'));
              e.stopPropagation();
            });
          });
        ")
      ),
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$div("In Progress...", id = "loadmessage")
      ),
      selectInput("plotType", "Plot Type", choices = c("Distribution Plot", "Scatter Plot")),
      uiOutput("gene"),
      uiOutput("group"),
      uiOutput("facet"),
      uiOutput("fill"),
      conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        uiOutput("y_axis")
      ),
      conditionalPanel(
        condition = "input.plotType == 'Distribution Plot' && input.group != 'None'",
        selectizeInput("additional_group", "Subset Grouping Variable", choices = NULL, multiple = TRUE)
      )
    ),
    mainPanel(
      fluidRow(
        column(width = 9, plotly::plotlyOutput("out.plot_plotly", height = "900px")),
        column(width = 6, dataTableOutput("out_plot_table"))  # Display the table
      ),
      downloadButton("download_data", "Download Data")
    )
  )
)
