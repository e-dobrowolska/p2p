#' psp -- data aggregation Shiny app
#'
#' A function that opens an interactive inferface for aggregating and downloading data.
#'
#' @author Ewa Dobrowolska
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_bbox
#' @importFrom sf st_intersects
#' @importFrom sf st_union
#' @importFrom sf st_within
#' @importFrom sf st_length
#' @importFrom sf st_cast
#' @importFrom sf st_intersection
#' @importFrom stats aggregate
#' @importFrom shiny fluidPage
#' @importFrom shiny shinyApp
#' @importFrom shiny titlePanel
#' @importFrom shiny includeMarkdown
#' @importFrom shiny p
#' @importFrom shiny mainPanel
#' @importFrom shiny selectInput
#' @importFrom shiny actionButton
#' @importFrom shiny numericInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny conditionalPanel
#' @importFrom shiny fileInput
#' @importFrom shiny downloadButton
#' @importFrom shiny tableOutput
#' @importFrom shiny radioButtons
#' @importFrom shiny plotOutput
#' @importFrom shiny observeEvent
#' @importFrom shiny selectInput
#' @importFrom shiny insertUI
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny updateSelectInput
#' @importFrom shiny eventReactive
#' @importFrom shiny reactive
#' @importFrom shiny renderTable
#' @importFrom shiny downloadHandler
#' @importFrom shiny renderPlot
#' @importFrom shiny updateRadioButtons
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggplot2 scale_color_viridis_c
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom utils write.csv
#' @importFrom utils head
#' @importFrom rgdal readOGR
#' @importFrom osmdata opq
#' @importFrom osmdata available_features
#' @importFrom osmdata available_tags
#' @importFrom osmdata add_osm_feature
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @examples
#' @export

p2p<-function(){

  ui <- fluidPage(

    titlePanel(p("Spatial data aggregation", style = "color:#3474A7")),
    includeMarkdown(system.file("rmd", "RMarkdownFile.Rmd", package="p2p")),
    mainPanel(
      selectInput(
        inputId = "aggtype",
        label = "Type of aggregation:",
        choices = c("Points to polygons", "Polygons to polygons - area", "Polygons to polygons - intersection",
                    "Polygons to polygons - distance", "Lines to polygons", "Points to points", "Polygons to points - area",
                    "Polygons to points - intersection", "Polygons to points - distance", "Lines to points"),
        selected = "Points to polygons",
        #multiple = FALSE,
        selectize = FALSE,
        size = 1,
        width = '100%'
      ),
      selectInput(
        inputId="key0",
        label="Key name:",
        choices=available_features(),
        width = '100%',
        multiple=FALSE,
        selected="amenity"),
      selectInput("value0", "Value names:", choices=NULL, width = '100%', multiple=TRUE),

      actionButton("add", "Add new key", style = "color: #ffffff; background-color: #3B2D93;
        border-color: #000000"),

      numericInput("sleep", "Sleep time", 1, 0, Inf, width = '100%'),
      checkboxInput("binary", "Binary form?", value = F, width = '100%'),
      checkboxInput("multi", "Include multiobjects?", value = T, width = '100%'),

      conditionalPanel(
        condition = "input.aggtype == 'Points to points' | input.aggtype == 'Polygons to points - area' | input.aggtype == 'Polygons to points - intersection' | input.aggtype == 'Lines to points'",
        numericInput("radius", "Radius:", 0, 0, Inf, width = '100%')),

      conditionalPanel(
        condition = "input.aggtype == 'Polygons to polygons - intersection' | input.aggtype == 'Polygons to points - intersection' |
    input.aggtype == 'Polygons to polygons - distance' | input.aggtype == 'Polygons to points - distance'",
        numericInput("minsize", "Minimal polygon size", 0, 0, Inf, width = '100%')),

      conditionalPanel(
        condition = "input.aggtype == 'Points to polygons' | input.aggtype == 'Points to points'",
        checkboxInput("outside", "Only points outside polygons?", value = F, width = '100%')),

      fileInput(inputId = "filedata",
                multiple=TRUE,
                label = "Upload polygons or points. Choose a shapefile:",
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      actionButton(
        "Start",
        label="Start",
        width = "100px",
        style = "color: #ffffff; background-color: #3B2D93;
        border-color: #000000"
      ),
      downloadButton("downloadData", "Download"),

      conditionalPanel(
        condition = "input.Start>0",
        radioButtons("radio", "Variable plotted:", choices="NULL", width = '100%')),

      tableOutput("preview"),
      plotOutput("plot"),
    ))

  server <- function(input, output) {


    observeEvent(input$add, {
      insertUI(
        selector = "#add",
        where = "beforeBegin",
        selectInput(
          paste0("key", input$add),
          label="Key name:",
          choices=available_features(),
          width = '100%',
          multiple=FALSE,
          selected="amenity"))
      insertUI(
        selector = "#add",
        where = "beforeBegin",
        selectInput(paste0("value", input$add), "Value names:", choices=NULL, width = '100%', multiple=TRUE))
    })

    observeEvent(input[[paste0("key", input$add)]], {
      updateSelectInput(session = getDefaultReactiveDomain(), paste0("value", input$add), choices = available_tags(input[[paste0("key", input$add)]]))
    })

    region<-eventReactive(input$Start, {

      region<-input$filedata
      tempdirname <- dirname(region$datapath[1])
      for (i in 1:nrow(region)) {
        file.rename(
          region$datapath[i],
          paste0(tempdirname, "/", region$name[i])
        )
      }

      region <- readOGR(paste(tempdirname,
                              region$name[grep(pattern = "*.shp$", region$name)],
                              sep = "/"
      ))
      region
    })

    value<-reactive({
      value<-input$value0
      if(input$add>0)  for(i in 1:input$add)  value<-c(value, input[[paste0("value", i)]])
      value
    })

    dataf<-eventReactive(input$Start, {


      key<-rep(input$key0, length(input$value0))
      if(input$add>0)  for(i in 1:input$add)  key<-c(key, rep(input[[paste0("key", i)]], length(input[[paste0("value", i)]])))


      if(input$aggtype=="Points to polygons")
      {
        df<-pts2poly(key, value(), region(), input$sleep, input$binary, input$outside, input$multi)
      }

      if(input$aggtype=="Polygons to polygons - area")
      {
        df<-poly2poly_a(key, value(), region(), input$sleep, input$binary, input$multi)
      }

      if(input$aggtype=="Polygons to polygons - intersection")
      {
        df<-poly2poly_i(key, value(), region(), input$sleep, input$binary, input$multi, input$minsize)
      }

      if(input$aggtype=="Polygons to polygons - distance")
      {
        df<-poly2poly_d(key, value(), region(), input$sleep, input$binary, input$multi, input$minsize)
      }

      if(input$aggtype=="Lines to polygons")
      {
        df<-lines2poly(key, value(), region(), input$sleep, input$binary, input$multi)
      }

      if(input$aggtype=="Polygons to points - area")
      {
        df<-poly2pts_a(key, value(), region(), radius=input$radius, input$sleep, input$binary, input$multi)
      }

      if(input$aggtype=="Polygons to points - intersection")
      {
        df<-poly2pts_i(key, value(), region(), radius=input$radius, input$sleep, input$binary, input$multi, input$minsize)
      }

      if(input$aggtype=="Polygons to points - distance")
      {
        df<-poly2pts_d(key, value(), region(), input$sleep, input$binary, input$multi, input$minsize)
      }

      if(input$aggtype=="Points to points")
      {
        df<-pts2pts(key, value(), region(), radius=input$radius, input$sleep, input$binary, input$outside, input$multi)
      }

      if(input$aggtype=="Lines to points")
      {
        df<-lines2pts(key, value(), region(), radius=input$radius, input$sleep, input$binary, input$multi)
      }

      df
    })

    output$preview <- renderTable({
      head(dataf())
    })

    observeEvent(input$Start, {
      updateRadioButtons(session = getDefaultReactiveDomain(), inputId = "radio", choices = names(dataf())[2:ncol(dataf())])
    })

    output$plot <- renderPlot({
      region.sf<-st_as_sf(region())
      region.sf$dataf<-dataf()[,input$radio]

      ggplot()+
        geom_sf(region.sf, mapping=aes(geometry=region.sf$geometry, fill=dataf, col=dataf))+
        ggtitle(paste0("Aggregated objects: ", names(dataf()))[2])+
        labs(paste0(names(dataf())))+
        scale_fill_viridis_c(option = "plasma")+
        scale_color_viridis_c(option = "plasma")+
        theme_minimal()+
        theme(plot.title=element_text(hjust=0.5))
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(dataf(), file)
      }
    )

  }

  shinyApp(ui = ui, server = server)
}
