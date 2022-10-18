library(DT)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(purrr)
library(glue)
library(gtools)

# Read all RDS files in data folder, assign to variables named from file name
list.files("./data/", ".rds", full.names = TRUE) %>%
  walk(~ assign(gsub(".rds", "", basename(.x)), readRDS(.x), envir = .GlobalEnv))

# Human-friendly descriptions of the values...
descriptions <- c(
  "Referrals in"           = "total_refs_in",
  "Referrals out"          = "total_refs_out",
  "Patients treated"       = "total_treated",
  "Patients waiting"       = "total_waiting",
  "Avg. wait (non-urgent)" = "avg_wait_non_urgent"
)

# ...and mapping backwards
col_to_desc <- setNames(names(descriptions), descriptions)

# Creates an info symbol with tooltip
info_tooltip <- function(content, style = "left-padding: 1px", ...) {
  span(
    class = "info-tippy", style = style, `data-tippy-content` = content,
    HTML("&#x1F6C8;")
  )
}

header <- dashboardHeader(
  title = "London Oral Surgery Waiting Times Map",
  titleWidth = 500
)

body <- dashboardBody(
  tags$head(
    HTML("<html lang='en'>"),
    tags$script(type = "text/javascript", src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(type = "text/javascript", src = "https://unpkg.com/tippy.js@6"),
    tags$script("tippy.setDefaultProps({theme: 'nhs-blue', allowHTML: true});"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "resizer.js") # Loads the javascript that controls the 
                                    # resizing of map and table to fit the app
                                    # exactly to the viewport size :)
  ),
  fluidRow(
    column(
      width = 6,
      box(
        width = NULL,
        solidHeader = TRUE,
        leafletOutput("london_map")
      )
    ),
    column(
      width = 6,
      div(
        box(
          width = NULL,
          fluidRow(
            id = "controls",
            column(
              width = 4,
              selectInput(
                "month_select",
                "Month",
                choices = months,
                width = 150
              )
            ),
            column(
              width = 4,
              radioButtons(
                "service",
                "Service",
                c("Endodontics", "IMOS")
              )
            ),
            column(
              width = 4,
              radioButtons(
                "highlight",
                tagList(
                  "Map Highlight",
                  info_tooltip(
                    "Choose what to highlight on map - the darker the colour, the 
                    higher the measure is for a borough relative to other boroughs."
                  )
                ),
                descriptions
              ),
            )
          )
        )
      ),
      box(
        width = NULL,
        dataTableOutput("table", width = "auto")
      ),
      tags$script("tippy('.info-tippy');")
    )
  ),
  tags$footer(
    div(
      id = "footer",
      style = 'margin-left: 14px;',
      "This Dashboard was produced by the NHSBSA Management Information Team.",
      HTML("&nbsp;"),
      "If you have any queries, please contact us at",
      a(
        "nhsbsa.managementinformation@nhs.net.",
        href = "mailto:nhsbsa.managementinformation@nhs.net"
      ),
      HTML("&nbsp;&nbsp;&nbsp;"),
      a(
        "Our accessibility statement",
        href = glue(
          "https://nhsbsauk.sharepoint.com/sites/Accessibility/SitePages/",
          "Accessibility-statement-for-Management-Information-RShiny-Dashboards.aspx"
        ),
        target = "blank"
      )
    )
  )
)

ui <- {
  dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
  )
}

server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  get_data <- reactive({
    
    data <- data %>%
      filter(
        month   == (as.Date(input$month_select) %m+% months(1)),
        service == input$service
      )
    
    # The RHS of the join is just the boroughs in the correct order for drawing
    # on the map
    boroughs@data <- boroughs@data %>%
      select(borough) %>%
      left_join(data, by = c("borough" = "borough"))
    
    boroughs
  })
  
  # Due to use of leafletProxy below, this will only be called once
  output$london_map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(
        attribution = '
          Contains
          <a href="https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london"
          target=_blank>
          National Statistics data</a> &copy; Crown copyright and database right [2015]
          Contains
          <a href="https://www.ordnancesurvey.co.uk/business-government/licensing-agreements/copyright-acknowledgements"
          target=_blank>
          Ordnance Survey data</a> &copy; Crown copyright and database right [2015]
        '
      ) %>%
      setView( # Centre the map in the middle of our co-ordinates
        mean(bounds[1,]),
        mean(bounds[2,]),
        zoom = 10 # highest zoom that will show the whole area on load
      )
  })
  
  observe({
    data <- get_data()
    
    # Colour palette mapped to chosen map highlight column
    pal <- colorNumeric("YlGn", data[[input$highlight]])
    
    # Fix colour palette mapped to chosen map highlight column for legend; NAs 
    # are removed. See https://github.com/rstudio/leaflet/issues/615 for issue
    palWithoutNA <- colorNumeric("YlGn", data[[input$highlight]], na.color=rgb(0,0,0,0))
    
    # If the data changes, the polygons are cleared and redrawn; however the map
    # is not redrawn
    leafletProxy("london_map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        data = data,
        fillColor = pal(data[[input$highlight]]),
        fillOpacity = 0.8,
        color ='black',
        dashArray ='3',
        weight = 2,
        label = ~label %>% lapply(HTML)
      ) %>%
      clearControls() %>%
      addLegend(
        "bottomright", 
        pal = palWithoutNA, 
        values = ~data[[input$highlight]],
        title = ~unname(col_to_desc[input$highlight]),
        labFormat = labelFormat(),
        opacity = 1,
        na.label = ""
      )
  })
  
  # Due to lotnum being of mixed type (i.e. alphanumeric) getting the table to sort
  # lexicographically took a lot of trial and error. The trick used here is to 
  # explicitly set the rownames in the table to the sequence 1:number of rows. Now
  # using columnDefs allows to specify that when sorting the lotnum column, to 
  # instead sort it according to the (hidden) rownames. Take note of 1-based indexing
  # for R, but 0-based indexing for anything that is passed into the client browser!
  output$table <- renderDataTable({
    data <- get_data()@data %>%
      select(-c(month, service, label)) %>%
      filter_at(-1, any_vars(!is.na(.)))
    
    data <- data %>% 
      arrange(borough) %>%
      arrange(order(mixedorder(lotnum)))
    
    names(data) <- c("Borough", "Lot", names(descriptions))
    
    table <- data %>%
      datatable(
        options = list(
          orderClasses = TRUE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollY = "100vh",
          scrollX = "100vw",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(orderable = TRUE, targets = 0),
            list(orderData = 0, targets = 2),
            list(visible = FALSE, targets = 0)
          )
        )
      )

    table$x$data[[1]] <- 1:nrow(data)
    
    table
  }, server = TRUE)
}

shinyApp(ui, server)
