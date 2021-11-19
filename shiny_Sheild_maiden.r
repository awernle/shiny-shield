# Run this from the command line with:
#   R -e "shiny::runApp('app.R', port = getOption('shiny.port', 1112), host = getOption('shiny.host', '127.0.0.5') )"
#
# Then open a browser window and enter the address 127.0.0.5:1112
# =============================== Load Libaries ===============================
library(shiny)
library(shinyWidgets)
library(leaflet)
library(datasets) 
library(ggplot2)
library(shinyjs)
library(scales)
library(sf)
library(dplyr)
library(grid)
library(plotly)
library(tidyverse)
library(timevis)
library(rgdal)

env = "py35_env" 
reticulate::virtualenv_create(envname = env, python = "python3")
reticulate::virtualenv_install(env,
    packages = c(
        'numpy', 
        'pandas', 
        'gspread', 
        'gspread-dataframe',
        'oauth2client'
    )
)
reticulate::use_virtualenv(env, required = TRUE) 


# ----- Source some of the helper functions defined elsewhere
#   
source("formatting.R")
# ------------------------------ Define Functions -----------------------------
# Python functions
reticulate::source_python("python_functions.py", envir = globalenv() ) # # globalenv(), NULL, parent.frame()

# Functions defined in the R language
source("functions.R")

# ============================ Set Global Variables ===========================
# load the globals
source("globals.R")
# We're working on more globals which we will continue to append to the globals.R file

# filter_btns <- descr$groups[descr$groups$ID == unique(descr$group$ID)[1],2]

#Define data for timetable

Name_loc <- which(nchar(geophysics$GeophysicsSiteVisitDate) > 4 )
content <- (geophysics$SiteName[Name_loc])
start <- (geophysics$GeophysicsSiteVisitDate[Name_loc])
id <- seq( 1, length(Name_loc) )
d <- cbind(id, content, start)
colnames(d) <- c("id", "content", "start")
tt_data <- data.frame(d)

# =============================== Create the UI ===============================

ui <- fluidPage( #theme = "style.css",
    # ------------------------------ Define CSS -------------------------------
    shinyjs::useShinyjs(),

     includeCSS("style.css"),

    # # --------------------------------- Title ---------------------------------
    setBackgroundColor(bg_color),
    titlePanel("School Seismic Safety Project 2019-2021"),

    # ------------------------------- Left Side -------------------------------
    column(6,
        # ------- Build the sidebar to toggle data and display overall progress
        column(8,
        
            h3("Overall Progress:"),
            column(2),
            # Add some overall descriptions of the project
            h4( paste("Schools (surveyed/confirmed/solicited): ", descr$schools) ),
            column(2),
            h4( paste("Districts (confirmed/solicited): ", descr$districts) ),
            column(2),
            h4( paste("Buildings (inspected/confirmed/solicited): ", descr$bldgs) )
        ),
        column(12,
            plotlyOutput("progressplot", height = "500px", width = "80%")
        ),

        # ------------- Search functionality
        column(2,
            h4( paste("Search District or Site")),
            textInput("did", "ID", width = '80px'),
            textInput("key", "Keyword", width = '100px'),
            actionButton("search", "Go")
        ),

        column(5,
            htmlOutput("text1", style = "height:500px" )
        ),
        column(5,
            htmlOutput("text2", style = "height:500px" )
        )
    ),

    # ------------------------------ Right Side -------------------------------

    column(6,
        column(6,
            actionButton("reset", "Restore Defaults")
        ),
        # Create the map input
        column(12,
            leafletOutput("partymap", height = map_height),
            p(),
        ),
        
        column(2, 
            style='border-right:1px solid;',
            radioButtons("cluster", "Cluster",
                choices = c("Yes", "No")
            )
        ),
        # Select which group we want to visualize 
        column(2,
            selectInput("selection_group", "Group", 
                c("Participants","Alternates","Withheld"), width = '150px' 
            )
        ),
        column(2,
            style='border-right:1px solid;',
            # Choose popup data for engineers or geophysicists
            radioButtons(
                "plotdata", "Plot data", 
                choices = list("Geophysics"= "geo", "Engineering"= "eng")
            )
        ),
        column(2,
            # We can filter by the categories on the left to display specific 
            # points of interest
            selectInput("filter_cat", "Select Filter",
                choices = c("None", unique(descr$groups$ID) ), 
                width = '175px'
            )
        ),
        column(2,
            checkboxGroupInput(
                "filter_checkbox", "ID",
                choices = filter_checks
                )
        ),
        column(1,
            actionButton("dofilt","Filter")
        )
    ),

    # ------------------------------ Bottom -------------------------------
    column(12,
            # Build the timetable plot
            timevisOutput("timeline"),
    )
         
)    



# ============================= Define the Server =============================
server <- function(input, output, session) {

    title <-
  shiny::tags$div(HTML('<h3>Schools</h3>')) # add shiny library to use "tags"

    # -------------------------------- Sidebar --------------------------------
    output$progressplot <- renderPlotly({
        outputbar <- makestatsbar(descr$groups, bargroup_colors )
        g <- ggplotly(outputbar) %>%
            config(displayModeBar = F)
        # ggplotly(outputbar) %>% config(displayModeBar = F)
        # When converting ggplot to plotly the values change from counts to frequency 
        # We want to see counts 
        n <- length(g$x$data)
        for(i in 1:n)
        {
            temps <- g$x$data[[i]]$text
            for(j in 1:length(temps) )
            {
                vals <- unlist(str_split(temps[j], "<br />"))
                m <- length(vals)
                val_id <- unlist(str_split(vals[1], ": ") )[2]
                val_identity <- unlist(str_split(vals[3], ": " ) )[2]

                count_val <- descr$groups[ descr$groups$ID == val_id,]
                count_val <- count_val[ count_val$Identity == val_identity,3]
                vals[2] <- paste("Value:", count_val)
                g$x$data[[i]]$text[j] <- paste(vals[1], "<br />",
                                            vals[2], "<br />",
                                            vals[3], sep = "")
            }
        }
        return(g)
    })

    # -------------------------- Main panel business --------------------------
    #  the plot data input to define the global variable 'popup_df'

    # Sometimes we need to reload everything
    observeEvent( input$reset, {
        source("defaults.R")

        if(input$plotdata == "geo")
        {
            plotdata_old <- "eng"
        }
        # Clustering to Yes
        updateRadioButtons(
            session, 
            "cluster", 
            choices = c("Yes", "No") )
        # Plot data to Geophysics
        updateRadioButtons(
            session, 
            "plotdata",
            choices = list("Geophysics"= "geo", "Engineering"= "eng") 
        )
        # Selection group
        updateSelectInput(
            session,
            "selection_group",
            choices = c("Participants","Alternates","Withheld")
        )
        # Filter and filter id to None
        updateSelectInput(
            session, 
            "filter_cat", 
            choices = c("None", unique(descr$groups$ID) )
        )
        updateCheckboxGroupInput(
            session, 
            "filter_checkbox", 
            choices = filter_checks 
        )

        marker_colors <- rep("blue", dim(input_data)[1] )

    })

    # ---------- Selected Schools Map --------------
    v <- reactiveValues(data = popup)
    
    observe({
        data <- create_popups(input$plotdata == "eng", input$selection_group)
        input_data <<- data$planning 
        v$data <<- data$popup
        marker_colors <<- data$colors
        if(input$plotdata != plotdata_old)
        {
            lon_min <<- input$partymap_bounds$west
            lon_max <<- input$partymap_bounds$east
            lat_min <<- input$partymap_bounds$south 
            lat_max <<- input$partymap_bounds$north 
            plotdata_old <<- input$plotdata
        }
    })

    # --------- Filter
    # We can't fun the filter until at least one checkbox is satisifed
    observe({
        if( !is.null(input$filter_checkbox) & length(input$filter_checkbox) >= 1 )
        {
            shinyjs::enable("dofilt")
        }else
        {
            shinyjs::disable("dofilt")
        }
    })
    
    observe({
        if(input$filter_cat != "None" )
        {
            filter_checks <<- descr$groups[descr$groups$ID == input$filter_cat,2]
            updateCheckboxGroupInput(session, "filter_checkbox", choices = filter_checks )
        }
    })

    observeEvent( input$dofilt, {
        filter_id <<- input$filter_cat 
        filter_filt <<- input$filter_checkbox
        data <- create_popups(input$plotdata == "eng", input$selection_group)
        input_data <<- data$planning 
        v$data <<- data$popup
        marker_colors <<- data$colors

        lon_min <<- input$partymap_bounds$west
        lon_max <<- input$partymap_bounds$east
        lat_min <<- input$partymap_bounds$south 
        lat_max <<- input$partymap_bounds$north 
    })

    # -------------- Leaflet Map    
    # Check for clustering 
    cv <- reactiveValues(data=cluster)
    observe({
        cv$data <<- input$cluster == "Yes"
        if( cv$data != cluster)
        {
            lon_min <<- input$partymap_bounds$west
            lon_max <<- input$partymap_bounds$east
            lat_min <<- input$partymap_bounds$south 
            lat_max <<- input$partymap_bounds$north 
            cluster <<- cv$data
        }
    })

    output$partymap <- renderLeaflet({
        if(cv$data)
        {   
            leaflet(input_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data = sd_boundaries, fill = FALSE, stroke = TRUE, color = "black", weight = 0.5) %>%
            # Add circle markers
            fitBounds(  
                lng1 = lon_min,
                lat1 = lat_min,
                lng2 = lon_max,
                lat2 = lat_max ) %>%
            addCircleMarkers(
                stroke = FALSE,
                fillOpacity = 0.85,
                radius = 4,
                color = marker_colors,
                lng = ~Longitude, 
                lat = ~Latitude, 
                popup = v$data,
                clusterOptions = markerClusterOptions() ) %>%
            addMiniMap() 
        }else
        {
            leaflet(input_data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
                data = sd_boundaries, 
                fill = FALSE, 
                stroke = TRUE,
                color = "#03F", 
                weight = 0.5) %>%
            # Add circle markers
            fitBounds(  
                lng1 = lon_min,
                lat1 = lat_min,
                lng2 = lon_max,
                lat2 = lat_max ) %>%
            addCircleMarkers(
                stroke = FALSE,
                fillOpacity = 0.85,
                radius = 4,
                color = marker_colors,
                lng = ~Longitude, 
                lat = ~Latitude, 
                popup = v$data ) %>%
            addMiniMap() 
        }
    })

    # --------------------------------- Search --------------------------------
    query <- eventReactive( input$search, {
        # the inputs are strings. This makes it easy to determine if we want to 
        # lookup 
        if( is.null(input$did) || nchar(input$did) == 0 )
        {
            msg <- "No ID value given"
        }else if(nchar(input$did) < 5 )
        {
            msg <- "District ID" 
        }else if(nchar(input$did) == 5 )
        {
            msg <- siteidsearch(input$did) 
        }else 
        {
            msg <- "Improper use of my time"
        }
    })

    output$text1 <- renderUI({
        query()$general
    })

    output$text2 <- renderUI({
        query()$buildings
    })
    
    # ------------------------------ Bottom -------------------------------
    {
    output$timeline <- renderTimevis({
        timevis(tt_data, fit = FALSE,)
    })
    }
}

shinyApp(ui,server)

