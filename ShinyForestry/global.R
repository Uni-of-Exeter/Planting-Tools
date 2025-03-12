map_and_slider_ui <- function(id, year_min, year_max, year_default, show_time_series = TRUE, panel_width = "37.5%") { # 37.5% is defined by (9/12) width in bootstrap * 50% 
  ns <- NS(id)  # Namespace all IDs properly
  
  absolutePanel(
    id = ns("year-slider"),
    sliderInput(ns("year"), "Planting Year", 
                min = year_min, 
                max = year_max, 
                value = year_default, 
                step = 1, 
                animate = TRUE,
                ticks = TRUE,  
                animateOptions(interval = 100, loop = FALSE),
                sep = "",
                width = "100%" 
    ),
    bottom = "50px", 
    left = "50%", 
    style = paste0("background-color: rgba(255, 255, 255, 0.9); padding: 10px 20px 10px 20px; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); width: ", panel_width, "; transform: translateX(-50%);"),
    
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%; height: 40px;",
      
      # Conditionally render the Time-Series section
      if (show_time_series) {
        actionButton(
          inputId = ns("toggle_plot"),
          label = "Show Time-Series",
          style = "
            width: 180px;
            height: 40px;
            line-height: 20px;
            display: flex;
            align-items: center;
            justify-content: center;
            margin-top: -17px;"
        )
      },
      
      # Always show the radio buttons for view toggle
      radioGroupButtons(
        inputId = ns("view_toggle"),
        choices = c("Annual", "Cumulative"),
        selected = "Cumulative",
        status = "primary",
        justified = TRUE,
        width = '220px'
      )
    ),
    
    # Time series plot inside the same div - initially hidden
    div(id = ns("time_series_plot"), 
        style = "display: none; margin-top: 10px;",  
        plotlyOutput(ns("areaPlot"), height = "300px")
    )
  )
}