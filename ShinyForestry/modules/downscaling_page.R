downscaling_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = "downscaling-page",
        # Ensure full height and center alignment
        div(class = "full-height",
            fluidRow(class = "d-flex align-items-center justify-content-center",  # Center images
                     column(6, div(class = "d-flex justify-content-center", 
                                   img(src = "https://netzeroplus.ac.uk/wp-content/uploads/2023/09/Exeter_colour_logo.png", 
                                       class = "image"))),
                     column(6, div(class = "d-flex justify-content-center", 
                                   img(src = "https://netzeroplus.ac.uk/wp-content/themes/eightwire/assets/images/add-trees-logo.png", 
                                       class = "image")))
            )
        )
    )
  )
}

        
# Commented out GIFs (for later use)
# div(class = "gif-container",
#     fluidRow(
#       column(6, div(class = "gif-wrapper", img(src = "gif1.gif", class = "gif"))),
#       column(6, div(class = "gif-wrapper", img(src = "gif2.gif", class = "gif")))
#     )
# )


downscaling_page_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Placeholder for downscaling logic
  })
}
