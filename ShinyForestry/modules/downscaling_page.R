downscaling_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("downscaling-page"),
        # Ensure full height and center alignment
        div(class = "full-height",
            fluidRow(class = "d-flex align-items-center justify-content-center",  # Center images
                     column(6, div(class = "d-flex justify-content-center", 
                                   imageOutput(ns("DownScalingImage")))),
                     column(6, div(class = "d-flex justify-content-center", 
                                   imageOutput(ns("DownScalingImage2"))))
            )
        )
    )
  )
}

downscaling_page_server <- function(id, DownscalingImagesFolder) {
  moduleServer(id, function(input, output, session) {
    # Tims code for downscaling image
    output$DownScalingImage <- renderImage({
      list(src = normalizePath(file.path(DownscalingImagesFolder, "9do9xt.gif")),
           contentType = 'image/gif', width = 800, height = 600)
    }, deleteFile = FALSE)
    
    output$DownScalingImage2 <- renderImage({
      list(src = normalizePath(file.path(DownscalingImagesFolder, "9do1ky.gif")),
           contentType = 'image/gif', width = 800, height = 600)
    }, deleteFile = FALSE)
  })
}