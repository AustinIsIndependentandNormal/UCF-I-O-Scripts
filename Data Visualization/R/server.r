

library(ggplot2)
library(shiny)

test_data <- read.csv("~/Desktop/Projects/Active/SIOP 2017/Master Tutorial - Data Vis/Data/Test Data for SIOP R Tutorial w Ethnicity w SAT.csv", header = TRUE)

# ### Server Part I

server <- function(input, output) {

    output$scatter <- renderPlot({
                    
          ggplot(test_data, aes(x = Height, y = Weight)) + 
              geom_point(size = 3) + 
              ylim(75, 265) +
              stat_smooth(method = 'lm', level = 0.90, na.rm = TRUE) +
              theme(axis.text.x  = element_text(size = 14),
                    axis.text.y  = element_text(size = 14),
                    axis.title.x = element_text(size = 15),
                    axis.title.y = element_text(size = 15))
                    
    })

}



### Server Part II

# input variable - contains the input values from the browser 
#                  that were set in ui.R

# server <- function(input, output) {

#     output$scatter <- renderPlot({
                    
#         ggplot(test_data, aes(x = Height, y = Weight)) + 
#             geom_point(size = 3) + 
#             ylim(75, 265) +
#             stat_smooth(method = input$smooth_method, level = 0.90, na.rm = TRUE) +
#             theme(axis.text.x  = element_text(size = 14),
#                   axis.text.y  = element_text(size = 14),
#                   axis.title.x = element_text(size = 15),
#                   axis.title.y = element_text(size = 15))

#     })

# }



### Server Part III

# server <- function(input, output) {

#     output$scatter <- renderPlot({
                    
#           ggplot(test_data, aes(x = Height, y = Weight)) + 
#               geom_point(size = 3) + 
#               ylim(75, 265) +
#               stat_smooth(method = input$smooth_method, level = 0.90, na.rm = TRUE) +
#               theme(axis.text.x  = element_text(size = 14),
#                     axis.text.y  = element_text(size = 14),
#                     axis.title.x = element_text(size = 15),
#                     axis.title.y = element_text(size = 15))

#     })

# }


### Server Part IV

# server <- function(input, output) {

#     output$scatter <- renderPlot({
                    
#           ggplot(test_data, aes(x = Height, y = Weight)) + 
#               geom_point(size = 3) + 
#               ylim(75, 265) +
#               stat_smooth(method = input$smooth_method, level = 0.90, 
#                           span = input$span, na.rm = TRUE) +
#               theme(axis.text.x  = element_text(size = 14),
#                     axis.text.y  = element_text(size = 14),
#                     axis.title.x = element_text(size = 15),
#                     axis.title.y = element_text(size = 15))

#     })

# }



### Server Part V

# server <- function(input, output) {

#     output$scatter <- renderPlot({
                    
#           ggplot(test_data, aes(x = Height, y = Weight)) + 
#               geom_point(size = 3) + 
#               ylim(75, 265) +
#               stat_smooth(method = input$smooth_method, level = 0.90, 
#                           span = input$span, na.rm = TRUE) +
#               theme(axis.text.x  = element_text(size = 14),
#                     axis.text.y  = element_text(size = 14),
#                     axis.title.x = element_text(size = 15),
#                     axis.title.y = element_text(size = 15))

#     })

# }


### Server Part VI

# server <- function(input, output) {

#     output$scatter <- renderPlot({
                    
#           ggplot(test_data, aes(x = Height, y = Weight)) + 
#               geom_point(size = 3) + 
#               ylim(75, 265) +
#               ggtitle(input$title_text) +
#               stat_smooth(method = input$smooth_method, level = 0.90, 
#                           span = input$span, na.rm = TRUE) +
#               theme(axis.text.x  = element_text(size = 14),
#                     axis.text.y  = element_text(size = 14),
#                     axis.title.x = element_text(size = 15),
#                     axis.title.y = element_text(size = 15),
#                     plot.title   = element_text(size = 20, face = 'bold', hjust = 0.5)) 
              
#     })

# }

