library(shiny)
library(readr) 
library(ggplot2)

test_data <- read_csv("Test Data for SIOP R Tutorial w Ethnicity w SAT.csv")

# Server Part I: display static plot

server <- function(input, output) {

    output$scatter <- renderPlot({            
        ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
            geom_point(size = 3, na.rm = TRUE, aes_string(shape = 'Gender')) +
            stat_smooth(method = 'lm', level = 0.95, na.rm = TRUE) +
            theme(axis.text.x  = element_text(size = 14),
                  axis.text.y  = element_text(size = 14),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15),
                  legend.title = element_text(size = 14),
                  legend.text  = element_text(size = 12)
        )
    })
}


# Server Part II: link the interactive widget for shape to ggplot2

# server <- function(input, output) {

#     output$scatter <- renderPlot({            
#         ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
#             geom_point(size = 3, na.rm = TRUE, aes_string(shape = input$shape)) +
#             stat_smooth(method = 'lm', level = 0.95, na.rm = TRUE) +
#             theme(axis.text.x  = element_text(size = 14),
#                   axis.text.y  = element_text(size = 14),
#                   axis.title.x = element_text(size = 15),
#                   axis.title.y = element_text(size = 15),
#                   legend.title = element_text(size = 14),
#                   legend.text  = element_text(size = 12)
#         )
#     })
# }


# Server Part III: link radio buttons to smoother type

# server <- function(input, output) {

#     output$scatter <- renderPlot({            
#         ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
#             geom_point(size = 3, na.rm = TRUE, aes_string(shape = input$shape)) +
#             stat_smooth(method = input$smooth_method, level = 0.95, na.rm = TRUE) +
#             theme(axis.text.x  = element_text(size = 14),
#                   axis.text.y  = element_text(size = 14),
#                   axis.title.x = element_text(size = 15),
#                   axis.title.y = element_text(size = 15),
#                   legend.title = element_text(size = 14),
#                   legend.text  = element_text(size = 12)
#         )
#     })
# }


# Server Part IV: link sliders to control conf level and span

# server <- function(input, output) {

#     output$scatter <- renderPlot({            
#         ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
#             geom_point(size = 3, na.rm = TRUE, aes_string(shape = input$shape)) +
#             ylim(75, 265) +
#             stat_smooth(method = input$smooth_method, span = input$span, level = input$conf, na.rm = TRUE) +
#             theme(axis.text.x  = element_text(size = 14),
#                   axis.text.y  = element_text(size = 14),
#                   axis.title.x = element_text(size = 15),
#                   axis.title.y = element_text(size = 15),
#                   legend.title = element_text(size = 14),
#                   legend.text  = element_text(size = 12)
#         )
#     })
# }


# Server Part V: return text dynamically

# server <- function(input, output) {

#     output$scatter <- renderPlot({            
#         ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
#             geom_point(size = 3, na.rm = TRUE, aes_string(shape = input$shape)) +
#             ylim(75, 265) +
#             stat_smooth(method = input$smooth_method, span = input$span, level = input$conf, na.rm = TRUE) +
#             theme(axis.text.x  = element_text(size = 14),
#                   axis.text.y  = element_text(size = 14),
#                   axis.title.x = element_text(size = 15),
#                   axis.title.y = element_text(size = 15),
#                   legend.title = element_text(size = 14),
#                   legend.text  = element_text(size = 12)
#         )
#     })

#     output$text <- renderText({
#         paste0('You chose ', input$shape, ' to set the shape of the points.', '\n',
#                'You chose ', input$smooth_method, ' as your smoother.')
#     })
# }


# Server Part VI: silence the warning message

# server <- function(input, output) {

#     output$scatter <- renderPlot({
#         suppressWarnings(
#             print(
#                 ggplot(test_data, aes_string(x = 'Height', y = 'Weight')) +   
#                     geom_point(size = 3, na.rm = TRUE, aes_string(shape = input$shape)) +
#                     ylim(75, 265) +
#                     stat_smooth(method = input$smooth_method, span = input$span, level = input$conf, na.rm = TRUE) +
#                     theme(axis.text.x  = element_text(size = 14),
#                           axis.text.y  = element_text(size = 14),
#                           axis.title.x = element_text(size = 15),
#                           axis.title.y = element_text(size = 15),
#                           legend.title = element_text(size = 14),
#                           legend.text  = element_text(size = 12)
#                 )
#             )
#         )
#     })

#     output$text <- renderText({
#         paste0('You chose ', input$shape, ' to set the shape of the points.', '\n',
#                'You chose ', input$smooth_method, ' as your smoother.')
#     })
# }

