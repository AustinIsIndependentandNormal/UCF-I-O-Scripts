# shiny::runApp('~/Desktop/SIOP 2017')

# UI Part I: display a static plot

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(),
        mainPanel(
            plotOutput(
                outputId = "scatter",
                width    = '100%',
                height   = '600px')
        )
    )    
)


# UI Part II: Add an interactive widget to control the shape of the points

# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId  = "shape", 
#                 label    = "Choose the variable to set the shapes of the points:", 
#                 choices  = c('Gender', 'Ethnicity'),
#                 selected = 'Gender')
#         ),
#         mainPanel(
#             plotOutput(
#                 outputId = "scatter",
#                 width    = '100%',
#                 height   = '600px')
#         )
#     )    
# )


# UI Part III: Add Radio Buttons to allow user to choose smoother type

# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId  = "shape", 
#                 label    = "Choose the variable to set the shapes of the points:", 
#                 choices  = c('Gender', 'Ethnicity'),
#                 selected = 'Gender'),

#             radioButtons(inputId  = 'smooth_method',
#                          label    = ' Choose your smoother:',
#                          choices  = c(LOESS = 'loess', LM = 'lm', None = 'none'),
#                          selected = 'none'
#             )
#         ),
#         mainPanel(
#             plotOutput(
#                 outputId = "scatter",
#                 width    = '100%',
#                 height   = '600px')
#         )
#     )    
# )


# UI Part IV: add slider inputs to control span and confidence level of smoother

# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId  = "shape", 
#                 label    = "Choose the variable to set the shapes of the points:", 
#                 choices  = c('Gender', 'Ethnicity'),
#                 selected = 'Gender'),

#             radioButtons(inputId  = 'smooth_method',
#                          label    = ' Choose your smoother:',
#                          choices  = c(LOESS = 'loess', LM = 'lm', None = 'none'),
#                          selected = 'none'
#             ),

#             sliderInput(inputId = "span", 
#                         label   = "Smoothing Parameter:",
#                         min     = 0.2,
#                         max     = 1.0,
#                         step    = 0.01,
#                         value   = 0.2),

#             sliderInput(inputId = "conf",
#                         label   = "Confidence Level:",
#                         min     = 0.80,
#                         max     = 0.999,
#                         step    = 0.001,
#                         value   = 0.85)
#         ),
#         mainPanel(
#             plotOutput(
#                 outputId = "scatter",
#                 width    = '100%',
#                 height   = '600px')
#         )
#     )    
# )


# UI Part V: add a text winoow beneath the plot that tells the user what was selected

# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId  = "shape", 
#                 label    = "Choose the variable to set the shapes of the points:", 
#                 choices  = c('Gender', 'Ethnicity'),
#                 selected = 'Gender'),

#             radioButtons(inputId  = 'smooth_method',
#                          label    = ' Choose your smoother:',
#                          choices  = c(LOESS = 'loess', LM = 'lm', None = 'none'),
#                          selected = 'none'
#             ),

#             sliderInput(inputId = "span", 
#                         label   = "Smoothing Parameter:",
#                         min     = 0.2,
#                         max     = 1.0,
#                         step    = 0.01,
#                         value   = 0.2),

#             sliderInput(inputId = "conf",
#                         label   = "Confidence Level:",
#                         min     = 0.80,
#                         max     = 0.999,
#                         step    = 0.001,
#                         value   = 0.85)
#         ),
#         mainPanel(
#             plotOutput(
#                 outputId = "scatter",
#                 width    = '100%',
#                 height   = '600px'),
#             HTML('<br>'),
#             verbatimTextOutput('text')            
#         )
#     )    
# )


# UI Part VI: add a conditional panel so that the sliders only appear when a smoother is chosen

# ui <- fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId  = "shape", 
#                 label    = "Choose the variable to set the shapes of the points:", 
#                 choices  = c('Gender', 'Ethnicity'),
#                 selected = 'Gender'),

#             radioButtons(inputId  = 'smooth_method',
#                          label    = ' Choose your smoother:',
#                          choices  = c(LOESS = 'loess', LM = 'lm', None = 'none'),
#                          selected = 'none'
#             ),

#             conditionalPanel(condition = "input.smooth_method != 'none'",
#                 sliderInput(inputId = "span", 
#                             label   = "Smoothing Parameter:",
#                             min     = 0.2,
#                             max     = 1.0,
#                             step    = 0.01,
#                             value   = 0.2),

#                 sliderInput(inputId = "conf",
#                             label   = "Confidence Level:",
#                             min     = 0.80,
#                             max     = 0.999,
#                             step    = 0.001,
#                             value   = 0.85)
#             )
#         ),
#         mainPanel(
#             plotOutput(
#                 outputId = "scatter",
#                 width    = '100%',
#                 height   = '600px'),
#             HTML('<br>'),
#             verbatimTextOutput('text')
#         )
#     )    
# )


