### shiny::runApp('~/Desktop/Projects/Active/SIOP 2017/Master Tutorial - Data Vis/R Code')



### UI Part I

# fluidpage()  - fluidpage is a layout that scales their components in
#                all available browser width.
#
# plotOutput() - function to display an an R plot image that was created in server.R
#   
#   outputID - output variable to read the plot from; set in server.R
#   width    - set the width of the plot; can be a percentage; 100%
#   height   - set the height of the plot


ui <- fluidPage(

    plotOutput(outputId = "scatter", 
               width    = '800px',   
               height   = '600px')   
  
)


### UI Part II

# selectInput() - Creates a 'select list' object in the browser that can be used to 
#                 choose a single or multiple items from a list of values.
#
#   inputId  - The ‘input’ slot that will be used to access the value. input is a list
#              that is passed to server.R
#   label    - display label for the browser object
#   choices  - list of (possibly named) values that to select from (and send to server.R)
#   selected - the default choice selected

# ui <- fluidPage(

#     plotOutput(outputId = "scatter",
#                width    = '800px', 
#                height   = '600px'),

#     selectInput(inputId  = 'smooth_method',
#                 label    = 'Choose your smoother:',
#                 choices  = list(LM = 'lm', Loess = 'loess', 'None' = 'none'),
#                 selected = 'none')    
  
# )


### UI Part III - sidepanel & main panel layouts

# sidebarLayout() - layout that splits the browser into two columns: a side panel 
#                   and a main panel. The side panel is typically used for 
#                   input options (e.g, selectInput()), and the main panel 
#                   typically contains outputs
#
#   sidebarPanel - The sidebarPanel containing input controls
#   mainPanel    - The mainPanel containing outputs

# ui <- fluidPage(

#     sidebarLayout(        

#         sidebarPanel(

#             selectInput(inputId  = 'smooth_method',
#                         label    = 'Choose your smoother:',
#                         choices  = list(LM = 'lm', Loess = 'loess', 'None' = 'none'),
#                         selected = 'none')
#         ),

#         mainPanel(

#             plotOutput(outputId = "scatter",
#                        width    = '800px', 
#                        height   = '600px')
#         )        

#     )  
# )


### UI Part IV - slider widget to control confidence level

# sliderInput() - Creates a slider widget in the browser to select a 
#                 numeric value from a range.
#
#   min   - minimum value of the slider
#   max   - maximum value of the slider
#   value - initial value of the slider
#   step  - sets the interval between slider values

# ui <- fluidPage(

#     sidebarLayout(        

#         sidebarPanel(

#             selectInput(inputId  = 'smooth_method',
#                         label    = 'Choose your smoother:',
#                         choices  = list(LM = 'lm', Loess = 'loess', 'None' = 'none'),
#                         selected = 'none'),

#             sliderInput(inputId  = 'span',
#                         label    = "Set the amount of smoothing:",
#                         min      = 0.20,
#                         max      = 1.00,
#                         value    = 0.80,
#                         step     = 0.01)

#         ),

#         mainPanel(

#             plotOutput(outputId = "scatter",
#                        width    = '800px', 
#                        height   = '600px')
#         )        

#     )  
# )



### UI Part V - conditional panel to hide slider widget to
###             until something other than 'none' is not selected

# conditionalPanel() - Creates a panel that is visible or not, depending on 
#                      the value of a JavaScript expression.

#   condition - JavaScript expression that is evaluated to determine whether
#               the panel should be hidden or not...

# ui <- fluidPage(

#     sidebarLayout(        

#         sidebarPanel(

#             selectInput(inputId  = 'smooth_method',
#                         label    = 'Choose your smoother:',
#                         choices  = list(LM = 'lm', Loess = 'loess', 'None' = 'none'),
#                         selected = 'none'),

#             conditionalPanel(condition = "input.smooth_method != 'none'",

#                 sliderInput(inputId  = 'span',
#                             label    = "Set the amount of smoothing:",
#                             min      = 0.20,
#                             max      = 1.00,
#                             value    = 0.80,
#                             step     = 0.01)
#             )

#         ),

#         mainPanel(

#             plotOutput(outputId = "scatter",
#                        width    = '800px', 
#                        height   = '600px')
#         )        

#     )  
# )


### UI Part VI - inputting text to add to the plot 

# 


# ui <- fluidPage(

#     sidebarLayout(        

#         sidebarPanel(

#             selectInput(inputId  = 'smooth_method',
#                         label    = 'Choose your smoother:',
#                         choices  = list(LM = 'lm', Loess = 'loess', 'None' = 'none'),
#                         selected = 'none'),

#             conditionalPanel(condition = "input.smooth_method != 'none'",

#                 sliderInput(inputId  = 'span',
#                             label    = "Set the amount of smoothing:",
#                             min      = 0.20,
#                             max      = 1.00,
#                             value    = 0.80,
#                             step     = 0.01)
#             ),

#             textInput(inputId = 'title_text',
#                       label   = 'What would you like the title to be?',
#                       value   = 'Here is a title!')

#         ),

#         mainPanel(

#             plotOutput(outputId = "scatter",
#                        width    = '800px', 
#                        height   = '600px')
#         )        

#     )  
# )

