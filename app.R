#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
imbu_all <- data.table(read.csv2("imbu_list.csv", stringsAsFactors = F))
item_list <- data.table(read.csv2("item_list.csv", stringsAsFactors = F))

imbu_levels <- c("Basic", "Intricate", "Powerful")

imbu_all[category %in% c("crit", "paral", "cap", "speed"), category := "other"]
imbu_select_list <- split(x = imbu_all$name, imbu_all$category )

# do Obliczen
item_list <- merge(imbu_all, item_list, by.x = "var", by.y = "imbuement", all = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Imbuement price"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(width = 12,
                     tags$head( tags$style(HTML(" .selectize-input {  min-width: 150px; padding-top: 5px;} ")) ),
                     selectizeInput(
                         inputId = "s_imbu",
                         label = "Select imbuent:",
                         choices = imbu_select_list),
                     selectizeInput("s_level",
                                    label = "Select level:",
                                    choices = imbu_levels,
                                    selected = imbu_levels[length(imbu_levels)]
                     )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 12,
                  tags$head(
                      tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; margin-top: 25px;} 
                .form-group { display: table-row;}")
                  ),
                  uiOutput(outputId = "ui_items"),
                  h3("Gold Token Price:"),
                  fluidRow(column(3,numericInput(inputId = "price_token_buy", label = "Buy Price:", value = 30000, step = 1)),
                           column(3, textOutput("tot_tokens_buy", inline = T)),
                           column(3, numericInput(inputId = "price_token_sell", label = "Sell Price:", value = 30000, step = 1)),
                           column(3, textOutput("tot_tokens_sell", inline = T))
                  )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ui_items <- renderUI({
        lvl <- switch(input$s_level,
                      Basic=1,
                      Intricate=2,
                      Powerful=3)
        rows.selected <- item_list[name == input$s_imbu]
        
        
        output$tot_1_buy <- renderText({paste0("TOT: ", input$price_1_buy * input$quant_1)})
        output$tot_1_sell <- renderText({paste0("TOT: ", input$price_1_sell * input$quant_1)})
        
        output$tot_tokens_buy <- renderText({paste0("TOT: ", input$price_token_buy * lvl)})
        output$tot_tokens_sell <- renderText({paste0("TOT: ", input$price_token_sell * lvl)})
        
        
        fluidPage(
            tags$head( tags$style(HTML(" .numeric-input {  margin-top: 150px;} ")) ),
            h1(sprintf("Items for %s %s", input$s_level, input$s_imbu)),
            fluidRow(numericInput(inputId = "quant_1", label = h3(rows.selected[stage == 1]$item), value = rows.selected[stage == 1]$number_of)),
            fluidRow(column(6, numericInput(inputId = "price_1_buy", label = "Buy Price:", value = 1000, width = "25%", step = 1)),
                     column(6, textOutput("tot_1_buy", inline = T))
            ),
            fluidRow(column(6,numericInput(inputId = "price_1_sell", label = "Sell Price:", value = 1000, width = "25%", step = 1)),
                     column(6,textOutput("tot_1_sell", inline = T))
            ),
            conditionalPanel("input.s_level != 'Basic'",
                             fluidRow(
                                 h3(rows.selected[stage == 2]$item),
                                 numericInput(inputId = "quant_2", label = "Quantity:", value = rows.selected[stage == 2]$number_of),
                                 numericInput(inputId = "price_3_buy", label = "Buy Price:", value = 1000, width = "25%", step = 1),
                                 numericInput(inputId = "price_3_sell", label = "Sell Price:", value = 1000, width = "25%", step = 1)
                             )
            ),
            conditionalPanel("input.s_level == 'Powerful'",
                             fluidRow(
                                 h3(rows.selected[stage == 3]$item),
                                 numericInput(inputId = "quant_3", label = "Quantity:", value = rows.selected[stage == 3]$number_of, width = "25%", step = 1),
                                 numericInput(inputId = "price_3_buy", label = "Buy Price:", value = 1000, width = "25%", step = 1),
                                 numericInput(inputId = "price_3_sell", label = "Sell Price:", value = 1000, width = "25%", step = 1)
                                 
                             )
            ),
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
