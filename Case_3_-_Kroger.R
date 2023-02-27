#Alan Tung Case 3
load("KrogerHouseholdsCarbs.RData")
HOUSE$Location <- paste(HOUSE$City,HOUSE$StateID)

library(shiny)
library(shinyWidgets)


ui <- fluidPage(
    setBackgroundImage(
        src = "https://cutewallpaper.org/21/final-fantasy-14-backgrounds/Image-Wiki-background-Final-Fantasy-XIV-Guide-Wiki-.jpg"),
    titlePanel(h1("Kroger Household Analysis",style={'color:orange;'}),),

 sidebarLayout(
        sidebarPanel(
            selectInput("city", 
                      "Select City(s) to Analyze",
                      choices = sort(unique(HOUSE$Location)),
                      selected = c("Alcoa TN","Knoxville TN","Maryville TN","Farragut TN"),multiple=TRUE),
            hr(),
            tags$hr(style="color: black;"),
            radioButtons("quantity", 
                         "Choose Quantity to Study by City",
                         c("Total Items" = "TotalItems", 
                           "Unique Items" = "UniqueItems", 
                           "Number of Stores" = "NumberOfStores", 
                           "Total Spent" = "TotSpent", 
                           "Number of Visits" = "NumVisits", 
                           "Number of Coupons" = "NumCoupons",
                           "Pancake Spending" = "PancakeMoney", 
                           "Pasta Spending" = "PastaMoney", 
                           "Sauce Spending" = "SauceMoney",
                           "Syrup Spending" = "SyrupMoney", 
                           "Number Featured Items Purchased" = "NumFeatured", 
                           "Number Displayed Items Purchased" = "NumDisplayed")),
            tags$hr(style="color: black;"),
            checkboxInput("qlog", 
                          "Display Quantity Studied on Logscale"),
            tags$hr(style="color: grey;"),
            tags$hr(style="color: black;"),
            radioButtons("verticalaxis", 
                         "Choose Vertical Axis for Scatterplot",
                         c("Pancake" = "PancakeMoney", 
                           "Pasta" = "PastaMoney", 
                           "Sauce" = "SauceMoney",
                           "Syrup" = "SyrupMoney")),
            tags$hr(style="color: black;"),
             radioButtons("horizontalaxis",
                         "Choose Horizontal Axis for Scatterplot",
                         c("Pancake" = "PancakeMoney", 
                           "Pasta" = "PastaMoney", 
                           "Sauce" = "SauceMoney",
                           "Syrup" = "SyrupMoney")),
            tags$hr(style="color: black;"),
            checkboxGroupInput("wlog",
                               "Display which axes on Log Scale for Scatterplot",
                               c("Vertical", "Horizontal"))
                               
        ),


        mainPanel(
           plotOutput("top"),
           textOutput("narration"),
           tags$head(tags$style("#narration{color: orange;}")),
           verbatimTextOutput("middle"),
           plotOutput("bottom")
        )
    )
)


server <- function(input,output) {
    
    output$top <- renderPlot({
        w <- unlist(strsplit(input$city, "  "))
        form <- HOUSE[HOUSE$Location %in% c(w), ]
        if(input$quantity == "TotalItems") {x <- form$TotalItems}
        if(input$quantity == "UniqueItems") {x <- form$UniqueItems}
        if(input$quantity == "NumberOfStores") {x <- form$NumberOfStores}
        if(input$quantity == "TotSpent") {x <- form$TotSpent}
        if(input$quantity == "NumVisits"){x <- form$NumVisits}
        if(input$quantity == "NumCoupons"){x <- form$NumCoupons}
        if(input$quantity == "PancakeMoney") {x <- form$PancakeMoney}
        if(input$quantity == "PastaMoney") {x <- form$PastaMoney}
        if(input$quantity == "SauceMoney") {x <- form$SauceMoney}
        if(input$quantity == "SyrupMoney") {x <- form$SyrupMoney}
        if(input$quantity == "NumFeatured") {x <- form$NumFeatured}
        if(input$quantity == "NumDisplayed") {x <- form$NumDisplayed}
        ifelse(input$qlog == TRUE, 
               {boxplot(x ~ Location, data= form, log= "y", xlab = "Location", ylab=input$quantity)},
               {boxplot(x ~ Location, data= form, xlab = "Location", ylab=input$quantity)})
    })
    
    output$middle <- renderPrint({
        w <- unlist(strsplit(input$city, "  "))
        form <- HOUSE[HOUSE$Location %in% c(w), ]
        if(input$quantity == "TotalItems") {y <- form$TotalItems}
        if(input$quantity == "UniqueItems") {y <- form$UniqueItems}
        if(input$quantity == "NumberOfStores") {y <- form$NumberOfStores}
        if(input$quantity == "TotSpent") {y <- form$TotSpent}
        if(input$quantity == "NumVisits"){y <- form$NumVisits}
        if(input$quantity == "NumCoupons"){y <- form$NumCoupons}
        if(input$quantity == "PancakeMoney") {y <- form$PancakeMoney}
        if(input$quantity == "PastaMoney") {y <- form$PastaMoney}
        if(input$quantity == "SauceMoney") {y <- form$SauceMoney}
        if(input$quantity == "SyrupMoney") {y <- form$SyrupMoney}
        if(input$quantity == "NumFeatured") {y <- form$NumFeatured}
        if(input$quantity == "NumDisplayed") {y <- form$NumDisplayed}
        graph <- aggregate(y ~ Location, data=form, FUN=mean)
        names(graph)[2] <- input$quantity
        graph
    })
    
    output$narration <- renderText({
        paste("Each value of", input$quantity, "for each selected city")
    })
    
    output$bottom<- renderPlot({
        
        if(input$verticalaxis == "PancakeMoney") {i <- HOUSE$PancakeMoney}
        if(input$verticalaxis == "PastaMoney") {i <- HOUSE$PastaMoney}
        if(input$verticalaxis == "SauceMoney") {i <- HOUSE$SauceMoney}
        if(input$verticalaxis == "SyrupMoney") {i <- HOUSE$SyrupMoney}
        
        if(input$horizontalaxis == "PancakeMoney") {p <- HOUSE$PancakeMoney}
        if(input$horizontalaxis == "PastaMoney") {p <- HOUSE$PastaMoney}
        if(input$horizontalaxis == "SauceMoney") {p <- HOUSE$SauceMoney}
        if(input$horizontalaxis == "SyrupMoney") {p <- HOUSE$SyrupMoney}
        
        if(is.null(input$wlog)) {plot(p,i,xlab=input$horizontalaxis,ylab=input$verticalaxis)}
        
        vertical <- "Vertical"%in%input$wlog
        horizontal <- "Horizontal"%in%input$wlog
        
        if(vertical & horizontal){
            plot(p,i,log="xy", xlab=input$horizontalaxis, ylab=input$verticalaxis)
        } else if(vertical){
            plot(p,i,log="y", xlab=input$horizontalaxis, ylab=input$verticalaxis)
        } else if(horizontal){
            plot(p,i,log="x", xlab=input$horizontalaxis, ylab=input$verticalaxis)
        }
        
    })
}


shinyApp(ui = ui, server = server)
