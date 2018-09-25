library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  # A descriptive title
  h4("Some hockey stats and graphs"),
  tags$hr(),
  
  # Some style tags to auto size the width of the images
  tags$head(tags$style(
    type="text/css",
    "#thumbnail img {width: auto; height:APp 90px}",
    "#team img {width: auto; height: 90px}"
  )),
  
  # Defining the scatter, bar, and pie chart layout
  fluidRow(
    column(8, 
           plotlyOutput(
             outputId = "scatter", 
             height = "500px")
           ),
    column(4, 
           fluidRow(
             plotlyOutput(
               outputId = "bar", 
               height = "250px")
             ),
    fluidRow(
      plotlyOutput(
        outputId = "pie", 
        height = "250px")
      )
    )
    ),
  tags$hr(),
  
  # Defining the player thumbnail, player stats, and the team photo
  fluidRow(
    wellPanel(
      fluidRow(
        column(2, 
               uiOutput(outputId = "name")
               )
        ),
      fluidRow(
        fluidRow(
          column(2,
                 imageOutput(outputId = "thumbnail", 
                             height = "90px")
                 ), 
          column(8, 
                 uiOutput(outputId = "stats")
                 ),
          column(2,
                 imageOutput(outputId = "team", 
                             height = "90px")
                 )
          )
        )
      )
    )
)
server <- function(input, output){
  
  # Read in the data
  df1 <- read.csv("www/data.csv", stringsAsFactors = FALSE)
  df2 <- read.csv("www/data2.csv", stringsAsFactors = FALSE)
  df3 <- read.csv("www/data3.csv", stringsAsFactors = FALSE)
  
  # Filtering out the players with under 500 minutes 
  df1 <- df1[df1$TOI > 500,]
  df2 <- df2[df2$TOI > 500,]
  df3 <- df3[df3$TOI > 500,]
  
  # Joining the data on column Player
  df <- (merge(merge(df1, df2, by = "Player"), df3, by = "Player"))
  
  # Adding a column for primary points (goals + first assists)
  df$P1.60 <- df$Goals.60 + df$First.Assists.60
  
  # A function to add a blue line to the bar chart
  hline <- function(y = 0, color = "blue") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y, 
      line = list(color = color)
    )
  }
  
  # Scatter plot of CF% vs Primary point per 60
  output$scatter <- renderPlotly({
    p <- plot_ly(data = df, 
                 type = 'scatter', 
                 mode = 'markers') %>%
         add_trace(x = ~CF.,
                   y = ~P1.60,
                   color = ~Position.x,
                   size = ~TOI.x,
                   key = ~Player,
                   text = ~Player,
                   hoverinfo = "text",
                   showlegend = F) %>%
         layout(title = "CF% vs Primary points per 60 (5v5)",
                titlefont = list(size = 10),
           xaxis = list(title = "Corsi For %", zeroline = F),
           yaxis = list(title = "Primary points per 60",hoverformat = '.2f'))
       p
  })
  
  # Defining a barplot based on ploty_hover
  output$bar <- renderPlotly({

    # Read in data the cursor is hovering over and extracting the value key (Player Name)
    eventdata <- event_data("plotly_hover")
    validate(need(!is.null(eventdata), "Hover over the scatter plot to see a breakdown of the player's High, Medium, and Low Danger Corsi"))
    datapoint <- as.character(eventdata["key"])


    # Shows a bar chart using the hover over text
    bardf <- df[df$Player == datapoint, c("Player","HDCF.", "MDCF.", "LDCF.")]
    bardf <- gather(bardf, Var, Freq, -Player)
    plot_ly(bardf, 
            x=~Var, 
            y = ~Freq, 
            type = 'bar', 
            marker = list(color = c('rgba(222,45,38,0.8)', 
                                    'rgba(248,229,4,0.8)',
                                    'rgba(15,248,11,0.8)'))) %>%
      layout(title = "CF Breakdown",
            shapes=list(hline(50)))
  })

  # Coupled hover event
  output$pie <- renderPlotly({

    # Read in hover data
    eventdata <- event_data("plotly_hover")
    validate(need(!is.null(eventdata), "Hover over the scatter plot to a pie chart of the players goal, primary assists, and secondary assists"))
    datapoint <- as.character(eventdata["key"])

    # Show correlation heatmap
    piedf <- df[df$Player == datapoint, c("Player","Goals", "First.Assists", "Second.Assists")]
    piedf <- gather(piedf, Var, Freq, -Player)

    plot_ly(piedf, 
            labels = ~Var, 
            values = ~Freq, 
            type = 'pie', 
            textposition = 'inside', 
            textinfo = 'label+percent') %>%
      layout(title = paste(datapoint, "Breakdown of Point Totals (All situations)"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = FALSE)
  })
  
  # Players name displayed above their thumbnail picture
  output$name <- renderUI({
    eventdata <- event_data("plotly_hover")
    datapoint <- as.character(eventdata["key"])
    h4(datapoint)
  })
  
  # Players thumbnail rendered from hover over data
   output$thumbnail <- renderImage({
    eventdata <- event_data("plotly_hover")
    datapoint <- as.character(eventdata["key"])
    outfile <- paste("www/",gsub(pattern = "[[:space:]]", replacement = "", x = datapoint), ".png", sep ="")
    list(src = outfile,
         width = "100px",
         height = "100px")
  }, deleteFile = FALSE)
  
  # Players team logo rendered from hover over data 
  output$team <- renderImage({
    eventdata <- event_data("plotly_hover")
    datapoint <- as.character(eventdata["key"])
    teamIMG <- substr(df[df$Player == datapoint, "Team.x"], 1, 3)
    outfile <- paste("www/",teamIMG, ".png", sep ="")
    list(src = outfile,
         width = "100px",
         height = "100px")
  }, deleteFile = FALSE)

  # Players stats 
  output$stats <- renderUI({
    eventdata <- event_data("plotly_hover")
    datapoint <- as.character(eventdata["key"])
    
    # Table in HTML code of players stats
    HTML(paste0("<style>
          table {
                border-collapse: collapse;
                width: 100%;
                }
                th, td {
                text-align: left;
                padding: 8px;
                border: 1px solid black;
                }
                th {
                background-color: #13058e;
                color: white;
                }
                </style><table style='width:100%; text-align: center'><tr>
         <th>GP</th>
         <th>TOI</th>
         <th>Goals</th>
         <th>Primary Assists</th>
         <th>Secondary Assists</th>
         <th>Total Points</th>
         <th>Shots</th>
         <th>CF%</th>
         </tr>
         <tr>
         <td>",df[df$Player == datapoint, "GP"],"</td>
         <td>",df[df$Player == datapoint, "TOI"],"</td>
         <td>",df[df$Player == datapoint, "Goals"],"</td>
         <td>",df[df$Player == datapoint, "First.Assists"],"</td>
         <td>",df[df$Player == datapoint, "Second.Assists"],"</td>
         <td>",df[df$Player == datapoint, "Total.Points"],"</td>
         <td>",df[df$Player == datapoint, "Shots"],"</td>
         <td>",df[df$Player == datapoint, "CF."],"</td>
         </tr>
         </table>"))
  })
}

shinyApp(ui=ui, server = server)