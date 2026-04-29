library(shiny)
library(plotly)
library(dplyr)

pd1 <- readRDS("/srv/shiny-server/pd1.rds")
officer_counts <- pd1 |> dplyr::count(Officer_Name, sort = TRUE)
ecdf_fn <- ecdf(officer_counts$n)

ui <- fluidPage(
  titlePanel("Officer Ticket Distribution"),
  fluidRow(
    column(8, plotlyOutput("ecdf_plot", height = "500px")),
    column(4,
      h4("Lookup by Percentile"),
      div(style = "width:60%;",
        numericInput("percentile_input", "Enter Percentile (0-100):", value = 50, min = 0, max = 100)
      ),
      actionButton("btn_pct", "Lookup", style = "color:white; background-color:#9e5a5a;"),
      br(), br(),
      textOutput("percentile_result"),
      hr(),
      h4("Filter by Ticket Count"),
      div(style = "width:60%;",
        numericInput("threshold", "Enter Threshold (>=):", value = 1000, min = 1)
      ),
      actionButton("btn", "View List", style = "color:white; background-color:steelblue;"),
      br(), br(),
      textOutput("summary_text"),
      br(),
      div(style = "height:200px; overflow-y:scroll; border:1px solid #ccc; padding:8px; width:75%;",
          tableOutput("officer_table")),
      hr(),
      h4("Search Officer by Name"),
      div(style = "width:75%;",
        textInput("officer_search", "Enter Officer Name (case-insensitive):",
                  placeholder = "e.g. Mims or D. Mims or ma")
      ),
      actionButton("btn_search", "Search", style = "color:white; background-color:#5a9e6f;"),
      br(), br(),
      textOutput("officer_result"),
      br(),
      div(style = "height:150px; overflow-y:scroll; border:1px solid #ccc; padding:8px; width:75%;",
          tableOutput("officer_match_table"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    officer_x=NULL, officer_y=NULL, officer_label=NULL, match_table=NULL,
    pct_x=NULL, pct_y=NULL, pct_label=NULL
  )
  observeEvent(input$btn_search, {
    name <- trimws(input$officer_search)
    matches <- officer_counts |> dplyr::filter(grepl(name, Officer_Name, ignore.case=TRUE))
    if (nrow(matches)==0) {
      rv$officer_x<-NULL; rv$officer_y<-NULL
      rv$officer_label<-paste0("No officer found matching '",name,"'.")
      rv$match_table<-NULL
    } else if (nrow(matches)==1) {
      rv$officer_x<-matches$n; rv$officer_y<-ecdf_fn(matches$n)
      rv$officer_label<-paste0(matches$Officer_Name,": ",matches$n," tickets | Percentile: ",round(ecdf_fn(matches$n)*100,1),"%")
      rv$match_table<-NULL
    } else {
      rv$officer_x<-matches$n; rv$officer_y<-ecdf_fn(matches$n)
      rv$officer_label<-paste0(nrow(matches)," officers matched '",name,"':")
      rv$match_table<-matches
    }
  })
  observeEvent(input$btn_pct, {
    pct <- input$percentile_input/100
    target_n <- quantile(officer_counts$n, pct)
    closest <- officer_counts |> dplyr::mutate(diff=abs(n-target_n)) |> dplyr::arrange(diff) |> dplyr::slice(1)
    rv$pct_x<-closest$n; rv$pct_y<-ecdf_fn(closest$n)
    rv$pct_label<-paste0(input$percentile_input,"th percentile ~ ",closest$n," tickets | Closest officer: ",closest$Officer_Name)
  })
  output$ecdf_plot <- renderPlotly({
    p <- plot_ly() |>
      add_trace(data=officer_counts |> dplyr::arrange(n), x=~n, y=~ecdf_fn(n),
        type="scatter", mode="lines", line=list(color="steelblue",width=2), name="ECDF",
        hoverinfo="text", text=~paste0("Tickets: ",n,"<br>Officer: ",Officer_Name,"<br>Percentile: ",round(ecdf_fn(n)*100,1),"%"))
    if (!is.null(rv$officer_x)) {
      p <- p |> add_trace(x=rv$officer_x, y=rv$officer_y, type="scatter", mode="markers",
        marker=list(color="#5a9e6f",size=12,symbol="circle"), name="Officer Search",
        hoverinfo="text",
        text=if(!is.null(rv$match_table)) paste0(rv$match_table$Officer_Name,": ",rv$match_table$n," tickets | Percentile: ",round(ecdf_fn(rv$match_table$n)*100,1),"%") else rv$officer_label)
    }
    if (!is.null(rv$pct_x)) {
      p <- p |> add_trace(x=rv$pct_x, y=rv$pct_y, type="scatter", mode="markers",
        marker=list(color="#9e5a5a",size=12,symbol="diamond"), name="Percentile Lookup",
        hoverinfo="text", text=rv$pct_label)
    }
    p |> layout(
      xaxis=list(title="Number of Tickets Issued (log scale)",type="log"),
      yaxis=list(title="Cumulative % of Officers",tickformat=".0%"),
      hovermode="closest",
      shapes=list(
        list(type="line",x0=100,x1=100,y0=0,y1=1,line=list(color="darkred",dash="dash")),
        list(type="line",x0=500,x1=500,y0=0,y1=1,line=list(color="darkred",dash="dash")),
        list(type="line",x0=1000,x1=1000,y0=0,y1=1,line=list(color="darkred",dash="dash"))
      ))
  })
  filtered <- eventReactive(input$btn, {
    officer_counts |> dplyr::filter(n>=input$threshold) |> dplyr::arrange(desc(n)) |>
      dplyr::rename("Officer Name"=Officer_Name,"Ticket Count"=n)
  })
  output$summary_text <- renderText({ req(filtered()); paste0(">= ",input$threshold," tickets -- ",nrow(filtered())," officers") })
  output$officer_table <- renderTable({ req(filtered()); filtered() })
  output$officer_result <- renderText({ rv$officer_label })
  output$officer_match_table <- renderTable({
    req(rv$match_table)
    rv$match_table |> dplyr::arrange(desc(n)) |> dplyr::rename("Officer Name"=Officer_Name,"Ticket Count"=n)
  })
  output$percentile_result <- renderText({ rv$pct_label })
}

shinyApp(ui, server)
