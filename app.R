# Load dependencies
library(shiny)
library(tidyverse)
library(plotly)
library(stringr)
library(bslib)

# Read in data
econ_data <- read.csv("dsproject3data.csv")

sector_data <- econ_data |> 
  filter(Indicator.Code %in% c(
    "NV.AGR.TOTL.ZS",
    "NV.IND.TOTL.ZS",
    "NV.SRV.TOTL.ZS",
    "NE.TRD.GNFS.ZS",
    "NY.GDP.PCAP.CD"
  )) |> mutate(
    # Add easier-to-read labels
    sector = case_when(
      Indicator.Code == "NV.AGR.TOTL.ZS" ~ "Agriculture",
      Indicator.Code == "NV.IND.TOTL.ZS" ~ "Industry",
      Indicator.Code == "NV.SRV.TOTL.ZS" ~ "Service",
      Indicator.Code == "NE.TRD.GNFS.ZS" ~ "Trade",
      Indicator.Code == "NY.GDP.PCAP.CD" ~ "GDP"
    )
  ) |> select(Country.Name, Year, Region, incomeclass, sector, Indicator.Value) |> 
  
  # Widen the data to make it easier to create visualizations
  pivot_wider(
    names_from = sector,
    values_from = Indicator.Value
  ) |> 
  # Sort the data alphabetically by country and chronologically
  arrange(Country.Name, Year)

# Create a global average across countries, weighted by GDP
global_avg <- sector_data |> 
  group_by(Year) |> 
  summarize(
    Agriculture = weighted.mean(Agriculture, w = GDP),
    Industry = weighted.mean(Industry, w = GDP),
    Service = weighted.mean(Service, w = GDP),
    Trade = weighted.mean(Trade, w = GDP)
  ) |> 
  mutate(Country.Name = "Global Average")

# Build plotly theme function
plot_theme <- function(p, title_text = NULL) {
  p |> layout(
    title = list(
      text = title_text,
      x = 0,
      xanchor = "left",
      font = list(family = "Georgia, serif", size = 24)
    ),
    paper_bgcolor = "#f7f7f5",
    plot_bgcolor = "#f7f7f5",
    font = list(
      family = "Georgia, Times New Roman, serif",
      color = "#1a1a1a",
      size = 15
    ),
    legend = list(
      bgcolor = "rgba(0,0,0,0)"
    ),
    margin = list(l = 70, r = 30, t = 60, b = 60)
  )
}

# Define sector colors
sector_colors <- c(
  "Service" = "rgba(55, 118, 171, 0.85)",
  "Industry" = "rgba(255, 127, 14, 0.85)",
  "Agriculture" = "rgba(34, 139, 34, 0.85)"
)

# Darker shade for visual purposes
sector_line_colors <- c(
  "Service" = "rgba(55, 118, 171, 1)",
  "Industry" = "rgba(255, 127, 14, 1)",
  "Agriculture" = "rgba(34, 139, 34, 1)"
)

# Greyed out colors for the tree plot
sector_grey_colors <- c(
  "Service" = "rgba(180, 180, 180, 0.85)",
  "Industry" = "rgba(180, 180, 180, 0.85)",
  "Agriculture" = "rgba(180, 180, 180, 0.85)"
)

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(
    bg = "#f7f7f5",
    fg = "#1a1a1a",
    primary = "#1a1a1a",
    base_font = "Georgia, serif",
    heading_font = "Georgia, serif"
  ),
  
  # Sidebar with selection buttons for economic sector
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        "sector", 
        tags$span(style = "font-weight: 600;", "Select an Economic Sector"),
        c("Service","Industry","Agriculture")
      ),
      sliderInput(
        "year_end",
        tags$span(style = "font-weight: 600;", "Year"),
        min = min(sector_data$Year, na.rm = TRUE),
        max = max(sector_data$Year, na.rm = TRUE),
        value = max(sector_data$Year, na.rm = TRUE),
        step = 1,
        sep = "",
        animate = animationOptions(interval = 275, loop = FALSE)
      ),
      
      uiOutput("selected_country_ui"),
      
      # Populates for income Level treeplot
      conditionalPanel(
        condition = "input.bottom_view == 'Income Level'",
        div(
          style = "margin-top: 12px;",
          selectInput(
            "income",
            tags$span(style = "font-weight: 600;", "Select an Income Class"),
            c("All","High Income","Low Income")
          )
        ),
        conditionalPanel(
          condition = "input.bottom_view == 'Income Level'",
          div(
            style = "margin-top: 6px; font-size: 13px; color: #6b6b6b;",
            "Note: Some regions may not have data available for all income levels."
          )
        ),
        conditionalPanel(
          condition = "input.bottom_view == 'Income Level' && input.income != 'All' && input.year_end < 1987",
          div(
            style = "margin-top: 6px; font-size: 13px; color: #6b6b6b;",
            "Note: 1987 is the first year income level classifications are available."
          )
        )
      )
    ),
    
    # Show a plot of the generated plotly
    mainPanel(
      br(),
      div(
        style = "font-size: 13px; color: #6b6b6b; margin-bottom: 3px;",
        "Click a country to track its path across all charts. Use the play button to animate changes over time. Double click to reset."
      ),
      plotlyOutput("Plot1", height = "400px"),
      br(),
      
      div(
        style = "font-size: 13px; color: #6b6b6b; margin: 4px 0;",
        "Use the tabs below to explore trends over time, compare income levels, and examine trade patterns."
      ),
      navset_card_tab(
        id = "bottom_view",
        nav_panel("Over Time", plotlyOutput("stackedArea", height = "400px")),
        nav_panel("Income Level", plotlyOutput("Plot2", height = "400px")),
        nav_panel("Trade Openness", plotlyOutput("tradeLine", height = "400px"))
      )
    )
  )
)


# Define required server logic
server <- function(input, output, session) {
  
  # Reactive clicked-country input, stays selected over time animation
  selected_country <- reactiveVal("World")
  
  observeEvent(event_data("plotly_click", source = "plot1", priority = "event"), {
    clicked <- event_data("plotly_click", source = "plot1", priority = "event")
    if (!is.null(clicked) && !is.null(clicked$key) && length(clicked$key) > 0) {
      selected_country(clicked$key[[1]])
    }
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "plot1", priority = "event"), {
    selected_country("World")
  })
  
  # Make data reactive for sector input
  # Reformat input to match the indicators in the data
  input.sector <- reactive({
    str_replace_all(
      input$sector, 
      c("Service"="NV.SRV.TOTL.ZS",
        "Industry"="NV.IND.TOTL.ZS",
        "Agriculture" ="NV.AGR.TOTL.ZS"))})
  
  # Modify the data to only include GDP and value added by the chosen sector
  plot1data<-reactive({
    sector_data %>% 
      filter(Year==input$year_end) 
  })
  
  # Set up first plot
  output$Plot1 <- renderPlotly({
    sector_var <- input.sector()
    
    # Selected country
    df <- plot1data() |> 
      mutate(
        sector_value = case_when(
          input$sector == "Service" ~ Service,
          input$sector == "Industry" ~ Industry,
          input$sector == "Agriculture" ~ Agriculture
        ),
        hover_label = paste0(
          "<b>", Country.Name, ", ", Year, "</b><br>",
          "Value Added by Sector: ", round(sector_value, 1), "%<br>",
          "GDP per Capita: $", scales::comma(round(GDP, 0))
        )
      )
    
    selected_df <- df |> 
      filter(Country.Name == selected_country())
    
    # Construct the plotly object
    plot_ly(
      data = df %>% filter(Country.Name != selected_country()),
      x = ~sector_value, 
      y = ~GDP,
      # Assign scatterplot point color to the country's region
      key = ~Country.Name,
      text=~Country.Name,
      color = ~Region,
      source = "plot1",
      type="scatter",
      mode="markers",
      customdata = ~Year,
      opacity = case_when(
        selected_country() == "World"~1,
        TRUE~0.2
      ),
      marker = list(
        line = list(color = "white", width = 1.5),
        size = 14
      ),
      # Customize the text shown when you hover over a point
      hovertext = ~hover_label,
      hoverinfo = "text"
    ) %>% 
      add_trace(
        data = df %>% filter(Country.Name == selected_country()),
        x = ~sector_value,
        y = ~GDP,
        key = ~Country.Name,
        text = ~Country.Name,
        color = ~Region,
        type = "scatter",
        mode = "markers",
        opacity = 1,
        customdata = ~Year,
        marker = list(
          size = 14,
          line = list(color = "white", width = 1.5)
        ),
        hovertext = ~hover_label,
        hoverinfo = "text",
        showlegend = FALSE
      ) %>% 
      # Add titles and adjust the text formatting
      layout(
        hovermode = "closest",
        yaxis = list(
          type = "log", 
          title = "GDP per Capita (Current USD)",
          tickprefix = "$", 
          range=c(0,5)),
        xaxis = list(
          title = paste(input$sector, "Sector Value Added (% of GDP)"),
          ticksuffix = "%",
          range=c(0,80))
      ) %>% 
      plot_theme("Countries' Economic Composition Changes As Their GDP Increases") %>% 
      config(doubleClick = FALSE)
  })
  
  # Set up second plot
  
  # Widen the data so that each indicator has its own column
  plot2data <- reactive({
    req(input$bottom_view == "Income Level")
    
    df <- sector_data
    
    # If an income class is selected, filter countries with that income 
    # classification in the year it has that classification
    if (input$income != "All") {
      df<-df %>% 
        filter(incomeclass==input$income)
    }
    
    # If a country is selected
    if (selected_country() != "World") {
      selected_region <- df %>%
        filter(Country.Name == selected_country()) %>%
        distinct(Region)
      
      df <- df %>%
        inner_join(selected_region, by = "Region")
    }
    
    # Group the data by both region and year
    df <- df %>% 
      group_by(Region,Year) %>% 
      # Create summary statistics for the total regional gdp per capita,
      # weighted averages for each sector's contribution to the GDP, and
      # the number of countries being used to create these summary stats
      summarize(
        regionalgdp = sum(GDP),
        Service = weighted.mean(Service, w = GDP),
        Industry = weighted.mean(Industry, w = GDP),
        Agriculture = weighted.mean(Agriculture, w = GDP),
        Count = n(),
        .groups = "drop")
    
    # If there is no selected country, further aggregate the data by
    # creating summary statistics for the total world gdp per capita,
    # weighted averages for each sector's contribution to the GDP, and
    # the number of countries being used to create these summary stats
    if (selected_country() =="World") {
      df <- df %>% 
        group_by(Year) %>% 
        summarize(
          Service = weighted.mean(Service, w = regionalgdp),
          Industry = weighted.mean(Industry, w = regionalgdp),
          Agriculture = weighted.mean(Agriculture, w = regionalgdp),
          Count = sum(Count),
          .groups = "drop"
        )
    }
    
    # Lengthen the data so that the sector columns are combined 
    # into one column for easier data visualization 
    df  %>% 
      pivot_longer(
        cols = c(Service,Industry,Agriculture),
        names_to = "sector",
        values_to = "sectorpercent"
      ) %>%  
      filter(Year == plot2_year())
  })
  
  plot2region<-reactive({
    req(input$bottom_view == "Income Level")
    
    sector_data %>% 
      filter(Country.Name == selected_country()) %>% 
      distinct(Region)
  })
  
  plot2_year <- reactive({
    req(input$bottom_view == "Income Level")
    
    df <- sector_data 
    
    if (input$income != "All") {
      df <- df %>%
        filter(incomeclass == input$income)
    }
    
    # If a country is selected, restrict to that region
    if(selected_country() != "World") {
      selected_region <- df |> 
        filter(Country.Name == selected_country()) |> 
        distinct(Region)
      df <- df |> 
        inner_join(selected_region, by = "Region")
    }
    
    available_years <- df |> 
      filter(
        !is.na(Service),
        !is.na(Industry),
        !is.na(Agriculture),
        !is.na(GDP)
      ) %>%
      distinct(Year)
    
    if (input$year_end %in% available_years$Year) {
      input$year_end
    } else {
      1987
    }
  })
  
  # When there is no income classification data
  no_year <- reactive({
    req(input$bottom_view == "Income Level")
    
    plot2_year() == 1987 && input$year_end < 1987 && input$income != "All"
  })
  
  output$Plot2 <- renderPlotly({
    df <- plot2data()
    df$fill_color <- sector_line_colors[df$sector]
    if(no_year()) {
      df$fill_color <- sector_grey_colors[df$sector]
    }
    
    # Construct the plotly object  
    plot_ly(
      data=df,
      # Animate the treemap plot by time (year)
      type='treemap',
      # Separate the boxes by sector
      labels=~sector,
      # Customize the text that shows on the treemap boxes
      text=~Count,
      textinfo="label+value",
      texttemplate = "<b>%{label}</b>\n%{value:.1f}%",
      
      # If the graph is showing the entire world, classify the parent as "World"
      # If the graph is showing one region, classify the parent as the region
      parents= if (input$income == "All") {
        "All Income Levels"
      } else {
        input$income
      },
      # Assign the size of the box to the percentage of the sector
      values= ~sectorpercent,
      # The size of the parent is determined by the total size of the sectors
      branchvalues = "total",
      marker = list(colors = df$fill_color),
      # Customize the text shown when you hover over a box
      hovertemplate = '<b>%{label}</b>\nValue Added (% of GDP): %{value:.1f}%\nNumber of Countries: %{text}<extra></extra>') %>% 
      # Add titles and adjust the text formatting
      plot_theme(
        paste0(
          if (selected_country() == "World") {
            "Sector Composition by Income Level: Global Average"
          } else {
            paste("Sector Composition by Income Level:", plot2region())
          }
        )
      )
  }) 
  
  # Make data reactive for Stacked Area Chart
  stackedArea_data <- reactive({
    req(input$bottom_view == "Over Time")
    
    # Input country select data
    sect_data <- if(selected_country() == "World") {
      global_avg
    } else {
      sector_data |> 
        filter(Country.Name == selected_country()) |> 
        select(Year, Agriculture, Industry, Service, Country.Name)
    }
    # input year animation data
    sect_data |> 
      filter(Year <= input$year_end) |> 
      arrange(Year)
  })
  
  # Make data reactive for trade and sector line chart
  tradeLine_data <- reactive ({
    req(input$bottom_view == "Trade Openness")
    
    # Input country select data
    trade_data <- if(selected_country() == "World") {
      global_avg
    } else {
      sector_data |> 
        filter(Country.Name == selected_country()) |> 
        select(Year, Agriculture, Industry, Service, Trade, Country.Name)
    }
    
    # Input year and sector data
    trade_data |>  
      filter(Year <= input$year_end) |> 
      arrange(Year) |> 
      mutate(
        Selected.Sector = case_when(
          input$sector == "Agriculture" ~ Agriculture,
          input$sector == "Industry" ~ Industry,
          input$sector == "Service" ~ Service
        )
      )
  })
  
  output$stackedArea <- renderPlotly({
    df <- stackedArea_data()
    
    # Construct plotly stacked area chart
    plot_ly(df, x = ~Year, y = ~Agriculture, 
            name = "Agriculture",
            type = "scatter", 
            mode = "lines", 
            stackgroup = "one", 
            groupnorm = "percent",
            fillcolor = sector_colors["Agriculture"],
            line=list(color=sector_line_colors["Agriculture"])
    ) |> 
      add_trace(
        y = ~Industry,
        name = "Industry",
        fillcolor = sector_colors["Industry"],
        line = list(color = sector_line_colors["Industry"])
      ) |> 
      add_trace(
        y = ~Service,
        name = "Service",
        fillcolor = sector_colors["Service"],
        line = list(color = sector_line_colors["Service"])
      ) |> 
      layout(
        xaxis = 
          list(
            title = "Year", 
            tickformat = 'd',
            tickmode = "array",
            tickvals = c(seq(1960, 2015, by = 5), 2019)
          ),
        yaxis = 
          list(
            title = "Percent of GDP", 
            ticksuffix = "%",
            hoverformat = '.1f'),
        hovermode = "x unified"
      ) |> 
      plot_theme(paste("Sector Composition Over Time:", unique(df$Country.Name)))
  })
  
  output$tradeLine <- renderPlotly ({
    df <- tradeLine_data()
    trade_color <- "rgba(80, 80, 80, 1)"
    
    # Construct trade line plot
    plot_ly(df, x = ~Year, y = ~Service,
            name = "Service",
            type = "scatter",
            mode = "lines",
            line = list(color = sector_line_colors["Service"], width = 2, dash = "4px,3px"),
            hovertemplate = "Service: %{y:.1f}%<extra></extra>"
    ) |> 
      add_lines(
        y = ~Industry,
        name = "Industry",
        line = list(color = sector_line_colors["Industry"], width = 2, dash = "4px,3px"),
        hovertemplate = "Industry: %{y:.1f}%<extra></extra>"
      ) |> 
      add_lines(
        y = ~Agriculture,
        name = "Agriculture",
        line = list(color = sector_line_colors["Agriculture"], width = 2, dash = "4px,3px"),
        hovertemplate = "Agriculture: %{y:.1f}%<extra></extra>"
      ) |> 
      add_lines(
        y = ~Trade,
        name = "Trade Openness",
        line = list(color = trade_color, width = 4, dash = "solid"),
        hovertemplate = "Trade Openness: %{y:.1f}%<extra></extra>"
      ) |> 
      layout(
        xaxis = list(
          title = "Year",
          tickformat = "d",
          tickmode = "array",
          tickvals = c(seq(1960, 2015, by = 5), 2019)
        ),
        yaxis = list(title = "Percent of GDP", ticksuffix = "%"),
        hovermode = "x unified"
      ) |> 
      plot_theme(paste("Trade Openness and Sector Share:", unique(df$Country.Name)))
  })
  
  # Selected country note in sidebar
  output$selected_country_ui <- renderUI ({
    div(
      style = "font-weight: 600; color: #2c5282;",
      "Selected Country: ",
      span(
        style = "font-weight: 400",
        if (selected_country() == "World") {
          "Global Average"
        } else {
          selected_country()
        }
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

