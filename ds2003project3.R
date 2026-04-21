# Load dependencies
library(shiny)
library(tidyverse)
library(plotly)
library(stringr)

# Read in data
econ_data<-read.csv("dsproject3data.csv")

# Modify the data to have easier-to-read sector labels and a better
# format for data visualization
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
  ) |> select(Country.Name, Year, Region, sector, Indicator.Value) |> 
  
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



# Define UI for application
ui <- fluidPage(
  
  # Application title
  
  titlePanel("How does economic composition vary with economic growth?"),
  
  # Sidebar with selection buttons for economic sector
  sidebarLayout(
    sidebarPanel(
      
      # Keep the sidebar panel visible even when you scroll down
      style = "position:fixed;",
      fluidPage(
        fluidRow(
          
          # Show this sidebar input panel only on tab 1      
          conditionalPanel(
            condition = ("input.tabs == 'tab1'"),
            column(12, radioButtons("sector", "Economic Sector",
                                    c("Service","Industry","Agriculture"))),
            column(12, selectInput("country",
                                   "Select a country",
                                   choices = c("World", sort(unique(econ_data$Country.Name))))),
            column(12, sliderInput(
              "year_end",
              NULL,
              min = min(sector_data$Year, na.rm = TRUE),
              max = max(sector_data$Year, na.rm = TRUE),
              value = max(sector_data$Year, na.rm = TRUE),
              step = 1,
              sep = "",
              animate = animationOptions(interval = 150, loop = FALSE)
            )
            )),
          
          # Show this sidebar input panel only on tab 2    
          conditionalPanel(
            condition = ("input.tabs == 'tab2'"),
            column(12, selectInput("country",
                                   "Select a country",
                                   choices = c("World", sort(unique(econ_data$Country.Name))))),
            column(12, selectInput("income",
                                   "Select an income class",
                                   choices = c("All","High Income","Low Income")))
          ),
          
          # Show this sidebar input panel only on tab 3    
          conditionalPanel(
            condition = ("input.tabs == 'tab3'"),
            column(12, radioButtons("sector", "Economic Sector",
                                    c("Service","Industry","Agriculture"))),
            column(12, selectInput("country",
                                   "Select a country",
                                   choices = c("World", sort(unique(econ_data$Country.Name))))),
            column(12, sliderInput(
              "year_end",
              NULL,
              min = min(sector_data$Year, na.rm = TRUE),
              max = max(sector_data$Year, na.rm = TRUE),
              value = max(sector_data$Year, na.rm = TRUE),
              step = 1,
              sep = "",
              
              # Allow animation by year
              animate = animationOptions(interval = 150, loop = FALSE)
            ))
          )
        ))
    ),
    
    # Show the created plots
    mainPanel(
      
      tabsetPanel(id="tabs",
                  
                  # Tab 1 should have two plots
                  tabPanel("Economic Composition over Time", 
                           value="tab1",
                           br(), 
                           plotlyOutput("Plot1"), 
                           br(),
                           plotlyOutput("stackedArea")),
                  
                  # Tab 2 should have one plot
                  tabPanel('Regional Composition', 
                           value="tab2",
                           br(), 
                           plotlyOutput("Plot2")),
                  
                  # Tab 3 should have one plot
                  tabPanel('Economic Sectors and Trade',
                           value="tab3",
                           br(), 
                           plotlyOutput("tradeLine")))
      
    )
  )
)

# Define required server logic
server <- function(input, output, session) {
  
  # Make data reactive for sector input
  # Reformat input to match the indicators in the data
  input.sector<- reactive({str_replace_all(input$sector, c("Service"="NV.SRV.TOTL.ZS",
                                                           "Industry"="NV.IND.TOTL.ZS",
                                                           "Agriculture" ="NV.AGR.TOTL.ZS"))})
  
  
  # Modify the data to only include GDP and value added by the chosen sector
  plot1data<-reactive({econ_data %>% 
      filter(Indicator.Code %in% c("NY.GDP.PCAP.CD", input.sector())) %>% 
      
      # Separate GDP and value added by the chosen sector into 2 different columns
      # to make data visualization in plotly easier
      pivot_wider(id_cols=c(Country.Name,Country.Code,Year,Region,incomeclass),
                  names_from=Indicator.Code, 
                  values_from=Indicator.Value) %>% 
      
      # Filter the data to just show the year chosen in the input sidebar
      filter(Year==input$year_end)})
  
  # Set up first plot
  output$Plot1 <- renderPlotly({
    
    # Construct the plotly object
    plot_ly(data = plot1data(), x = ~get(input.sector()), y = ~NY.GDP.PCAP.CD,
            
            # Assign scatterplot point color to the country's region
            color = ~Region,
            
            text=~Country.Name,
            ids=~Country.Code,
            type="scatter",
            mode="markers",
            marker = list(opacity = 0.7,line = list(color = "white", width = 1)),
            
            # Customize the text shown when you hover over a point
            customdata = ~Year,
            hovertemplate = '<b>%{text}, %{customdata}</b>\nValue Added by Sector: %{x:.1f}%\nGDP per Capita: %{y:$,.0f}<extra></extra>'
    ) %>% 
      
      # Add titles, adjust the text formatting, and axis ranges
      layout(title = list(text="Countries' economic composition changes rapidly as their GDP increases",
                          x = 0,
                          xanchor = "left",
                          xref = "paper"),
             yaxis = list(type = "log", title = "GDP per Capita (Current USD)",
                          tickprefix = "$", range=c(0,6)),
             xaxis = list(title = paste(input$sector,
                                        "Sector Value Added (% of GDP)"),
                          ticksuffix = "%",
                          range=c(0,80)))
  })
  
  # Set up second plot
  
  # Widen the data so that each indicator has its own column
  plot2data <- reactive({
    plot2a<- econ_data %>% 
      pivot_wider(id_cols=c(Country.Name,Country.Code,Year,Region,incomeclass),
                  names_from=Indicator.Code, 
                  values_from=Indicator.Value)
    
    # If an income class is selected, filter countries with that income 
    # classification in the year it has that classification
    if (input$income != "All") {
      plot2a<-plot2a %>% 
        filter(incomeclass==input$income)
    }
    
    # Group the data by both region and year
    plot2b<-plot2a %>% group_by(Region,Year) %>% 
      
      # Create summary statistics for the total regional gdp per capita,
      # weighted averages for each sector's contribution to the GDP, and
      # the number of countries being used to create these summary stats
      summarize(regionalgdp = sum(NY.GDP.PCAP.CD),
                Service = weighted.mean(NV.SRV.TOTL.ZS, w = NY.GDP.PCAP.CD),
                Industry = weighted.mean(NV.IND.TOTL.ZS, w = NY.GDP.PCAP.CD),
                Agriculture = weighted.mean(NV.AGR.TOTL.ZS, w = NY.GDP.PCAP.CD),
                Count = n())
    
    
    # If there is no selected country, further aggregate the data by
    # creating summary statistics for the total world gdp per capita,
    # weighted averages for each sector's contribution to the GDP, and
    # the number of countries being used to create these summary stats
    if (input$country =="World") {
      plot2b %>% 
        group_by(Year) %>% 
        summarize(Service = weighted.mean(Service, w = regionalgdp),
                  Industry = weighted.mean(Industry, w = regionalgdp),
                  Agriculture = weighted.mean(Agriculture, w = regionalgdp),
                  Count = sum(Count)) %>% 
        
        # Lengthen the data so that the sector columns are combined 
        # into one column for easier data visualization
        pivot_longer(cols = c(Service,Industry,Agriculture),
                     names_to = "sector",
                     values_to = "sectorpercent")
    }
    
    # If a country is selected,
    else {
      
      # Determine which region the selected country is in
      plot2region<-econ_data %>% 
        filter(Country.Name == input$country) %>% 
        distinct(Region)
      
      # Lengthen the data so that the sector columns are combined 
      # into one column for easier data visualization 
      plot2b  %>% 
        pivot_longer(cols = c(Service,Industry,Agriculture),
                     names_to = "sector",
                     values_to = "sectorpercent") %>%  
        
        # and filter the data to the region where the selected country is
        filter(Region==plot2region)
    }
  })
  
  # Make the plot 2 region reactive to the country input
  plot2region<-reactive({econ_data %>% 
      filter(Country.Name == input$country) %>% 
      distinct(Region)})
  
  # Create the second plot
  output$Plot2 <- renderPlotly({
    
    # Construct the plotly object  
    plot_ly(
      data=plot2data(),
      
      # Animate the treemap plot by time (year)
      frame=~Year,
      type='treemap',
      
      # Separate the boxes by sector
      labels=~sector,
      
      # Customize the text that shows on the treemap boxes
      text=~Count,
      textinfo="label+value",
      texttemplate = "<b>%{label}</b>\n%{value:.1f}%",
      
      # If the graph is showing the entire world, classify the parent as "World"
      # If the graph is showing one region, classify the parent as the region
      parents= (if (input$country=="World") {"World"} 
                else {paste(plot2region())}),
      
      # Assign the size of the box to the percentage of the sector
      values= ~sectorpercent,
      
      # The size of the parent is determined by the total size of the sectors
      branchvalues = "total",
      
      # Customize the text shown when you hover over a box
      hovertemplate = '<b>%{label}</b>\nValue Added (% of GDP): %{value:.1f}%\nNumber of Countries: %{text}<extra></extra>') %>% 
      
      # Add titles and adjust the text formatting
      layout(title = list(text="Regional Economic Sector Composition",
                          x = 0,
                          xanchor = "left",
                          xref = "paper")) %>% 
      
      # Customize the title of the animation slider
      animation_slider(currentvalue = list(prefix = "Year:"))
  }) 
  
  
  # Make data reactive for Stacked Area Chart
  stackedArea_data <- reactive({
    
    # Input country select data
    sect_data <- if(input$country == "World") {
      global_avg
    } else {
      sector_data |> 
        filter(Country.Name == input$country) |> 
        select(Year, Agriculture, Industry, Service, Country.Name)
    }
    
    # input year animation data
    sect_data |> 
      filter(Year <= input$year_end) |> 
      arrange(Year)
  })
  
  # Make data reactive for trade and sector line chart
  tradeLine_data <- reactive ({
    
    # Input country select data
    trade_data <- if(input$country == "World") {
      global_avg
    } else {
      sector_data |> 
        filter(Country.Name == input$country) |> 
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
            groupnorm = "percent"
    ) |> 
      add_trace(
        y = ~Industry,
        name = "Industry"
      ) |> 
      add_trace(
        y = ~Service,
        name = "Service"
      ) |> 
      layout(
        title = list(text=paste("Sector Composition Over Time:", unique(df$Country.Name)),
                     x = 0,
                     xanchor = "left",
                     xref = "paper"),
        xaxis = list(title = "Year", tickformat = 'd',
                     type = 'category'),
        yaxis = list(title = "Percent of GDP", ticksuffix = "%",
                     hoverformat = '.1f'),
        hovermode = "x unified"
      )
  })
  
  # Create the trade line chart
  output$tradeLine <- renderPlotly ({
    
    # Call the data
    df <- tradeLine_data()
    
    # Construct a line chart
    plot_ly(df, x = ~Year, y = ~Selected.Sector,
            name = input$sector,
            type = "scatter",
            mode = "lines"
    ) |> 
      
      # Add another line to represent trade openness
      add_lines(
        y = ~Trade,
        name = "Trade Openness"
      ) |> 
      
      # Add and customize the axes and hover information
      layout(
        title = list(text=paste(
          "Trade Openness and", input$sector, "Share:",
          unique(df$Country.Name)),
          x = 0,
          xanchor = "left",
          xref = "paper"
        ),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Percent of GDP", ticksuffix = "%",
                     hoverformat = '.1f'),
        hovermode = "x unified"
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


