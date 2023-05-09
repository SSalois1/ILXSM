library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(crosstalk)
library(plotly)
library(sf)
library(sp)
library(broom)
library(rgdal)
library(tidyverse)
source('ilxsm_data_wrangle_functions.R')
# Pull in pre-wrangled length/weight data
setwd(here::here('ilxsm_app/data'))
ml <- isolate_lengths('ILXSM_EntirePull_Wdealerinfo_3_14_21.csv','groundfish_stat_areas')
wt <- isolate_weights('ILXSM_EntirePull_Wdealerinfo_3_14_21.csv','groundfish_stat_areas')
paired <- pair_length_weight(ml, wt)
paired$AREA_CODE <- as.character(paired$AREA_CODE)

## Bring in NAFO shelfbreak shapefiles
wd = here::here('shapefiles')
stat_areas <- rgdal::readOGR(wd,'groundfish_stat_areas') # wd = working directory
proj4string(stat_areas) <- CRS("+init=epsg:4326")
stat_areas2 <- stat_areas[na.omit(stat_areas@data$Id %in% c('622', '537', '632',
                                                            '616', '526', '541', 
                                                            '623', '525', '615',
                                                            '626', '613', '621', 
                                                            '600', '562')),]
sfareas <- st_as_sf(stat_areas2)
# Add sharedata outside ui/server
# Note - can add this in and make interative, but slower
# Check on utility - comparisons across years may be better
# In future - add this to select years to compare, but modulize to make faster? 
ml1 <- ml %>% filter(year == 2021)
ml1 <- na.omit(ml1)
coordinates(ml1) <- c('lon', 'lat')
ml1 <- st_as_sf(ml1)
ml1 <- SharedData$new(ml1)
#ml2 <- ml %>% filter(year == input$Year)
ml2 <- ml %>% filter(year == 2022)
ml2 <- na.omit(ml2)
coordinates(ml2) <- c('lon', 'lat')
ml2 <- st_as_sf(ml2)
ml2 <- SharedData$new(ml2)
# Define UI ----
ui <- fluidPage(
  titlePanel("Visualizing ILXSM data"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Select times and locations of interest"),
      p(""),
      br(),
      # selectInput(input = "Year", label= "Year:",
      #             choices = unique(ml$year), selected ="2021"),
      selectInput('var2', 
                  label = 'GARFO Fisheries Statistical Areas', 
                  choices = c('525', '526', '537', '541', '613',
                              '615', '616', '621', '622', '623', 
                              '626', '632'),
                  selected = '525'),
      sliderInput('range', 
                  label = 'Week', 
                  min = 14, max = 35, value = c(14,35)),
      # br(),
      # plotOutput('map'),
      
    ), 
    mainPanel(
      h1('Shortfin Squid Electronic Size Monitoring'), 
      p('A collaboration between NOAA researchers and Industry 
        partners to collect high frequency, region-wide, 
        size and weight sampling of shortfin squid'),
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram",
                           fluidRow(
                             shinydashboard::box(width = 6, title = '2021', status = "primary", 
                                                 solidHeader = TRUE,
                                                 #collapsible = TRUE,
                                                 plotOutput("histPlot1")),
                             shinydashboard::box(width = 6, title = '2022', 
                                                 solidHeader = TRUE,
                                                 #collapsible = TRUE,
                                                 plotOutput('histPlot2')),
                           ),
                           fluidRow(
                             shinydashboard::box(width = 6, #title = '2021', status = "primary", 
                                                 solidHeader = TRUE,
                                                 #collapsible = TRUE,
                                                 plotOutput("histPlot3")),
                             shinydashboard::box(width = 6, #title = '2022', 
                                                 solidHeader = TRUE,
                                                 #collapsible = TRUE,
                                                 plotOutput('histPlot4')),
                           ),
                  ),
                  tabPanel("Map",
                           h2('Map of summary statistics for Illex lengths'), 
                           p('Select year of interest from side panel and 
                             then mouse over area of interest on map 
                             to display site-specific information'),
                           plotlyOutput(output ="map"),
                           plotlyOutput("scatterplot"), # add images
                           
                  ),
                  tabPanel("Highlighting Maps",
                           fluidRow(
                             bscols(plotlyOutput("highlighthist"), 
                                    plotlyOutput("map2"))
                           ),
                           fluidRow(
                             bscols(plotlyOutput("highlighthist2"), 
                                    plotlyOutput("map3"))
                           ),
                  ),
                  tabPanel("Fleet dynamics",
                           fluidRow(
                             shinydashboard::box(width = 6, title = 'Freezer Trawlers', 
                                                 status = "primary", 
                                                 solidHeader = TRUE,
                                                 plotOutput("boxplot_wt1")),
                             shinydashboard::box(width = 6, title = 'Wet Boats', 
                                                 solidHeader = TRUE,
                                                 plotOutput("boxplot_wt2"))
                           ),
                           fluidRow(
                             shinydashboard::box(plotOutput("boxplot_ml1")),
                             shinydashboard::box(plotOutput("boxplot_ml2"))
                           ),
                  ),
                  tabPanel('Relative Condition',
                           fluidRow(
                             shinydashboard::box(width = 12,
                                                 title = 'Relative Condition
                                                 Within Statistical Areas', 
                                                 status = "primary", 
                                                 solidHeader = TRUE,
                                                 plotOutput("boxplot_kn")),
                             
                           ),
                           fluidRow(
                             shinydashboard::box(width = 12, 
                                                 title = 'Mean Pop rel Condition
                                                 Across Years', 
                                                 plotOutput("boxplot_kn2")),
                           ),
                           fluidRow(
                             shinydashboard::box(width = 12, 
                                                 title = 'Relative Condition
                                                 Across Years', 
                                                 plotOutput("boxplot_kn3")),
                           ),
                  ),
                  tabPanel('Age at weight',
                           fluidRow(
                             shinydashboard::box(width = 12,
                                                 title = 'Estimating age at weight', 
                                                 status = "primary", 
                                                 solidHeader = TRUE,
                                                 plotOutput("hist_age")),
                             
                           ),
                           fluidRow(
                             shinydashboard::box(width = 12, 
                                                 title = 'Estimating birth week at age', 
                                                 plotOutput("hist_birth")),
                           ),
                  ),
      ),
      br(),
      p('For more information or to contact us, visit the',
        a('Cooperative Research Science and Data Page', 
          href = 'https://www.fisheries.noaa.gov/new-england-mid-atlantic/science-data/cooperative-research-northeast#shortfin-squid-electronic-size-monitoring')),
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # formulaText <- reactive({
  #   paste("Year ~", input$var1)
  # })
  # 
  # output$selected_var1 <- renderText({
  #   paste('Displaying data from:', input$var1)
  # })
  
  formulaText <- reactive({
    paste('Statisical Area: ~', input$var2)
  })
  
  output$selected_var2 <- renderText({
    paste('In Statisical Area:', input$var2)
  })
  
  formulaText <- reactive({
    paste('For weeks ranging from: ~',
          input$range[1], 'to', input$range[2])
  })
  
  output$min_max <- renderText({
    paste('For weeks ranging from:',
          input$range[1], 'to', input$range[2])
  })
  
  # Generate the plots
  output$histPlot1 <- renderPlot({
    
    df <- subset(wt, year == 2021) #year %in% input$var1
    ggplot(df %>% filter(AREA_CODE == input$var2 & 
                           week %in% c(input$range[1]:input$range[2])), 
           aes(x = weight)) +
      geom_histogram(aes(y = ..density..), 
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      xlab('Weight (g)') +
      ylab('Density') + 
      xlim(0,500) +
      theme_classic()
    
  })
  
  output$histPlot2 <- renderPlot({
    
    df2 <- subset(wt, year == 2022) # %in% input$var1
    ggplot(df2 %>% filter(AREA_CODE == input$var2& 
                            week %in% c(input$range[1]:input$range[2])), 
           aes(x = weight)) +
      geom_histogram(aes(y = ..density..), 
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      xlab('Weight (g)') +
      ylab('Density') + 
      xlim(0,500) +
      theme_classic()
    
  })
  
  output$histPlot3 <- renderPlot({
    
    df <- subset(ml, year == 2021) #year %in% input$var1
    ggplot(df %>% filter(AREA_CODE == input$var2& 
                           week %in% c(input$range[1]:input$range[2])), 
           aes(x = length)) +
      geom_histogram(aes(y = ..density..), 
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      xlab('Mantle Length (mm)') +
      ylab('Density') + 
      xlim(0,300) +
      theme_classic()
    
  })
  
  output$histPlot4 <- renderPlot({
    
    df2 <- subset(ml, year == 2022) # %in% input$var1
    ggplot(df2 %>% filter(AREA_CODE == input$var2& 
                            week %in% c(input$range[1]:input$range[2])), 
           aes(x = length)) +
      geom_histogram(aes(y = ..density..), 
                     colour = "black",
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      #labs(title = '2022') +
      xlab('Mantle Length (mm)') +
      ylab('Density') + 
      xlim(0,300) +
      theme_classic()
    
  })
  
  mapkey <- sfareas %>%  
    highlight_key(~Id) # set highlight key prior to plotting
  
  output$map <- renderPlotly({
      p <- ggplot(mapkey) +
        geom_sf(aes(fill = Id), show.legend = FALSE) +
        geom_sf_label(data = mapkey,aes(label = Id)) +
        labs(x = 'Longitude', y = 'Latitude') +
        theme_classic()
      ggplotly(p, source = 'subset') %>%
        style(p, hoveron = 'fills', fillcolor = toRGB("lightblue"))
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "subset")
    # s[x,y] --> s[curveNumber,key]
    print(s)
    d = paired[paired$AREA_CODE==s$key,]
    ggplot(d,  
           aes(x=length, y=weight)) +
      geom_point(color = 'grey32') + 
      geom_density_2d() +
      geom_density_2d(colour = 'dodgerblue1') +
      scale_fill_brewer() +
      xlab('Mantle Length (mm)') +
      ylab('Body Weight (g)') +
      labs(fill = 'Count') +
      theme_bw() +
      facet_wrap(~year)
        # ggplot(d, 
        #      aes(x=length, y=weight)) +
        # geom_hex() +
        # scale_fill_gradientn(colours=pal) +
        # xlab('Mantle Length (mm)') +
        # ylab('Body Weight (g)') +
        # labs(fill = 'Count') +
        # ecodata::theme_facet() +
        # facet_wrap(~year)
  })
  
  
  output$highlighthist <- renderPlotly({
   
    # histogram length
    length_hist <- plot_ly(ml1, x = ~ length) %>%
      add_histogram(xbins = list(start = 0, end = 500)) %>%
      layout(barmode = "overlay",
             title = '2021',
             xaxis = list(title = 'Mantle length (mm)',
                          range=c(0,300)), 
             yaxis = list(title = 'Frequency')
               ) %>%
      highlight(on = "plotly_selected", persistent = FALSE)
  })
  
  output$map2 <- renderPlotly({
    
    # map of stat areas
    g <- list(
      fitbounds = "locations",
      showland = TRUE,
      landcolor = toRGB("#CDBA96")
    )
    length_map <- plot_geo(ml1, mode = 'markers', 
                           marker = list(size = 12), 
                           text = ~paste('Stat area:', AREA_CODE)) %>% 
      layout(geo = g) %>%
    highlight(persistent = F, dynamic = T)
    
    })
  
  output$highlighthist2 <- renderPlotly({
  
    # histogram length
    length_hist <- plot_ly(ml2, x = ~ length) %>%
      add_histogram(xbins = list(start = 0, end = 500)) %>%
      layout(barmode = "overlay",
             title = '2022',
             xaxis = list(title = 'Mantle length (mm)',
                          range=c(0,300)), 
             yaxis = list(title = 'Frequency')) %>%
      highlight(on = "plotly_selected", persistent = FALSE)
  })
  
  output$map3 <- renderPlotly({
    
    # map of stat areas
    g <- list(
      fitbounds = "locations",
      showland = TRUE,
      landcolor = toRGB("#CDBA96")
    )
    length_map <- plot_geo(ml2, mode = 'markers', 
                           marker = list(size = 12), 
                           text = ~paste('Stat area:', AREA_CODE)) %>% 
      layout(geo = g) %>%
      highlight(persistent = F, dynamic = T)
    
  })
  
  
  output$boxplot_wt1 <- renderPlot({
    ## Create boxplot by fleet, color = factor(year)
    ggplot(wt %>% filter(hold_type == 'FT'), 
           aes(x = factor(vessel_id), y = weight, fill = as.factor(year)))+ 
      labs(x = 'Vessel', y = 'Weight (grams)', 
           title = 'Fleet: Freezer Trawlers', 
           subtitle = 'Weights') +
      scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
      theme_classic() + 
      guides(fill=guide_legend(title=NULL))+
      #facet_wrap(~factor(hold_type))
      geom_boxplot()
    
  })
  
  output$boxplot_wt2 <- renderPlot({
    ggplot(wt %>% filter(hold_type %in% c('RSW', 'Ice')), 
           aes(x = factor(vessel_id), y = weight, fill = as.factor(year)))+ 
      labs(x = 'Vessel', y = 'Weight (grams)', 
           title = 'Fleet: Wet boats', 
           subtitle = 'Weights') +
      scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
      theme_classic() + 
      guides(fill=guide_legend(title=NULL))+
      geom_boxplot()
    
  })
  
  output$boxplot_ml1 <- renderPlot({
    ## Create boxplot by fleet, color = factor(year)
    ggplot(ml %>% filter(hold_type == 'FT'& length <= 500), 
           aes(x = factor(vessel_id), y = length, fill = as.factor(year)))+ 
      labs(x = 'Vessel', y = 'Mantle length (mm)', 
           title = 'Fleet: Freezer Trawlers', 
           subtitle = 'Lengths') +
      scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
      theme_classic() + 
      guides(fill=guide_legend(title=NULL))+
      geom_boxplot()
    
  })
  
  output$boxplot_ml2 <- renderPlot({
    ggplot(ml %>% filter(hold_type %in% c('RSW', 'Ice') & length <= 500), 
           aes(x = factor(vessel_id), y = length, fill = as.factor(year)))+ 
      labs(x = 'Vessel', y = 'Mantle length (mm)', 
           title = 'Fleet: Wet boats', 
           subtitle = 'Lengths') +
      scale_fill_manual(values = c("#F1BB7B","#FD6467")) +
      theme_classic() + 
      guides(fill=guide_legend(title=NULL))+
      geom_boxplot()
    
  })
  
  output$boxplot_kn <- renderPlot({
    ggplot(dat %>% filter(AREA_CODE == input$var2),
           aes(x = factor(AREA_CODE), y = cond, fill = as.factor(year)))+ 
      labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
           title = '2021 vs 2022') +
      theme_classic() +
      guides(fill=guide_legend(title=NULL))+
      geom_hline(yintercept = 1, lty = 2) +
      geom_boxplot()
    
  })
  
  output$boxplot_kn2 <- renderPlot({
    ggplot(dat, aes(x = factor(AREA_CODE), y = cond, fill = as.factor(year))) + 
      labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
           title = '2021 vs 2022') +
      theme_classic() +
      guides(fill=guide_legend(title=NULL))+
      geom_hline(yintercept = 1, lty = 2) +
      geom_boxplot()
    
  })
  output$boxplot_kn3 <- renderPlot({
    ggplot(dat, aes(x = factor(AREA_CODE), y = pop_kn, fill = as.factor(year))) + 
      labs(x = 'Statistical Area', y = 'Relative Condition (Kn)',
           title = '2021 vs 2022') +
      theme_classic() +
      guides(fill=guide_legend(title=NULL))+
      geom_hline(yintercept = 1, lty = 2) +
      geom_boxplot()
    
  })
  
  # output$hist_age <- renderPlot({
  #   ggplot(dat, aes(x = factor( ), y = pop_kn, fill = as.factor(year))) + 
  #     labs(x = 'Age in days', y = 'Frequency',
  #          title = '2021 vs 2022') +
  #     theme_classic() +
  #     guides(fill=guide_legend(title=NULL))+
  #     geom_hline(yintercept = 1, lty = 2) +
  #     geom_boxplot() +
  #     facet_wrap(~year)
  #   
  # })
  
  # output$hist_birth <- renderPlot({
  #   ggplot(dat, aes(x = factor(AREA_CODE), y = pop_kn, fill = as.factor(year))) + 
  #     labs(x = 'Birth week', y = 'Frequncy',
  #          title = '2021 vs 2022') +
  #     theme_classic() +
  #     guides(fill=guide_legend(title=NULL))+
  #     geom_hline(yintercept = 1, lty = 2) +
  #     geom_boxplot()
  #   
  # })
}

# Run the app ----
shinyApp(ui = ui, server = server)

