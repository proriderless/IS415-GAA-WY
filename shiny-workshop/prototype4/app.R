pacman::p_load(shiny, sf ,tidyverse, tmap, DT)

popdata <- read_csv("data/aspatial/respopagesextod2011to2020.csv")

mpsz <- st_read(dsn = "data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

pop <- popdata %>%
  filter(Time == 2019) %>%
  group_by(PA, SZ, AG) %>%
  summarise(POP = sum(Pop)) %>%
  ungroup()%>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = 0_to_4 +5_to_9 +10_to_14 + 15_to_19 + 20_to_24) %>%
  mutate(ECONOMY ACTIVE = rowSums(.[9:13])+
           rowSums(.[15:17]))%>%
  mutate(AGED=rowSums(.[18:22])) %>%
  mutate(TOTAL=rowSums(.[5:21])) %>%  
  mutate(DEPENDENCY = (YOUNG + AGED)
         /ECONOMY ACTIVE) %>%
  mutate_at(.vars = vars(PA, SZ), 
            .funs = funs(toupper)) %>%
  select(PA, SZ, YOUNG, 
         ECONOMY ACTIVE, AGED, 
         TOTAL, DEPENDENCY) %>%
  filter(ECONOMY ACTIVE > 0)

mpsz_demog <- left_join(mpsz, pop,
                        by = c("SUBZONE_N" = "SZ"))

ui <- fluidPage(
  titlePanel("Choropleth Mapping System"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "variable",
        label = "Mapping Variable",
        choices = list("Young" = "YOUNG",
                       "Economy Active" = "ECONOMY ACTIVE",
                       "Aged" = "AGED",
                       "Dependency" = "DEPENDENCY"
        ),
        selected = "DEPENDENCY"),
      
      selectInput(
        inputId = "classification",
        label = "Classification Method",
        choices = c("pretty" = "pretty",
                    "quantile" = "quantile",
                    "sd" = "sd",
                    "equal" = "equal", 
                    "kmeans" = "kmeans", 
                    "hclust" = "hclust", 
                    "bclust" = "bclust", 
                    "fisher" = "fisher",
                    "jenks" = "jenks"
        ),
        selected = "pretty"),
      
      
      
      sliderInput(
        inputId = "classes",
        label = "Number of Classes",
        min = 6,
        max = 12,
        value = c(6)),
      
      selectInput(
        inputId = "colour",
        label = "Colour scheme",
        choices = c(
          "blues" = "Blues",
          "reds" = "Reds",
          "greens" = "Greens",
          "Yellow-Orange-Red" = "YlOrRd",
          "Yellow-Orange-Brown" = "YlOrBr",
          "Yellow-Green" = "YlGn",
          "Orange-Red" = "OrRd"
        ),
        selected = "YlOrRd"
      )
      
    ),
    
    mainPanel(
      tmapOutput("mapPlot",
                 width = "100%",
                 height = 400)
    )
  )
)

server <- function(input, output){
  
  
  output$mapPlot <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(mpsz_demog) +
      tm_fill(input$variable,
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(alpha = 1, lwd = 0.1) +
      tm_view(set.zoom.limits = c(11,14))
  })
  
}

shinyApp(ui = ui, server = server)