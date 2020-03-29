library(shiny)
library(readxl)
require(caret)
library(tibble)
library(factoextra) 
library(NbClust)
library(rworldmap)
library(rworldxtra)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Assignment 1"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("dimension", "Index of dimension:", min = 1, max = 19, value = 1),
         sliderTextInput("k", "Choice of k:", c(2,3,5,6,7), selected = 3)
         ),
        
      # Show a plot of the generated distribution
      mainPanel(
        
        fluidRow(
         h3("PCA"),  
         h4("Contributions of each dimension:"),
         plotOutput("plot1"),
         h6("By dimension 6, above 85% is covered. This can be verified by looking at contributions as all metrics make significant contributions in the top 6 dimensions."),
         h3("K Means"),
         h6("According to the majority rule, the best number of clusters is 3."),
         h4("Clusters represented textually"),
         plotOutput("plot3"),
         h4("Clusters shown on world map"),
         plotOutput("plot4"),
         h4("NbClust metrics analysis"),
         h6("Among all indices:"),                                          
         h6("8 proposed 2 as the best number of clusters"),
         h6("9 proposed 3 as the best number of clusters"), 
         h6("2 proposed 5 as the best number of clusters"),
         h6("1 proposed 6 as the best number of clusters"), 
         h6("2 proposed 7 as the best number of clusters"), 
         h6("1 proposed 10 as the best number of clusters"), 
         h6("3 is the most suitable number of clusters, as it accurately separates upper, middle and lower developed countries."),
         plotOutput("plot2"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
      df <- read_excel("CountryData.xlsx")
      df<- df[c(1:21)]
      names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")
      set.seed(0)
      
      #we impute missing values with a random forest
      imputation_model = preProcess(x = df[,-c(1,2)],method = "bagImpute")
      imputated_data = predict(object = imputation_model,newdata=df[,-c(1,2)])
      # Adding country names to the rows
      imputated_data <-cbind(df[,2], imputated_data)
      imputated_data <- column_to_rownames(imputated_data, "CountryName")
      scaled_data = scale(imputated_data)
      pca.out<-prcomp(imputated_data,scale=TRUE)
      
      km.res <- reactive({
        kmeans(scaled_data, input$k, nstart = 50)
      })
    
      output$plot1 <- renderPlot({
        fviz_contrib(pca.out, choice="var", axes = input$dimension, top = 19)
      }) 
      
      output$plot2 <- renderPlot({
        NbClust(scaled_data, distance = "euclidean", min.nc=2, max.nc=10, 
                method = "kmeans", index = "all")
      })
      
      output$plot3 <- renderPlot({
        fviz_cluster(km.res(), data = scaled_data, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
      })
      
      output$plot4 <- renderPlot({
        cluster = as.numeric(km.res()$cluster)
        spdf = joinCountryData2Map(data.frame(cluster,df$CountryName), joinCode="NAME", nameJoinColumn="df.CountryName",verbose = TRUE,mapResolution = "low")
        mapCountryData(spdf, nameColumnToPlot="cluster", catMethod="fixedWidth", addLegend = FALSE, lwd = 0.5)
      })
      
      }

# Run the application 
shinyApp(ui = ui, server = server)

