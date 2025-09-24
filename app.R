library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(reshape2)
library(scales)
library(shiny)
library(bslib)
library(dplyr)
library(ggExtra)
library(rsconnect)

#Load data
data <- read.csv('GlobalYouTubeStatistics.csv')
data
gys <- read.csv('GlobalYouTubeStatistics.csv')

data$Youtuber <- as.factor(data$Youtuber)
########################################################################################
#Preprocessing
########################################################################################
#Find the number of rows with null values in the "Latitude" column
num_null_latitude <- sum(is.nan(data$Latitude))
print(num_null_latitude)

#Filter out rows with null values
filtered_data <- data[complete.cases(data[c("Latitude", "Longitude")]), ]
filtered_data
nrow(filtered_data)

#Find the number of rows with null values in the "Latitude" column
num_null <- sum(is.nan(filtered_data$Latitude))
print(num_null)
########################################################################################
filtered_data2 <- data[complete.cases(data[c("Latitude", "Longitude", "video_views_for_the_last_30_days", "subscribers_for_last_30_days")]), ]
histograms_vars <- filtered_data2[, c("subscribers", "video.views", "uploads", "video_views_for_the_last_30_days", 
                                                      "lowest_monthly_earnings", "highest_monthly_earnings", "lowest_yearly_earnings", 
                                                      "highest_yearly_earnings", "subscribers_for_last_30_days")]
########################################################################################
#Filter out rows with video views equal to 0
filtered_top_channels <- filtered_data %>% filter(`video.views` != 0)

top_channels_by_subscribers <- head(arrange(filtered_top_channels, desc(subscribers)), 10)

top_channels_by_subscribers$Youtuber <- iconv(top_channels_by_subscribers$Youtuber, to = "UTF-8", sub = " ")
print(top_channels_by_subscribers$Youtuber)

top_channels_by_views <- head(arrange(filtered_top_channels, desc(video.views)), 10)

top_channels_by_views$Youtuber <- iconv(top_channels_by_views$Youtuber, to = "UTF-8", sub = " ")
print(top_channels_by_views$Youtuber)
########################################################################################
#Shiny Page Implementation Details
########################################################################################
ui <- fluidPage(
  tags$style(type = "text/css", "#map {overflow: hidden;}"),
  
  titlePanel("YouTube Data Visualization"),
  
  tabsetPanel(
    tabPanel("Map", 
             leafletOutput("map", height = "600px"),
             tags$div(
               style = "font-family: Arial; margin-top: 20px;",
               HTML("This map displays the geographical locations of YouTube channels based on their latitude and longitude coordinates.<br><br>",
                    "<ul>",
                    "<li>Regions with more YouTube channels are displayed with orange and yellow cluster points while regions with less YouTube channels are displayed with green cluster points.</li>",
                    "<li>Click on the regions to see the YouTube channels.</li>",
                    "<li>Each circle marker represents a YouTube channel, and the color of the marker indicates its rank.</li>",
                    "<li>Red markers are within the top 100.</li>",
                    "<li>Purple markers are beyond the top 100.</li>",
                    "<li>Circle markers towards the center of the region span are ranked higher while circle markers towards the outside of the span are ranked lower.</li>",
                    "<li>You can click on the markers to see more information about the channel.</li>",
                    "</ul>",
                    "*Key Finding: The United States and India have the most popular YouTube channels, showing the highest numbers of top YouTubers globally. This insight sheds light on the global distribution of YouTube content creators."
               )
             )
    ),
    tabPanel("Correlation Matrix", 
             div(style = "text-align: center;", plotOutput("heatmap")),
             tags$div(
               style = "font-family: Arial; margin-top: 20px; margin-top: 200px;",
               HTML("This correlation matrix displays the correlation between multiple numerical variables in the dataset.<br><br>",
                    "<ul>",
                    "*Key Findings: <br><br>",
                    "<li>highest_monthly_earnings, lowest_monthly_earnings, highest_yearly_earnings, and lowest_yearly_earnings are all very strongly correlated with each other, having a correlation coefficient of 1.</li>",
                    "<li>The number of subscribers and the number of video views has a correlation coefficient of 0.85.</li>",
                    "<li>Highest and lowest monthy and yearly earnings compared to number of subscribers has a correlation coefficient of 0.47.</li>",
                    "<li>Highest and lowest monthy and yearly earnings compared to number of subscribers gained in the last 30 days has a correlation coefficient of 0.65.</li>",
                    "<li>Highest and lowest monthy and yearly earnings compared to number of video views has a correlation coefficient of 0.57.</li>",
                    "<li>Highest and lowest monthy and yearly earnings compared to number of video views gained in the last 30 days has a correlation coefficient of 0.64.</li>",
                    "<li>There is a weak correlation when comparing number of uploads to number of subscribers, video views, and earnings.</li>",
                    "</ul>",
               )
             )
    ),
    tabPanel("Subscribers vs. Video Views", 
             plotlyOutput("scatter_plot", height = "600px"),
             tags$div(
               style = "font-family: Arial; margin-top: 20px;",
               HTML("This scatter chart plots the relationship of each YouTube channel's number of subscribers and number of video views.<br><br>",
                    "<ul>",
                    "<li>Data points are color-coded by channel category.</li>",
                    "<li>Click on the categories at the right to show or hide channels of that category.</li>",
                    "<li>Mouseover the data points to see information about the channel's name, category, number of subscribers, number of video views, number of video views in the last 30 days, and country of the YouTube channel.</li>",
                    "</ul>",
                    "*Key Finding: The relationship between number of subscribers and number of video views has a strong, positive relationship (correlation coefficient = 0.85). The relationship is not completely linear, but it is clear that as one variable increases, so does the other."
               )
             )
    ),
    tabPanel("Top 10 YouTube Channels", 
             plotlyOutput("combined_visualizations", height = "600px"),
             tags$table(
               style = "width: 100%; border-collapse: collapse;",
               tags$tr(
                 tags$td(
                   style = "font-family: Arial; padding-top: 20px; vertical-align: top;",
                   tags$div(
                     HTML("This bar graph plots the top 10 YouTube channels in the world based on number of subscribers.<br><br>",
                          "<ul>",
                          "<li>The y-axis scale is in millions.</li>",
                          "<li>Mouseover the bars to see information about the channel's name, category, number of subscribers, number of video views, number of video views in the last 30 days, and country of the YouTube channel.</li>",
                          "</ul>"
                     )
                   )
                 ),
                 tags$td(
                   style = "font-family: Arial; padding-top: 20px; vertical-align: top;",
                   tags$div(
                     HTML("This bar graph plots the top 10 YouTube channels in the world based on number of video views.<br><br>",
                          "<ul>",
                          "<li>The y-axis scale is in billions.</li>",
                          "<li>Mouseover the bars to see information about the channel's name, category, number of subscribers, number of video views, number of video views in the last 30 days, and country of the YouTube channel.</li>",
                          "</ul>"
                     )
                   )
                 )
               )
             ),
             tags$div(
               style = "font-family: Arial;",
               HTML("<ul>",
                    "*Key Findings: <br><br>",
                    "<li>We can see that the most successful channels are not individuals but are rather corporations.</li>",
                    "<li>Some of the most successful individual YouTubers on the platform, such as Like Nastya and Vlad and Niki make it onto the top 10 channels by views, and Pewdiepie and Mr. Beast join them in the top 10 channels by subscribers.</li>",
                    "<li>It is common understanding that someone doing YouTube for fun would not be able to make the ranks of the most successful on the platform, but these bar plots we have created provide concrete numerical proof of that understanding.</li>",
                    "</ul>",
               )
             )
    ),
    tabPanel("Median Earnings by Category",
             plotlyOutput("earnings_plot", height = "600px"),
             tags$div(
               style = "font-family: Arial; margin-top: 20px;",
               HTML("This plot displays the median lowest monthly earnings for different YouTube channel categories. Mouseover the bars to see information about the category and the exact lowest monthly earnings.<br><br>",
                    "*Key Finding: It is apparent that Shows, New & Politics, and Movies are three of the most popular categories. However, Autos & Vehicles severely outperforms any of the other categories, with a bar roughly twice as high as the next highest, which is Shows.",
               )
             )
    ),
    tabPanel("Median Subscribers Over the Years",
             plotlyOutput("subscribers_plot", height = "600px"),
             tags$div(
               style = "font-family: Arial; margin-top: 20px;",
               HTML("This plot displays the median number of subscribers for YouTube channels over the years. Mouseover the data points to see information about the creation year and the median number of subscribers.<br><br>",
                    "<ul>",
                    "*Key Findings: <br><br>",
                    "<li>We can see that the year of creation does not seem to have any significant impact on the number of subscribers.</li>",
                    "<li>There is a wave-like pattern where the median number of subscribers rises and falls, but there is no one year that stands out from the others, aside from channels made in 2022 having a median subscriber count of roughly 13 million, which is the lowest median subscriber count; due to the year 2022 being the cutoff year, many channels are likely still gaining subscribers, so this count appears to be an outlier but is actually likely not an outlier. </li>",
                    "<li>There is no conclusive proof in this plot that older channels tend to have more subscribers than newer channels.</li>",
                    "<li>Factors beyond the creation date greatly influence channel popularity.</li>",
                    "</ul>",
               )
             )
    ),
    tabPanel("Histograms",
             sidebarLayout(
               sidebarPanel(
                 selectInput("hist_var", "Select a variable to plot:",
                             choices = names(histograms_vars)[sapply(histograms_vars, is.numeric)],
                             selected = "subscribers"),
                 sliderInput("bins", "Number of bins:", min = 10, max = 100, value = 30)
               ),
               mainPanel(
                 plotlyOutput("histogram_plot", height = "600px"),
                 tags$div(
                   style = "font-family: Arial; margin-top: 20px;",
                   HTML("This page allows you to explore histograms of certain numerical variables.<br><br>
                        <ul>
                          <li>Choose a variable from the dropdown to see its distribution.</li>
                          <li>Adjust the number of bins with the slider.</li>
                          <li>Hover over the bars for exact frequencies.</li>
                        </ul>")
                 )
               )
             )
    ),
    tabPanel("Boxplots",
             tags$div(
               style = "font-family: Arial; margin-top: 20px;",
               HTML("This page shows boxplots of nine key numerical variables in the dataset.<br><br>
                    <ul>
                      <li>Each panel shows the distribution of one variable.</li>
                      <li>The box shows the interquartile range (IQR) and median.</li>
                      <li>Points outside the whiskers are potential outliers.</li>
                    </ul>")
             ),
             plotlyOutput("boxplots_grid1", height = "900px"),
             tags$div(
               style = "font-family: Arial; margin-top: 50px; margin-bottom: 20px; font-weight: bold;",
               HTML("A log transformation is applied to the boxplots below to reduce the effect of outliers.")
             ),
             plotlyOutput("boxplots_grid2", height = "900px")
    ),
    tabPanel("EXTRA: Statistical Analysis",
             fluidRow(
               column(6, plotOutput("coef_plot1", height = "400px")),
               column(6, plotOutput("coef_plot2", height = "400px"))
             ),
             fluidRow(
               column(6,
                      h4("Model 1 Output"),
                      verbatimTextOutput("model1_summary"),
               ),
               column(6,
                      h4("Model 2 Output"),
                      verbatimTextOutput("model2_summary"),
               )
             ),
             fluidRow(
               column(12,
                      tags$div(
                        style = "font-family: Arial; margin-top: 20px;",
                        HTML("<h4>Analysis of Results</h4>
                             <p><b>Model 1:</b> Includes log(subscribers). The coefficient is negative 
                             (–477,000), which is counterintuitive; after controlling for views and growth, 
                             more subscribers are associated with lower earnings. This likely reflects 
                             multicollinearity, since subscribers are highly correlated with video views (correlation=0.85 from correlation matrix tab). 
                             R-squared=0.8284 and Adjusted R-squared=0.8239 indicates strong explanatory power.</p>

                             <p><b>Model 2:</b> Drops log(subscribers). All predictors remain highly 
                             significant and positive (except rank, which is marginal). R-squared=0.8065 is only 
                             slightly lower than Model 1, but the model is simpler and avoids the 
                             interpretability issue of a negative subscriber coefficient.</p>

                             <p><b>Multicollinearity:</b> We can check Variance Inflation Factor (VIF) for multicollinearity; however, we had trouble 
                             with deploying the Shiny App with VIF. Nevertheless, log(subscribers) and log(video.views) 
                             overlap in explanatory power. Removing log(subscribers) improves interpretability 
                             and reduces redundancy without sacrificing too much explanatory power.</p>

                             <p><b>Conclusion:</b> Although Model 1 fits slightly better, Model 2 may be 
                             preferable for practical interpretation because it is simpler, avoids a 
                             counterintuitive negative coefficient, and focuses on the predictors most 
                             directly related to profit: video views and recent growth.</p>")
                      )
               )
             )
    )
    
  )
)

########################################################################################
#Shiny Page Implementation Details + Creating Visualizations
########################################################################################
server <- function(input, output) {
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    map <- map %>% addTiles()
    
    filtered_data$Youtuber <- iconv(filtered_data$Youtuber, to = "UTF-8", sub = " ")
    
    map <- map %>% addCircleMarkers(
      data = filtered_data,
      lng = ~Longitude,
      lat = ~Latitude,
      clusterOptions = markerClusterOptions(
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        animate = TRUE
      ),
      popup = ~paste("Country: ", Country, "<br>YouTuber: ", Youtuber, "<br>Rank: ", rank, "<br>Subscribers: ", subscribers),
      color = ~ifelse(rank < 100, "red", "#483d8b")
    )
    
    map
  })
  
  
  output$heatmap <- renderPlot({
    heatmap_data <- filtered_data2[, c("subscribers", "video.views", "uploads", "video_views_for_the_last_30_days", 
                                       "lowest_monthly_earnings", "highest_monthly_earnings", "lowest_yearly_earnings", 
                                       "highest_yearly_earnings", "subscribers_for_last_30_days", "Population", 
                                       "Gross.tertiary.education.enrollment....", "Unemployment.rate", "Urban_population")]
    
    correlation_matrix <- cor(heatmap_data)
    
    correlation_df <- as.data.frame(as.table(correlation_matrix))
    
    ggplot(correlation_df, aes(x = Var1, y = Var2)) +
      geom_tile(aes(fill = Freq)) +
      geom_text(aes(label = round(Freq, 2)), color = "black", size = 4, angle = 0, vjust = 0.5) +  
      scale_fill_gradient2(low = "#483d8b", high = "firebrick2", mid = "lightgrey", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12)) +
      labs(title = "Correlation Matrix Heat Map")
  }, width = 1000, height = 600)
  
  
  output$scatter_plot <- renderPlotly({
    scatter_data <- data[, c("subscribers", "video.views", "category", "Youtuber", "video_views_for_the_last_30_days", "Country")]
    
    scatter_data$Youtuber <- iconv(scatter_data$Youtuber, to = "UTF-8", sub = " ")
    
    scatter_data$category <- ifelse(scatter_data$category == 'nan', 'Other', scatter_data$category)
    
    no_other_scatter_data <- scatter_data[scatter_data$category != 'Other', ]
    
    scatter_plot <- plot_ly(data = no_other_scatter_data, x = ~subscribers, y = ~`video.views`,
                            text = ~paste("<b>", Youtuber, "</b><br>",
                                          "<br>",
                                          "Category: ", category, "<br>",
                                          "Subscribers: ", subscribers, "<br>",
                                          "Video Views: ", `video.views`, "<br>",
                                          "Views (Last 30 days): ", video_views_for_the_last_30_days, "<br>",
                                          "Country: ", Country),
                            color = ~category, colors = "Set1",
                            type = "scatter", mode = "markers",
                            marker = list(size = 10, opacity = 0.8)) %>%
      layout(title = "Scatter Plot of Subscribers vs. Video Views", margin = list(t = 50),
             xaxis = list(title = "Subscribers"),
             yaxis = list(title = "Video Views"))
    
    scatter_plot
  })
  
  output$combined_visualizations <- renderPlotly({
    bar_chart_subscribers <- plot_ly(data = top_channels_by_subscribers, x = ~reorder(Youtuber, subscribers), y = ~subscribers,
                                     type = "bar",
                                     text = ~paste("<b>", Youtuber, "</b><br>",
                                                   "<br>",
                                                   "Category: ", category, "<br>",
                                                   "Subscribers: ", subscribers, "<br>",
                                                   "Video Views: ", `video.views`, "<br>",
                                                   "Views (Last 30 days): ", video_views_for_the_last_30_days, "<br>",
                                                   "Country: ", Country),
                                     hoverinfo = "text",
                                     textposition = "none",
                                     marker = list(color = "rgb(56, 108, 176)")) %>%
      layout(title = "",
             xaxis = list(title = "YouTube Channel"),
             yaxis = list(title = "Subscribers (Millions)"),
             showlegend = FALSE)
    
    bar_chart_views <- plot_ly(data = top_channels_by_views, x = ~reorder(Youtuber, video.views), y = ~video.views,
                               type = "bar",
                               text = ~paste("<b>", Youtuber, "</b><br>",
                                             "<br>",
                                             "Category: ", category, "<br>",
                                             "Subscribers: ", subscribers, "<br>",
                                             "Video Views: ", `video.views`, "<br>",
                                             "Views (Last 30 days): ", video_views_for_the_last_30_days, "<br>",
                                             "Country: ", Country),
                               hoverinfo = "text",
                               textposition = "none",
                               marker = list(color = "rgb(56, 108, 176)")) %>%
      layout(title = "",
             xaxis = list(title = "YouTube Channel"),
             yaxis = list(title = "Number of Views (Billions)"),
             showlegend = FALSE)
    
    subplot(bar_chart_subscribers, bar_chart_views, nrows = 1) %>%
      layout(annotations = list(
        list(text = "Top 10 YouTube Channels by Subscribers", x = 0.15, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE),
        list(text = "Top 10 YouTube Channels by Video Views", x = 0.75, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE)
      ))
  })
  
  output$earnings_plot <- renderPlotly({
    ec <- select(gys, lowest_monthly_earnings, category, highest_monthly_earnings)
    ec <- ec %>%
      mutate(category = ifelse(category == "nan", "Other", category))
    ecat <- group_by(ec, category)
    ecs <- summarise(ecat, lowest_monthly_earnings = median(lowest_monthly_earnings), highest_monthly_earnings = median(highest_monthly_earnings))
    ecpl <- ggplot(data = ecs) +
      geom_col(aes(x = category, y = lowest_monthly_earnings), color = "#483d8b") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      labs(title = "Median Lowest Monthly Earnings by Category",  
           x = "Category",  
           y = "Median Lowest Monthly Earnings")
    
    ggplotly(ecpl)
  })
  
  output$subscribers_plot <- renderPlotly({
    y_sub <- select(gys, created_year, subscribers)
    ye_subs <- summarise(group_by(y_sub, created_year), median_subs = median(subscribers))
    ye_subs <- ye_subs[-1,]
    
    ysplot <- ggplot(ye_subs) +
      geom_line(aes(x = created_year, y = median_subs)) +
      geom_point(aes(x = created_year, y = median_subs)) +
      ylim(0, 22000000) +
      scale_y_continuous(labels = scales::comma_format()) + 
      labs(title = "Median Subscribers Over the Years",  
           x = "Year",  
           y = "Median Subscribers")
    
    ggplotly(ysplot)
  })
  
  output$histogram_plot <- renderPlotly({
    req(input$hist_var)  # ensure a variable is selected
    
    plot <- ggplot(histograms_vars, aes(x = .data[[input$hist_var]])) +
      geom_histogram(bins = input$bins, fill = "#483d8b", color = "white", alpha = 0.8) +
      labs(title = paste("Histogram of", input$hist_var),
           x = input$hist_var,
           y = "Frequency") +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  output$boxplots_grid1 <- renderPlotly({
    box_data <- filtered_data2[, c("subscribers", "video.views", "uploads", 
                                   "video_views_for_the_last_30_days",
                                   "lowest_monthly_earnings", "highest_monthly_earnings",
                                   "lowest_yearly_earnings", "highest_yearly_earnings",
                                   "subscribers_for_last_30_days")]
    
    box_data_melt <- reshape2::melt(box_data)
    
    p <- ggplot(box_data_melt, aes(x = variable, y = value)) +
      geom_boxplot(fill = "#483d8b", alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
      facet_wrap(~variable, scales = "free", ncol = 3) +
      labs(title = "Boxplots of Key Numerical Variables",
           x = "", y = "Value") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold"))
    
    ggplotly(p)
  })
  
  
  output$boxplots_grid2 <- renderPlotly({
    box_data <- filtered_data2[, c("subscribers", "video.views", "uploads", 
                                   "video_views_for_the_last_30_days",
                                   "lowest_monthly_earnings", "highest_monthly_earnings",
                                   "lowest_yearly_earnings", "highest_yearly_earnings",
                                   "subscribers_for_last_30_days")]
    
    box_data_melt <- reshape2::melt(box_data)
    
    p <- ggplot(box_data_melt, aes(x = variable, y = value)) +
      geom_boxplot(fill = "#483d8b", alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
      facet_wrap(~variable, scales = "free", ncol = 3) +
      labs(title = "Boxplots of Key Numerical Variables",
           x = "", y = "Value") +
      scale_y_continuous(labels = scales::comma) +  
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold")) +
      scale_y_log10(labels = scales::comma)
    
    ggplotly(p)
  })
  
  
  #Subset to US data
  us_data <- filtered_data2 %>% filter(Country == "United States")
  
  model1 <- lm(highest_monthly_earnings ~ 
                 log(subscribers) +
                 log(video.views) +
                 subscribers_for_last_30_days +
                 video_views_for_the_last_30_days + 
                 video_views_rank, data = us_data)
  
  model2 <- lm(highest_monthly_earnings ~ 
                 log(video.views) +
                 subscribers_for_last_30_days +
                 video_views_for_the_last_30_days + 
                 video_views_rank, data = us_data)
  
  output$coef_plot1 <- renderPlot({
    coefs1 <- broom::tidy(model1)
    ggplot(coefs1, aes(x = reorder(term, estimate), y = estimate)) +
      geom_col(fill = "#2e8b57") +
      geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
      coord_flip() +
      labs(title = "Model 1 Coefficients", x = "Predictors", y = "Effect on Earnings")
  })
  
  output$coef_plot2 <- renderPlot({
    coefs2 <- broom::tidy(model2)
    ggplot(coefs2, aes(x = reorder(term, estimate), y = estimate)) +
      geom_col(fill = "#2e8b57") +
      geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
      coord_flip() +
      labs(title = "Model 2 Coefficients", x = "Predictors", y = "Effect on Earnings")
  })
  
  output$model1_summary <- renderPrint({
    summary(model1)
  })
  
  output$model2_summary <- renderPrint({
    summary(model2)
  })
  
}

shinyApp(ui, server)

