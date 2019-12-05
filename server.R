# server.R
options(shiny.deprecation.messages=FALSE)

# Reading the data
data_whole_olympics <- read.csv("data_whole_olympics.csv")

# Removing art competitions from data
women_subset<- data_whole_olympics[data_whole_olympics$Sport != "Art Competitions",]
# Removing art competitions from data
women_subset_new<- data_whole_olympics[data_whole_olympics$Sport != "Art Competitions",]

# Changing the data to Olympaid data (every 4 years once Olympics held)
old <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(old)) {
  women_subset$Year <- gsub(old[i], new[i], women_subset$Year)
  women_subset_new$Year <- gsub(old[i], new[i], women_subset_new$Year)
}
women_subset$Year <- as.integer(women_subset$Year)
women_subset_new$Year <- as.integer(women_subset_new$Year)

# Grouping the data according to Year and Sex
counts_sex <- women_subset %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

#For SCATTER PLOT

# Removing all the null values from height and weight columns after the Year 1959
data <- women_subset %>% filter(!is.na(Height), !is.na(Weight), Year > 1959) 

# Finding events that are present in all 15 Games since 1960
events <- data[data$Year==1960,"Event"] %>% unique  
years <- data$Year %>% unique %>% sort %>% tail(-1)
for (i in 1:length(years)) {
  nxt <- data[data$Year==years[i],"Event"] %>% unique 
  events <- intersect(events, nxt)
}

# Filter out the data  according to only these events
data <- data %>% filter(Event %in% events)

# Selecting the sports that match these events
sports_events <- data %>% select(Sport, Event) %>% unique

# Removing wrestling, weightlifting, and boxing for simplicity
sports_events <- sports_events %>% 
  filter(!Sport %in% c("Wrestling","Weightlifting","Boxing","Equestrianism")) %>%
  filter(!Event %in% c("Figure Skating Mixed Pairs")) %>%
  arrange(Sport)

# Filtering for Sex
sports_events$Sex <- ifelse(grepl("Women",sports_events$Event),"F","M")

# Fit regression lines for each event
s.height <- s.weight <- c()
for (i in 1:nrow(sports_events)) {
  temp <- data %>% filter(Event == sports_events$Event[i])
  lm.height <- lm(Height ~ Year, data=temp)
  lm.weight <- lm(Weight ~ Year, data=temp)
  s.height[i] <- lm.height$coefficients["Year"]
  s.weight[i] <- lm.weight$coefficients["Year"]
}
slopes <- tibble(Sport = sports_events$Sport, 
                 Event = sports_events$Event,
                 Sex = sports_events$Sex,
                 Height = s.height,
                 Weight = s.weight)

# Multiply by 56 as 56 years passed by from 1960 to 2016
slopes$Height <- round(slopes$Height*56,1)
slopes$Weight <- round(slopes$Weight*56,1)

# For MAP

# Read the dataset containing world co-ordinates
whole_data <- read_excel("whole.xlsx")
# Adding a region column
women_subset$region <- women_subset$Country_x
# Merging the original dataset and the co-ordinates dataset 
women_subset_1 <- left_join(women_subset, whole_data)
# Filtering out 'did not win' medal values and grouping them by various required attributes
women_subset_2 <- women_subset_1 %>% filter(Medal != 'dnw') %>% 
  group_by(Year, Sex, Team, NOC, latitude, longitude, region, Medal) %>% summarize( Medal_Count = length(Medal))
# Converting the Year column to integer datatype
women_subset_2$Year<- as.integer(women_subset_2$Year)

# Ordering the NOC by the total medal count
levels_1 <- women_subset_2 %>%
  group_by(NOC) %>%
  summarize(Total=sum(Medal_Count)) %>%
  arrange(Total) %>%
  select(NOC)
women_subset_2$NOC <- factor(women_subset_2$NOC, levels=levels_1$NOC)

# Filling in latitude and longitude values for United Kingdom region
women_subset_2$latitude[women_subset_2$region == 'UK'] <- 55.378051
women_subset_2$longitude[women_subset_2$region == 'UK'] <- -3.435973
# Removing the null values from the dataset
women_subset_2<- women_subset_2 %>% drop_na()

# Filtering out according to the requried year and medal for female and male separately
women_subset_2_m_g_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'M', Medal == 'Gold')
women_subset_2_m_s_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'M', Medal == 'Silver')
women_subset_2_m_b_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'M', Medal == 'Bronze')
women_subset_2_f_g_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'F', Medal == 'Gold')
women_subset_2_f_s_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'F', Medal == 'Silver')
women_subset_2_f_b_1<- women_subset_2 %>% filter(Year == 1896, Sex == 'F', Medal == 'Bronze')

# Filtering out according to the requried year and medal for female and male separately
women_subset_2_m_g_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'M', Medal == 'Gold')
women_subset_2_m_s_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'M', Medal == 'Silver')
women_subset_2_m_b_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'M', Medal == 'Bronze')
women_subset_2_f_g_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'F', Medal == 'Gold')
women_subset_2_f_s_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'F', Medal == 'Silver')
women_subset_2_f_b_2<- women_subset_2 %>% filter(Year == 1936, Sex == 'F', Medal == 'Bronze')

# Filtering out according to the requried year and medal for female and male separately
women_subset_2_m_g_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'M', Medal == 'Gold')
women_subset_2_m_s_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'M', Medal == 'Silver')
women_subset_2_m_b_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'M', Medal == 'Bronze')
women_subset_2_f_g_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'F', Medal == 'Gold')
women_subset_2_f_s_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'F', Medal == 'Silver')
women_subset_2_f_b_3<- women_subset_2 %>% filter(Year == 1976, Sex == 'F', Medal == 'Bronze')

# Filtering out according to the requried year and medal for female and male separately
women_subset_2_m_g_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'M', Medal == 'Gold')
women_subset_2_m_s_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'M', Medal == 'Silver')
women_subset_2_m_b_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'M', Medal == 'Bronze')
women_subset_2_f_g_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'F', Medal == 'Gold')
women_subset_2_f_s_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'F', Medal == 'Silver')
women_subset_2_f_b_4<- women_subset_2 %>% filter(Year == 2016, Sex == 'F', Medal == 'Bronze')



#For Athletes count
# Read the NOC regions dataset
noc <- read_csv("noc_regions.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))

# Merge the original dataset with the NOC regions dataset and remove the null values in region
data_1 <- women_subset_new %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))

# Filter the data according the required year and get the athletes count for the respective year
athens <- data_1 %>% filter(Games == "1896 Summer") %>% group_by(region) %>% 
  summarise(Athens = length(unique(ID)))
berlin <- data_1 %>% filter(Games == "1936 Summer") %>% group_by(region) %>% 
  summarise(Berlin = length(unique(ID)))
montreal <- data_1 %>% filter(Games == "1976 Summer") %>% group_by(region) %>% 
  summarise(Montreal = length(unique(ID)))
rio <- data_1 %>% filter(Games == "2016 Summer") %>% group_by(region) %>% 
  summarise(Rio = length(unique(ID)))

# Creating data for plotting a map
world_2 <- map_data("world")
mapdat <- tibble(region=unique(world_2$region))
mapdat <- mapdat %>% 
  left_join(athens, by="region") %>%
  left_join(berlin, by="region") %>%
  left_join(montreal, by="region") %>%
  left_join(rio, by="region")
mapdat$Athens[is.na(mapdat$Athens)] <- 0
mapdat$Berlin[is.na(mapdat$Berlin)] <- 0
mapdat$Montreal[is.na(mapdat$Montreal)] <- 0
mapdat$Rio[is.na(mapdat$Rio)] <- 0
world_2 <- left_join(world_2, mapdat, by="region")


#For pyramid graph

# Filter the data according the required year and get the athletes count and the average age
#for the respective year
athens_1 <- data_1 %>% filter(Games == "1896 Summer") %>% group_by(region, Sex) %>% 
  summarise(Athens = length(unique(ID)), Age= mean(Age))
berlin_1 <- data_1 %>% filter(Games == "1936 Summer") %>% group_by(region, Sex) %>% 
  summarise(Berlin = length(unique(ID)),  Age= mean(Age))
montreal_1 <- data_1 %>% filter(Games == "1976 Summer") %>% group_by(region, Sex) %>% 
  summarise(Montreal = length(unique(ID)) ,  Age= mean(Age))
rio_1 <- data_1 %>% filter(Games == "2016 Summer") %>% group_by(region, Sex) %>% 
  summarise(Rio = length(unique(ID)) ,  Age= mean(Age))

# Rounding the age column to integer value
athens_1$Age <- as.integer(round(athens_1$Age,0))
berlin_1$Age <- as.integer(round(berlin_1$Age,0))
montreal_1$Age <- as.integer(round(montreal_1$Age,0))
rio_1$Age <- as.integer(round(rio_1$Age,0))

# Grouping the required data by Sex and Age and count the Athletes
athens_1<- athens_1 %>% group_by(Sex, Age) %>% summarise(Athens = sum(Athens))
berlin_1<- berlin_1 %>% group_by(Sex, Age) %>% summarise(Berlin = sum(Berlin))
montreal_1<- montreal_1 %>% group_by(Sex, Age) %>% summarise(Montreal = sum(Montreal))
rio_1<- rio_1 %>% group_by(Sex, Age) %>% summarise(Rio = sum(Rio))

# Define server logic required to plot various graphs according to the requirement
shinyServer(function(input, output) {
  output$caption <- reactiveText(function(){
    paste("Sex", input$Sex)
  })
  
  # Generate a line plot showing the trend of participation  
  output$plotOne <- renderPlotly({
    Year_2 <- seq(input$Year_2[1], input$Year_2[2])
    df_1 <- counts_sex[which(counts_sex$Year %in% Year_2), ]
    
    x<- df_1 %>% filter(Sex == input$Sex) %>%
    ggplot(aes(x= Year, y=Athletes))  +
      geom_line(size=1, color= "purple")+
      geom_point(size=3, color= "purple") +
      labs(x= 'Year', y= 'Athletes Count',  title = "Number of Participants Over Years across all Countries") +
      theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
    gg<- ggplotly(x)
    style(gg, hoverinfo = "text", hovertext= paste("Year :", counts_sex$Year,
                                                   "<br> Athletes Count :", counts_sex$Athletes))
  })
  
  
  # Plot 2
  # Generate a  scatter plot showing the change in height v/s change in weight for the participants over time
  output$plotTwo <- renderPlotly( {
   y<- slopes %>% filter(Sex == input$Sex) %>%
    ggplot(aes(x=Height, y=Weight, color=Sport, label=Event)) +
      geom_point(alpha=0.75) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      labs(title="Temporal trends in Participant's size in different events",
           x="Height (cm)",
           y="Weight (kg)")  +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="none")
    ggplotly(y)
  })
  
  # Plot3
  # Generate a GIF showing the overall medal count across countries for each year
  output$plotThree <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    world <- ggplot() +
     borders("world", colour = "gray85", fill = "gray80") +
    theme_map() 
    map <- world +
     geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                 data = women_subset_2, alpha = .7, show.legend = FALSE) +
      scale_color_viridis_d()+ 
      scale_size_continuous(range = c(2, 8)) +
      labs(x= 'Longitude', y='Latitude', size = 'Medal Counts', title = "Trend of Medal Counts of Participants from Athens 1896 to Rio 2016")
    p<- map + transition_time(Year) + labs(title = "Trend of Medal Counts of Participants in Year: {frame_time}")
    anim_save("outfile.gif", animate(p))
    
    # Returning a list that has the filename
    list(src = "outfile.gif",
       contentType = 'image/gif',
       width = 650,
       Height= 650,
          alt = "This is alternate text"
   , deleteFile = TRUE)
    
 })
  
  # Plot 4
  # Generate a bubble map chart showing the medal count for various countries for selective years
  output$plotFour <- renderPlotly({
    if(input$Sex == "M" && input$Medal == "Gold" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_g_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1896 Olympics")
      ggplotly(map)
      }
    
    else if(input$Sex == "M" && input$Medal == "Silver" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_s_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1896 Olympics")
     ggplotly(map)
      }
    
    else if(input$Sex == "M" && input$Medal == "Bronze" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_b_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1896 Olympics")
      ggplotly(map)
      }
    
    else if(input$Sex == "F" && input$Medal == "Gold" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_g_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1896 Olympics")
    ggplotly(map)}
    
    else if(input$Sex == "F" && input$Medal == "Silver" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_s_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1896 Olympics")
       ggplotly(map)
      }
    
    else if(input$Sex == "F" && input$Medal == "Bronze" && input$Year== 1896){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_b_1, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1896 Olympics")
     ggplotly(map)
      }
    
    #1936
    
    else if(input$Sex == "M" && input$Medal == "Gold" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_g_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1936 Olympics")
      ggplotly(map)}
    
    else if(input$Sex == "M" && input$Medal == "Silver" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_s_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1936 Olympics")
     ggplotly(map)}
    
    else if(input$Sex == "M" && input$Medal == "Bronze" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_b_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1936 Olympics")
      ggplotly(map)}
    
    else if(input$Sex == "F" && input$Medal == "Gold" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_g_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1936 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "F" && input$Medal == "Silver" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_s_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1936 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "F" && input$Medal == "Bronze" && input$Year== 1936){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_b_2, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1936 Olympics")
      ggplotly(map)}
      
    #1976
    else if(input$Sex == "M" && input$Medal == "Gold" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_g_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
         y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1976 Olympics")
     ggplotly(map)}
    
    else if(input$Sex == "M" && input$Medal == "Silver" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_s_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1976 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "M" && input$Medal == "Bronze" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_b_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 1976 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "F" && input$Medal == "Gold" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_g_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
      y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1976 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "F" && input$Medal == "Silver" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_s_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1976 Olympics")
     ggplotly(map)}
     
    else if(input$Sex == "F" && input$Medal == "Bronze" && input$Year== 1976){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_b_3, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 1976 Olympics")
      ggplotly(map)}
      
    #2016
    else if(input$Sex == "M" && input$Medal == "Gold" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_g_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 2016 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "M" && input$Medal == "Silver" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_s_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 2016 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "M" && input$Medal == "Bronze" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_m_b_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Men at 2016 Olympics")
      ggplotly(map)}
      
    else if(input$Sex == "F" && input$Medal == "Gold" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_g_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
       y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 2016 Olympics")
     ggplotly(map)}
     
    else if(input$Sex == "F" && input$Medal == "Silver" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_s_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
        y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 2016 Olympics")
     ggplotly(map)}
     
    else if(input$Sex == "F" && input$Medal == "Bronze" && input$Year== 2016){
      world <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map() 
      map <- world+ geom_point(aes(x = longitude, y = latitude, size = Medal_Count, color= region),
                               data = women_subset_2_f_b_4, alpha = .7, show.legend = FALSE) +
        scale_color_viridis_d()+ scale_size_continuous(range = c(2, 8)) +labs(x='Longitude',
         y= 'Latitude',size = 'Medal Counts', title = "Medal Counts for Women at 2016 Olympics")
      ggplotly(map)}
      
  })
  
  # Plot 5
  # Generate a chloropleth map showing the trend of atheletes count variation for different countries 
  # for selective years 
  output$plotFive <- renderPlotly({
    # 1896
     if(input$Year_1== 1896){
    l<- ggplot(world_2, aes(x = long, y = lat, group = group, text= region)) +
      geom_polygon(aes(fill = Athens)) +
      labs(title = "Athletes Trend across all Countries at Athens 1896 Olympics",
           x = 'Longitude', y='Latitude') +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "lightblue"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_colourbar(title="Athletes")) +
      scale_fill_gradient(low="white",high="red")
    ggplotly(l)
    }
    # 1936
    else if (input$Year_1== 1936){
      l<- ggplot(world_2, aes(x = long, y = lat, group = group, text= region)) +
        geom_polygon(aes(fill = Berlin)) +
        labs(x='Longitude', y='Latitude', title = "Athletes Trend across all Countries at Berlin 1936 Olympics") +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "lightblue"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient(low="white",high="red")
      ggplotly(l)
      }
    
    # 1976
    else if (input$Year_1== 1976){
      l<- ggplot(world_2, aes(x = long, y = lat, group = group, text= region)) +
        geom_polygon(aes(fill = Montreal)) +
        labs(title = "Athletes Trend across all Countries at Montreal 1976 Olympics",
             x = 'Longitude', y='Latitude') +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "lightblue"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient(low="white",high="red")
     ggplotly(l)
     }
    
    # 2016
    else if(input$Year_1== 2016){
      l<- ggplot(world_2, aes(x = long, y = lat, group = group, text= region)) +
        geom_polygon(aes(fill = Rio)) +
        labs(title = "Athletes Trend across all Countries at Rio 2016 Olympics",
             x = 'Longitude', y='Latitude') +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "lightblue"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient(low="white",high="red")
      ggplotly(l)
      }
  })
  
  # Plot 6
  # Generate a population pyramid graph for comparing the Age of the athelets in selective years
  output$plotSix <- renderPlotly({ 
    if(input$Year_1== 1896){
      m<- ggplot(data = athens_1, 
                 mapping = aes(x = Age, fill = Sex, 
                               y = ifelse(test = Sex == "M", 
                                          yes = -Athens, no = Athens))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = abs, limits = max(athens_1$Athens) * c(-1,1)) +
        labs(x='Age',y = "Athletes Count", 
             title = "Comparison of the Participant's Age Group across all Countries") +
        coord_flip()
      gg<- ggplotly(m)
      style(gg, hoverinfo = "text", hovertext= paste("Age :", athens_1$Age,
                                                     "<br> Athletes Count :", athens_1$Athens,
                                                     "<br> Sex :", athens_1$Sex))
    }
    
    else if (input$Year_1== 1936){
      m<- ggplot(data = berlin_1, 
                 mapping = aes(x = Age, fill = Sex, 
                               y = ifelse(test = Sex == "M", 
                                          yes =-Berlin, no = Berlin))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = abs, limits = max(berlin_1$Berlin) * c(-1,1)) +
        labs(x='Age',y = "Athletes Count", 
             title = "Comparison of the Participant's Age Group across all Countries") +
        coord_flip()
      gg<- ggplotly(m)
      style(gg, hoverinfo = "text", hovertext= paste("Age :", berlin_1$Age,
                                                     "<br> Athletes Count :", berlin_1$Berlin,
                                                     "<br> Sex :", berlin_1$Sex))
    }
    
    else if (input$Year_1== 1976){
      m<- ggplot(data = montreal_1, 
                 mapping = aes(x = Age, fill = Sex, 
                               y = ifelse(test = Sex == "M", 
                                          yes =-Montreal, no = Montreal))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = abs, limits = max(montreal_1$Montreal) * c(-1,1)) +
        labs(x='Age',y = "Athletes Count", 
             title = "Comparison of the Participant's Age Group across all Countries") +
        coord_flip()
      gg<- ggplotly(m)
      style(gg, hoverinfo = "text", hovertext= paste("Age :", montreal_1$Age,
                                                     "<br> Athletes Count :", montreal_1$Montreal,
                                                     "<br> Sex :", montreal_1$Sex))
    }
    
    else if(input$Year_1== 2016){
      m<- ggplot(data = rio_1, 
                 mapping = aes(x = Age, fill = Sex, 
                               y = ifelse(test = Sex == "M", 
                                          yes =-Rio, no = Rio))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = abs, limits = max(rio_1$Rio) * c(-1,1)) +
        labs(x='Age',y = "Athletes Count", 
             title = "Comparison of the Participant's Age Group across all Countries") +
        coord_flip()
      gg<- ggplotly(m)
      style(gg, hoverinfo = "text", hovertext= paste("Age :", rio_1$Age,
                                                     "<br> Athletes Count :", rio_1$Rio,
                                                     "<br> Sex :", rio_1$Sex))
    }
  })
  
  
})

