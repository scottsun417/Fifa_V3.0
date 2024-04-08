library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(maps)
library(ggrepel)
#input: slider input year ; select input  position

df=read.csv("curated_data.csv")
coor=read.csv("world_country_latitude_and_longitude_values.csv")
coor=coor %>%
  select(latitude, longitude, country)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    df1=df %>%
      select(year, short_name, nationality)
    df1$pos_value=df[,input$position]
    df1=df1 %>%
      filter(year==input$year) %>%
      left_join(coor,by=c("nationality"="country")) %>%
      filter(!is.na(latitude), !is.na(longitude))
    df1=df1 %>%
      arrange(desc(pos_value))
    df1=df1[1:10,]
    mp<-NULL
    mapworld<-borders("world",colour = "gray50",fill="white")
    mp<-ggplot()+mapworld+ylim(-60,90)
    mp2<-mp+geom_point(aes(x=df1$longitude,y=df1$latitude),color="darkorange")+scale_size(range=c(1.5,1.5))
    mp3<-mp2+geom_text_repel(aes(x=df1$longitude,y=df1$latitude,label = df1$short_name), col='darkblue',segment.size = 0.2)
    mp4<-mp3+theme(legend.position = "none")+labs(title="Top 10 Players of each position") 
    mp4
  }, res = 96)
  
  output$plot2 <- renderPlot({
    df2=df %>%
      filter(year==input$year, wage_eur!=0)
    ggplot(df2) +
      geom_point(aes(x = wage_eur, y = overall)) +
      labs(x = "wage", y = "rating",
           title = "Relationship between Rating and Wage") +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank()
      )
  }, res = 96)
  
  output$plot3 <- renderPlot({
    pos_value=df[,input$position]
    df30=df
    df30$pos_value=pos_value
    df3=df30 %>%
      filter(!is.na(wage_eur),!is.na(pos_value)) %>%
      group_by(year) %>%
      summarise(pos_max=max(pos_value), wage_max=max(wage_eur))
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Wage"
    )
    fig <- plot_ly()
    fig <- fig %>% add_lines(x = ~df3$year, y = ~df3$pos_max, name = "value")
    fig <- fig %>% add_lines(x = ~df3$year, y = ~df3$wage_max, name = "wage", yaxis = "y2")
    fig <- fig %>% layout(
      yaxis = list(
        side = "left",
        title = "Value"
      ),
      title = "Position Highest Value and Highest Wage", yaxis2 = ay,
      xaxis = list(title="Year"),legend = list(orientation = "v", y = 0.75, x = 1)
    )
    fig
  }, res = 96)
  
}


