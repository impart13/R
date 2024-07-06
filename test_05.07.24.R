library(data.table)
library(ggplot2)
library(gridExtra)
library(shiny)
library(magrittr)
library(reshape2)

# Загрузка данных
data <- fread("E:\\Soft\\CityDataTest.csv")
data

# Фильтрация нулевых значений
data <- data[!is.na(fact)]
data <- data[!duplicated(data), ]

# Сортировка по названию городов
data <- data[order(Город)]

# Отклонение фактической численности от теоретической 
data[, Отклонение := fact - Модель]

# Крайниие границы столбца year 
min_year <- min(data$year)
max_year <- max(data$year)

# Города с частым первышением верхней границы численности
top_cities <- data[fact > `Верхняя граница`, .N, by = .(Город)]
top_cities <- top_cities[order(-N)][1:10]

# Города с частым занижением нижней границы численности
worst_cities <- data[fact < `Нижняя граница`, .N, by = .(Город)]
worst_cities <- worst_cities[order(-N)][1:10]

# Города с высоким приростом численности
top_max <- data[, .(Город, year, Max = ifelse(year == min_year, 0, ((fact - shift(fact, type = "lag"))/shift(fact, type = "lag"))*100))]
top_max <- top_max[order(-Max)]

# Года с наибольши приростом и сокращением численности
yearly_max <- top_max[, .(Прирост = sum(Max)), by = year]
yearly_max <- yearly_max[order(-Прирост)]
yearly_min <- yearly_max[order(Прирост)]

yearly_max

 # Города с высоким приростом численности (2)
'top_max [, .SD[which.max(Max)], by = .(Город)]'
setkey(top_max,Город)
top_max <- top_max[J(unique(Город)),mult="first"]
top_max <- top_max[order(-Max)]

# Города с высоким сокращением численности
top_mim <- data[, .(Город, year, Min = ifelse(year == min_year, 0, ((shift(fact, type = "lag") - fact ))/shift(fact, type = "lag")*100))]
top_min <- top_mim[order(-Min)]

setkey(top_min,Город)
top_min <- top_min[J(unique(Город)),mult="first"]
top_min <- top_min[order(-Min)]

# Интерфейс
ui <- fluidPage(
  titlePanel("Анализ численности населения"),
  
  sidebarLayout(
    # Панель выбора города из списка
    sidebarPanel(
      selectInput("city", "Выберите город:", choices = unique(data$Город)
      ),
      
      # Ползунок выбора временного интервала
      sliderInput("yearR", "Выберите диапазон лет:", min = min_year, max = max_year, value = c(min_year, max_year), step = 1),
      
      #теоретическая численность
      checkboxInput("showBounds", "Показать теоретическую численность", value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Фактическая численность", plotOutput("popPlot", height = "850px")),
        tabPanel("Города с высокой численностью", plotOutput("UpPlot", height = "850px")),
        tabPanel("Города с низкой численностью", plotOutput("DownPlot", height = "850px")),
        tabPanel("Динамика численности по годам", plotOutput("YearlyPlot", height = "850px"))
      )
    )
  )
)

# Серверная логика
server <- function(input, output, session) {
  
  # Реактивная функция для фильтрации данных
  fdata <- reactive({
    data [data$Город == input$city & data$year >= input$yearR[1] & data$year <= input$yearR[2]]
  })
  
  # График численности
  output$popPlot <- renderPlot({
    p1 <- ggplot(fdata(), aes(x = year, y = fact)) +
      geom_line(color = "blue") +
      labs(title = "Фактическая численность населения", x = "Год", y = "Численность населения") +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1))
    
    # Проверка нажария кнопки и отрисовка линий модели
    if (input$showBounds) {
      p1 <- p1 +
      geom_line(aes(y = `Нижняя граница`), linetype = "dashed", color = "red") +
      geom_line(aes(y = `Верхняя граница`), linetype = "dashed", color = "red") +
      geom_line(aes(y = Модель), linetype = "dashed", color = "green")
    }
    p1
  
  # График отклонения
    p2 <- ggplot(fdata(), aes(x = year, y = Отклонение)) +
      geom_line(color = "purple") +
      labs(title = "Отклонение фактической численности населения", x = "Год", y = "Отклонение") +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1))
    
  # Гистограмма отклонения численности от модели
    p3 <- ggplot(fdata(), aes(x = Отклонение)) +
        geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Гистограмма отклонений", x = "Отклонение", y = "Частота") +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1))
    
    grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))
  })
  
  # Топ городов по превышению верхней границы численности
  output$UpPlot <- renderPlot({
    
    p1 <- ggplot(top_cities, aes(y = N, x = Город)) +
      geom_bar(stat = "identity", fill = "blue") +
      ggtitle("Города с частым первышением верхней границы численности")
    
    p2 <- ggplot(top_max[1:5], aes(x = "", y = Max, fill = Город)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      ggtitle("Города с высоким приростом численности") +
      geom_text(aes(label = paste0(round(Max, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
      scale_fill_brewer(palette = "Set1")
    
    grid.arrange(p1, p2, ncol = 1)
  })
  
  # Топ городов по занижению нижней границы численности
  output$DownPlot <- renderPlot({
    p1 <- ggplot(worst_cities, aes(y = N, x = Город)) +
      geom_bar(stat = "identity", fill = "blue") +
      ggtitle("Города с частым занижением нижней границы численности")
    
    p2 <- ggplot(top_min[1:5], aes(x = "", y = Min, fill = Город)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      ggtitle("Города с высоким сокращением численности") +
      geom_text(aes(label = paste0(round(Min, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
      scale_fill_brewer(palette = "Set1")
    
    grid.arrange(p1, p2, ncol = 1)
  })
  
  output$YearlyPlot <- renderPlot({
    ggplot(yearly_max, aes(x = year, y = Прирост)) +
     geom_bar(stat = "identity", fill = "blue") +
     labs(title = "Динамика численности населения по годам", x = "Год", y = "Прирост (%)") +
     theme_minimal()
  })
  
}

# Запуск приложения
shinyApp(ui = ui, server = server)
