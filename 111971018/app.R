library(shiny)
library(shinydashboard)
library(ggbiplot)
library(ggplot2)
# library(datasets)
library(DT)
# library(stats)
library(FactoMineR)
library(factoextra)

# 自定義函數
iris_data <- reactive({
    iris
})
pca_data <- reactive({
    iris_data <- iris[,1:4]
    iris_pca <- PCA(iris_data[,1:4], graph = FALSE)
    data.frame(iris_pca$ind$coord)
  })


# 將iris資料集轉換為data.frame
# 定義Shiny應用程序UI
ui <- dashboardPage(
  dashboardHeader(title = "Sophia的作業4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PCA分析", tabName = "pca_analysis", icon = icon("magnifying-glass-chart")),
      menuItem("CA分析", tabName = "ca_analysis", icon = icon("uncharted")),
      menuItem("Information", tabName = "sophia", icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("pca_analysis",
              fluidPage(

                column(
                  width = 3,
                  height = 12,
                    titlePanel("PCA Result"),
                    selectInput("select_pca_x", "X軸資料", 
                                choices = paste0("PC", 1:4)),
                    selectInput("select_pca_y", "Y軸資料", 
                                choices = paste0("PC", 1:4), 
                                selected = "PC2"),
                    sliderInput("slider_pca", "要進行分析的資料數量",
                                min = 10, max = 150, value = 150, step = 1)
                ),
                column(
                  width = 9,
                  height = 12,
                  navbarPage("分頁",
                      tabPanel("分佈圖",
                        plotOutput("pca_plot"),
                      ),
                      tabPanel("PCA資料",
                        verbatimTextOutput("pca_data_summary")
                      ),
                      tabPanel("原始資料",
                        dataTableOutput("pca_iris_table")
                      )
                    )
                )
              )
            ),
      tabItem("ca_analysis",
              fluidPage(
                column(
                  width = 3,
                  height = 12,
                    titlePanel("CA Result"),
                    selectInput("select_ca_x", "X軸資料",
                                choices = paste0("PC", 1:4)),
                    selectInput("select_ca_y", "Y軸資料",
                                choices = paste0("PC", 1:4),
                                selected = "PC2"),
                    sliderInput("slider_ca", "要進行分析的資料數量",
                              min = 10, max = 150, value = 150, step = 1)
                ),
                column(
                  width = 9,
                  height = 12,
                  navbarPage("分頁",
                    tabPanel("分佈圖",
                      plotOutput("ca_plot"),
                    ),
                    tabPanel("CA資料",
                      dataTableOutput("ca_table")
                    ),
                    tabPanel("原始資料",
                      dataTableOutput("ca_iris_table")
                    ),
                  )
                )
              )
      ),
      tabItem("sophia",
      titlePanel("資料科學系在職專班"),
      titlePanel("111971018"),
      titlePanel("李姿瑩")
      )
    )
  ),
  # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_by_chatGPT.css"))
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
)

  library(ggbiplot)
# 定義Shiny應用程序伺服器
server <- function(input, output, session) {

  data(iris)
  iris_df <- data.frame(iris)

  # PCA分析 
  observe({
    log.ir <- log(iris[1:input$slider_pca, 1:4])
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    ir.species <- iris[1:input$slider_pca, 5]

    # 分佈圖
    output$pca_plot <- renderPlot({
      pca_x <- as.numeric(gsub("\\D+", "", input$select_pca_x))
      pca_y <- as.numeric(gsub("\\D+", "", input$select_pca_y))
      my_margin <- unit(c(0, 0, 0, 0), "cm")
      ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, 
               choices = c(pca_x, pca_y), circle = TRUE, ellipse = TRUE ,margin = my_margin) +
        scale_color_discrete(name = '') +
        theme(
          legend.direction = 'horizontal', legend.position = 'top',
          panel.background = element_rect(fill = "#edf6f9"),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(family = "Georgia", face = "bold", size = 20),
          plot.subtitle = element_text(family = "Georgia", face = "italic", size = 16),
          legend.text = element_text(color = "#006d77", family = "Courier"),
          legend.title = element_text(color = "orange", family = "Times New Roman"),
          legend.background = element_rect(fill = "#edf6f9"),
           panel.border = element_rect(color = "black", fill = NA, size = 1),
          #  panel.spacing = unit(0, "mm")
        )
    })
    # theme(
    #   axis.line = element_line(linetype = "dashed"),
    #   plot.background = element_rect(fill = "pink"),
    #     panel.background = element_rect(fill = "lightpink"),
    #     plot.title = element_text(family = "Georgia", face = "bold", size = 20),
    #     plot.subtitle = element_text(family = "Georgia", face = "italic", size = 16)
    #   )
    # PCA Data
    output$pca_data_summary <- renderPrint({
      summary(ir.pca)
    })
    # 原始資料
    output$pca_iris_table <- DT::renderDataTable({
    datatable(iris[1:input$slider_pca, ],
              options = list(
                pageLength = 10, # 設置每頁顯示行數為10
                scrollY = "auto" # 設置表格高度為300像素
              ))
    })

  })

  # CA分析
  observe({
    log.ir <- log(iris[1:input$slider_ca, 1:4])
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    ir.species <- iris[1:input$slider_ca, 5]

    # 分佈圖
    output$ca_plot <- renderPlot({
      ca_data <- iris[1:input$slider_ca, -5]
      ca_result <- CA(ca_data, graph = FALSE)
      fviz_ca(ca_result,
              col.row = iris[1:input$slider_ca, 5],
              col.col = "black",
              # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
              )
    })
    # CA資料
    output$ca_table <- renderDataTable({
      ca_data <- iris[1:input$slider_ca, -5]
      # ca_data <- PCA(ca_data, graph = FALSE)
      ca_res <- CA(ca_data, graph = FALSE)
      as.data.frame(ca_res$col$coord)
    })

    # 原始資料
    output$ca_iris_table <- DT::renderDataTable({
    datatable(iris[1:input$slider_ca, ],
              options = list(
                pageLength = 10, # 設置每頁顯示行數為10
                scrollY = "auto" # 設置表格高度為300像素
              ))
    })

  })

}

# 執行Shiny應用程序
shinyApp(ui = ui, server = server)
