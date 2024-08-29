packages <- c(
  "shiny",
  "shinydashboard",
  "shinydashboardPlus",
  "dplyr",
  "plyr",
  "readxl",
  "shinythemes",
  "ggplot2",
  "plotly",
  "gt",
  "tidyr",
  "DT",
  "writexl"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

lapply(packages, install_if_missing)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(gt)
library(tidyr)


# Shiny UI
ui <- dashboardPage(
  skin = "green",
  options = list(sidebarExpandOnHover = TRUE),
  header = dashboardHeader(title = "DiscrimHub", titleWidth = 300),
  sidebar = dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("A Special Case of Brennan’s Index", tabName = "Bul"),
      menuItem("Brennan's Index", tabName = "brennan_index"),
      menuItem("Other Discrimination Indexes", tabName = "other_indexes"),
      menuItem(
        "Item Response Curves",
        tabName = "item_plots",
        icon = icon("chart-line")
      ),
      menuItem(
        "For further information",
        tabName = "information",
        icon = icon("info-circle")
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload", fluidRow(
        box(
          title = "Data Upload",
          width = 6,
          solidHeader = TRUE,
          status = "success",
          
          helpText(
            HTML(
              "<b> Upload an Excel file (.xlsx or .xls) that meets the following criteria:</b><br>
          - Each row should represent the responses of a single student. <br>
          - Each column should contain responses to a single item. <br>
          - The first row must contain item names as column headings. <br>
          - For the Variable Name Pattern: <br>
          If the column headings are Item1, Item2, ..., Itemx, type Item. If the column headings are i_1, i_2, ..., i_x, type i_, etc. <br>     
          - Use 0 for wrong answers and 1 for correct answers. <br> 
            </ol>"
            )
          ),
          
          fileInput("data", "Upload Data File (.xlsx or .xls):", accept = c(".xls", ".xlsx")),
          textInput("custom_pattern", "Variable Name Pattern (e.g., Item)", value = "Item"),
          actionButton("analyze", "Analyze" 
                     #icon("paper-plane"), 
                     #class= "btn-success"
                     ),
          tags$hr(),
          checkboxInput("sample_data", "Use sample data instead", value = FALSE),
          downloadButton("download_sample", "Download Sample Data")
        ),
        box(
          title = "Data Preview",
          width = 6,
          solidHeader = TRUE,
          status = "success",
          DTOutput("data_preview")
        )
      ), fluidRow(
        box(
          title = "Frequency Distribution of Total Scores",
          width = 8,
          solidHeader = TRUE,
          status = "success",
          textOutput("data_summary"),
          plotlyOutput("total_score_frequency", height = "400px")
        ),
        box(
          title = "Descriptive Statistics for the Items and the Total Score",
          width = 4,
          solidHeader = TRUE,
          status = "success",
          gt_output("descriptive_stats")
        )
      ))
      ,
      
      tabItem(tabName = "Bul", fluidRow(
        box(
          title = "Parameters",
          width = 5,
          solidHeader = TRUE,
          status = "success",
          numericInput(
            "upper_percentage",
            "Upper Percentage:",
            10,
            min = 1,
            max = 99
          ),
          actionButton("analyze_brennan", "Analyze"),
          helpText(
            HTML(
              "<b>A Special Case of Brennan’s Index:</b><br>
                            <p>When the purpose is to select a limited group of examinees, such as only the top 10%, the widely used Classical Test Theory (CTT) and Item Response Theory (IRT) item discrimination indices may be biased because they estimate how well the entire score range is discriminated. </p>
                            <p>In this case, a special case of Brennan’s index is proposed by Arikan & Aybek (2022) to identify items for selecting a limited number of high-achieving examinees. Inspired by Brennan’s formula, this index calculates the difference between the performance of the upper 10% and lower 90% of the group on the item. It's important to note that the proportion defining the upper group is adjustable based on user requirements and is not fixed at 10%.</p>
                            <p>The formula is expressed as:</p>
                            <p><code>B10−90 = pU10 − pL90</code></p>
                            <p>where <code>pU10</code> represents the proportion of students in the upper 10 percentile who answered the item correctly and <code>pL90</code> denotes the proportion of students in the lower 90 percentile who answered the item correctly.</p>"
            )
          )
        ),
        box(
          title = "Results",
          width = 7,
          solidHeader = TRUE,
          status = "success",
          textOutput("group_info"),
          dataTableOutput("results"),
          uiOutput("column_explanation")
        ),
        fluidRow(
          box(
            title = "A Special Case of Brennan’s Index Plot",
            width = 10,
            solidHeader = TRUE,
            status = "success",
            plotlyOutput("index_plot", height = "400px")
          )
        )
      )),
      
      tabItem(tabName = "brennan_index", 
        fluidRow(
        box(
          title = "Parameters",
          width = 5,
          solidHeader = TRUE,
          status = "success",
          numericInput(
            "cutoff_score",
            "Cut-off Score:",
            value = 10,
            min = 0,
            max = 100
          ),
          actionButton("calculate_brennan_1972", "Analyze"),
          textOutput("cutoff_warning"),
          helpText(
            HTML(
              "<b>Brennan's Index:</b><br>
              <p>This index is designed for criterion-referenced tests or absolute assessments where a specific cut-off score is used to make decisions.</p>
              <p>The index is calculated using the following formula:</p>
              <p><code>BI = p(A) - p(B)</code></p>
              <p>where <code>p(A)</code> is the proportion of examinees above the cut-off score who answered the item correctly, and <code>p(B)</code> is the proportion of examinees below the cut-off score who answered the item correctly.</p>"            )
          )
        ),
        box(
          title = "Results",
          width = 7,
          solidHeader = TRUE,
          status = "success",
          textOutput("brennan_group_info"),
          dataTableOutput("brennan_results"),
          uiOutput("brennan_column_explanation")
        )),
        fluidRow(
          box(
            title = "Brennan's Index Plot",
            width = 10,
            solidHeader = TRUE,
            status = "success",
            plotlyOutput("brennan_plot", height = "400px")
          )
        )
      )
      ,
      
      tabItem(tabName = "other_indexes", 
        fluidRow(
        box(
          title = "Discrimination Indexes",
          width = 5,
          solidHeader = TRUE,
          status = "success",
          actionButton(
            "calculate_discrimination_index",
            "Analyze"
          ),
          helpText(
            HTML(
              "<b>Discrimination Index:</b><br>
        <p>The discrimination index is a measure of how well a question discriminates between high and low scorers. It's calculated in the following steps:</p>
        <ol>
          <li>Rank all students based on their total test score.</li>
          <li>Identify the upper 27% and the lower 27% of students.</li>
          <li>For each item, calculate the proportion of students in the upper group who answered correctly (U) and the proportion in the lower group who answered correctly (L).</li>
          <li>The discrimination index is D = U - L.</li>
        </ol>

        <b>Item-Total Correlation (Rit):</b><br>
        <p>The item-total correlation is the correlation between a scored item and the total test score. Higher positive values indicate that the item is more consistent with the overall test. </p>
        
        <b>Item-Rest Correlation (Rir):</b><br>
        <p>The item-rest correlation is the correlation between a specific item and the total score of all other items in the scale, excluding the specific item. It helps to determine how well an item fits with the other items in the test.</p>"
            )
          )
        ),
          box(
            title = "Results",
            width = 7,
            solidHeader = TRUE,
            status = "success",
            dataTableOutput("discrimination_results"),
            uiOutput("column_explanations")
          )
        ),
        fluidRow(
          box(
            title = "Discrimination Indices Plot",
            width = 10,
            solidHeader = TRUE,
            status = "success",
            plotlyOutput("discrimination_plot", height = "400px")
          )
        )
        ),
      
      tabItem(tabName = "item_plots", fluidRow(
        box(
          title = "Parameters",
          width = 4,
          solidHeader = TRUE,
          status = "success",
          selectInput("item_select", "Select Item:", choices = NULL),
          selectInput(
            "num_groups",
            "Number of Groups:",
            choices = c(3, 5, 10, 20),
            selected = 5
          ),
        actionButton("plot_item", "Plot Item")
        ),
        box(
          title = "Item Response Curves",
          width = 8,
          solidHeader = TRUE,
          status = "success",
          plotlyOutput("item_response_curves")
        )
      ))
      ,
      
      tabItem(tabName = "information", fluidRow(column(12, helpText(
        HTML(
          " <b>How to cite:</b><br>
          Doğaç, A., Aybek, E. C., Arıkan, S., & Coşkun, S. (2024). <i> DiscrimHub </i> [R Shiny application]. <br><br>
          </ol>
          
          <b>Developers:</b><br>
          Aybüke Doğaç, <a>https://www.linkedin.com/in/aybukedogac/</a><br>
          Eren Can Aybek,<a>https://www.linkedin.com/in/ecaybek/ </a><br>
          Serkan Arıkan, <a>https://www.linkedin.com/in/serkan-arikan-a62a38a2/</a><br>
          Sinem Coşkun, <a>https://www.linkedin.com/in/sinem-co%C5%9Fkun-7632b5214/</a><br><br>
                  </ol>
                  
          <b>Please refer to the following publication for further information:</b><br>
              Arikan, S., & Aybek, E. C. (2022). A Special Case of Brennan's Index for Tests That Aim to Select a Limited Number of Students: A Monte Carlo Simulation Study. <i>Educational Measurement: Issues and Practice, 41</i>(4), 35-49.<br><br>
               </ol>
               
           <b>If you have any questions, concerns, or feedback about the Shiny app, please do not hesitate to get in touch:</b><br>
              <a>dogacaybuke@gmail.com</a><br>"
        )
      ))))
    )
  )
)


# Server
server <- function(input, output, session) {
  session_id <- session$token
  
  # FIRST PAGE
  # Data upload
  data <- reactive({
    if (input$sample_data) {
      return(sample_data())
    }
    
    req(input$data)
    file_extension <- tools::file_ext(input$data$name)
    if (file_extension %in% c("xls", "xlsx")) {
      df <- read_excel(input$data$datapath)
      df <- df %>% mutate(TotalScore = rowSums(select(
        ., starts_with(input$custom_pattern)
      )))
      return(df)
    } else {
      shiny::showNotification("Please upload a valid Excel file.", type = "error")
      return(NULL)
    }
  })
  
  sample_data <- reactive({
    df <- as.data.frame(replicate(10, sample(0:1, 100, replace = TRUE)))
    names(df) <- paste0("Item", 1:10)
    df$TotalScore <- rowSums(df)
    df
  })
  
  output$download_sample <- downloadHandler(
    filename = function() {
      "sample_data.xlsx"
    },
    content = function(file) {
      write_xlsx(sample_data(), file)
    }
  )
  
  observeEvent(input$analyze, {
    req(data())
    analyzed_data <- data() %>%
      mutate(TotalScore = rowSums(select(
        ., starts_with(input$custom_pattern)
      )))
    
    output$data_preview <- renderDT({
      datatable(head(analyzed_data, 10),
                options = list(scrollX = TRUE, scrollY = "300px"))
    })
    
    output$descriptive_stats <- render_gt({
      analyzed_data %>%
        dplyr::summarise(across(
          everything(),
          list(
            mean = ~ base::mean(., na.rm = TRUE),
            sd = ~sn::sd(., na.rm = TRUE),
            min = ~ base::min(., na.rm = TRUE),
            max = ~ base::max(., na.rm = TRUE),
            median = ~ stats::median(., na.rm = TRUE)
          )
        )) %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = c("Item", "Statistic"),
          names_pattern = "(.*)_(.*)"
        ) %>%
        tidyr::pivot_wider(names_from = Statistic, values_from = value) %>%
        gt() %>%
        gt::fmt_number(columns = c("mean", "sd", "min", "max", "median"),
                   decimals = 2) %>%
        gt::tab_options(table.font.size = 12,
                    table.background.color = "#ffffff") %>%
        gt::tab_header(title = "Descriptive Statistics for the Items and the Total Score") %>%
        gt::cols_label(
          Item = "Item",
          mean = "Mean",
          sd = "SD",
          min = "Min",
          max = "Max",
          median = "Median"
        )
    })
    
    output$total_score_frequency <- renderPlotly({
      req(analyzed_data)
      req("TotalScore" %in% names(analyzed_data))
      
      mean_score <- mean(analyzed_data$TotalScore, na.rm = TRUE)
      median_score <- median(analyzed_data$TotalScore, na.rm = TRUE)
      
      p <- ggplot(analyzed_data, aes(x = TotalScore)) +
        geom_histogram(
          aes(y = after_stat(count)),
          fill = "seagreen",
          color = "seagreen4",
          alpha = 0.7,
          binwidth = 1,
          center = 0.5
        ) +
        geom_vline(
          aes(xintercept = mean_score),
          color = "violetred2",
          linetype = "dashed",
          size = 1
        ) +
        geom_vline(
          aes(xintercept = median_score),
          color = "turquoise1",
          linetype = "dashed",
          size = 1
        ) +
        theme_minimal() +
        labs(title = "Frequency Distribution of Total Scores", 
             x = "Total Score", y = "Frequency") +
        scale_x_continuous(breaks = seq(
          min(analyzed_data$TotalScore),
          max(analyzed_data$TotalScore),
          by = 1
        ))
      
      ggplotly(p) 
    })
    
    output$data_summary <- renderText({
      num_items <- ncol(analyzed_data) - 1  
      num_students <- nrow(analyzed_data)
      paste("Number of items:",
            num_items,
            "\nNumber of students:",
            num_students)
    })
  })
  
  # SECOND PAGE
  # A Special Case of Brennan's Index
  item_results <- reactiveVal()
  
  observeEvent(input$analyze_brennan, {
    req(data())
    
    num_students <- nrow(data())
    upper_cutoff <- round((input$upper_percentage / 100) * num_students)
    
    sorted_data <- data() %>%
      arrange(desc(TotalScore))
    
    upper_group <- sorted_data[1:upper_cutoff, ]
    lower_group <- sorted_data[(upper_cutoff + 1):num_students, ]
    
    item_results_df <- data.frame(
      Item = character(),
      upperGroupCorrect = numeric(),
      lowerGroupCorrect = numeric(),
      Pupper = numeric(),
      Plower = numeric(),
      Bul = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:ncol(data())) {
      col_name <- colnames(data())[i]
      if (col_name != "TotalScore") {
        upper_group_correct <- sum(upper_group[[col_name]] == 1, na.rm = TRUE)
        lower_group_correct <- sum(lower_group[[col_name]] == 1, na.rm = TRUE)
        
        pupper <- upper_group_correct / upper_cutoff
        plower <- lower_group_correct / (num_students - upper_cutoff)
        
        item_results_df <- rbind(item_results_df, data.frame(
          Item = col_name,
          upperGroupCorrect = upper_group_correct,
          lowerGroupCorrect = lower_group_correct,
          Pupper = round(pupper, 2),
          Plower = round(plower, 2),
          Bul = round(pupper - plower, 2)
        ))
      }
    }
    
    item_results_df <- item_results_df %>%
      mutate(
        DiscriminationClass = case_when(
          Bul >= 0.40 ~ "Excellent",
          Bul >= 0.30 ~ "Good",
          TRUE ~ "Poor"
        )
      )
    
    item_results_df$Item <- factor(item_results_df$Item, 
                                   levels = sort(unique(item_results_df$Item), 
                                                 method = "radix"))
    item_results(item_results_df)
    
    output$group_info <- renderText({
      paste(
        "Upper group students:",
        upper_cutoff,
        "\nLower group students:",
        num_students - upper_cutoff
      )
    })
    
    output$results <- renderDataTable({
      item_results() %>%
        select(
          Item,
          ugc = upperGroupCorrect,
          lgc = lowerGroupCorrect,
          Pu = Pupper,
          Pl = Plower,
          Bul = Bul,
          DC = DiscriminationClass
        )
    })
    
    output$column_explanation <- renderUI({
      HTML("
    <b>Column Explanations:</b><br>
    ugc: Upper Group Correct - Number of correct answers in the upper group<br>
    lgc: Lower Group Correct - Number of correct answers in the lower group<br>
    Pu: Proportion of correct answers in the upper group<br>
    Pl: Proportion of correct answers in the lower group<br>
    Bul: A Special Case of Brennan's Index<br>
    DC: Discrimination Class based on Bul value
    ")
    })
    
    output$index_plot <- renderPlotly({
      req(item_results())
      item_results_df <- item_results() %>% filter(Item != "TotalScore")
      
      p <- ggplot(item_results_df, aes(x = Item, y = Bul, color = Bul)) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(hjust = 1, vjust = 1)
        ) +
        labs(title = "A Special Case of Brennan's Index per Item", 
             x = "Items", y = "Bul") +
        scale_y_continuous(limits = c(0, max(item_results_df$Bul, na.rm = TRUE) * 1.1),
                           breaks = seq(0, 1, by = 0.1)) +
        scale_color_gradientn(
          colors = c("tomato", "yellow", "darkgreen"),
          values = scales::rescale(c(0, 0.2, 0.4))
        ) +
        geom_hline(yintercept = 0.40, linetype = "dashed", color = "darkgreen") +
        geom_hline(yintercept = 0.30, linetype = "dashed", color = "tomato")
      
      ggplotly(p)
    })
  })
  
  # THIRD PAGE
  # Brennan's Index (1972)
  observeEvent(input$calculate_brennan_1972, {
    req(data())
    
    cutoff_score <- input$cutoff_score
    max_score <- max(data()$TotalScore, na.rm = TRUE)
    
    if (cutoff_score > max_score) {
      output$cutoff_warning <- renderText({
        paste("Warning: Cutoff score is higher than the maximum total score:",
              max_score)
      })
      return()
    }
    
    output$cutoff_warning <- renderText({
      ""
    })
    
    brennan_results <- data.frame(
      Item = character(),
      AboveCutoffCorrect = integer(),
      BelowCutoffCorrect = integer(),
      BrennanIndex = numeric(),
      stringsAsFactors = FALSE
    )
    
    above_cutoff <- data()$TotalScore >= cutoff_score
    above_cutoff_total <- sum(above_cutoff)
    below_cutoff_total <- sum(!above_cutoff)
    
    for (i in 1:ncol(data())) {
      col_name <- colnames(data())[i]
      if (col_name != "TotalScore") {
        item_data <- as.numeric(unlist(data()[, col_name]))
        
        above_cutoff_correct <- sum(item_data[above_cutoff], na.rm = TRUE)
        below_cutoff_correct <- sum(item_data[!above_cutoff], na.rm = TRUE)
        
        p_A <- above_cutoff_correct / above_cutoff_total
        p_B <- below_cutoff_correct / below_cutoff_total
        
        brennan_results <- rbind(
          brennan_results,
          data.frame(
            Item = col_name,
            AboveCutoffCorrect = above_cutoff_correct,
            BelowCutoffCorrect = below_cutoff_correct,
            BrennanIndex = round(p_A - p_B, 2)
          )
        )
      }
    }
    
    brennan_results <- brennan_results %>%
      mutate(
        DiscriminationClass = case_when(
          BrennanIndex >= 0.40 ~ "Excellent",
          BrennanIndex >= 0.30 ~ "Good",
          TRUE ~ "Poor"
        )
      )
    
    output$brennan_group_info <- renderText({
      paste(
        "Above Cutoff Total:",
        above_cutoff_total,
        "\nBelow Cutoff Total:",
        below_cutoff_total
      )
    })
    
    output$brennan_results <- renderDataTable({
      brennan_results %>%
        select(
          Item,
          AC = AboveCutoffCorrect,
          BC = BelowCutoffCorrect,
          BI = BrennanIndex,
          DC = DiscriminationClass
        )
    })
    
    output$brennan_column_explanation <- renderUI({
      HTML("
    <b>Column Explanations:</b><br>
    AC: Above Cutoff Correct - Number of correct answers above the cutoff score<br>
    BC: Below Cutoff Correct - Number of correct answers below the cutoff score<br>
    BI: Brennan's Index<br>
    DC: Discrimination Class based on Brennan's Index value
    ")
    })
    
    output$brennan_plot <- renderPlotly({
      p <- ggplot(brennan_results,
                  aes(x = Item, y = BrennanIndex, color = BrennanIndex)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "Brennan's Index per Item", x = "Item", y = "Brennan's Index") +
        theme(axis.text.x = element_text(hjust = 1)) +
        scale_y_continuous(limits = c(0, max(brennan_results$BrennanIndex, na.rm = TRUE) * 1.1)) +
        scale_color_gradientn(
          colors = c("tomato", "yellow", "darkgreen"),
          values = scales::rescale(c(0, 0.3, 0.4))
        ) +
        geom_hline(yintercept = 0.40, linetype = "dashed", color = "darkgreen") +
        geom_hline(yintercept = 0.30, linetype = "dashed", color = "tomato")
      
      ggplotly(p)
    })
  })
  
  
  # FOURTH PAGE
  # Other Discrimination Indexes
  observeEvent(input$calculate_discrimination_index, {
    req(data())
    
    num_students <- nrow(data())
    upper_percentile <- round(num_students * 0.27)
    lower_percentile <- round(num_students * 0.27)
    
    sorted_data <- data() %>%
      arrange(desc(TotalScore))
    
    upper_group <- sorted_data[1:upper_percentile, ]
    lower_group <- sorted_data[(num_students - lower_percentile + 1):num_students, ]
    
    discrimination_results <- data.frame(
      Item = colnames(data()),
      Bul = numeric(ncol(data())),
      DI = numeric(ncol(data())),
      Rit = numeric(ncol(data())),
      Rir = numeric(ncol(data()))
    )
    
    for (i in 1:ncol(data())) {
      if (colnames(data())[i] != "TotalScore") {
        p_upper <- sum(upper_group[, i] == 1) / upper_percentile
        p_lower <- sum(lower_group[, i] == 1) / lower_percentile
        
        discrimination_index <- p_upper - p_lower
        
        upper_cutoff_brennan <- round((input$upper_percentage / 100) * num_students)
        upper_group_brennan <- sorted_data[1:upper_cutoff_brennan, ]
        lower_group_brennan <- sorted_data[(upper_cutoff_brennan + 1):num_students, ]
        
        upper_group_correct_brennan <- sum(upper_group_brennan[, i] == 1)
        lower_group_correct_brennan <- sum(lower_group_brennan[, i] == 1)
        
        pupper_brennan <- upper_group_correct_brennan / upper_cutoff_brennan
        plower_brennan <- lower_group_correct_brennan / (num_students - upper_cutoff_brennan)
        brennan_index <- pupper_brennan - plower_brennan
        
        # Item-Total Correlation
        item_total_correlation <- cor(data()[, i], data()$TotalScore, use = "pairwise.complete.obs")
        
        # Item-Rest Correlation
        rest_score <- data()$TotalScore - data()[, i]
        item_rest_correlation <- cor(data()[, i], rest_score, use = "pairwise.complete.obs")
        
        discrimination_results[i, "Bul"] <- round(brennan_index, 2)
        discrimination_results[i, "DI"] <- round(discrimination_index, 2)
        discrimination_results[i, "Rit"] <- round(item_total_correlation, 2)
        discrimination_results[i, "Rir"] <- round(item_rest_correlation, 2)
      }
    }
    
    discrimination_results <- discrimination_results %>% filter(Item != "TotalScore")
    
    output$discrimination_results <- renderDataTable({
      discrimination_results
    })
    
    output$column_explanations <- renderUI({
      HTML(
        "<b>Column Explanations:</b><br>
        Bul: A Special Case of Brennan's Index<br>
        DI: Discrimination Index<br>
        Rit: Item-Total Correlation<br>
        Rir: Item-Rest Correlation"
      )
    })
    
    output$discrimination_plot <- renderPlotly({
      plot_data <- discrimination_results %>%
        pivot_longer(cols = c(Bul, DI, Rit, Rir),
                     names_to = "Index", values_to = "Value")
      
      p <- ggplot(plot_data, aes(x = Item, y = Value, color = Index, shape = Index)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "Discrimination Indices per Item", x = "Item", y = "Index Value") +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.2)) +
        scale_color_manual(values = c("salmon", "deepskyblue", "palegreen3", "plum1")) +
        scale_shape_manual(values = c(16, 17, 18, 15)) +
        geom_hline(yintercept = 0.30, linetype = "dashed", color = "tomato") +
        geom_hline(yintercept = 0.40, linetype = "dashed", color = "seagreen")
      
      ggplotly(p)
    })
  })
  
  
  # FIFTH PAGE
  # Item Response Curves
  observe({
    req(data())
    item_choices <- setdiff(names(data()), "TotalScore")
    updateSelectInput(session, "item_select", choices = item_choices)
  })
  
  observeEvent(input$plot_item, {
    req(data(), input$item_select, input$num_groups)
    
    data_with_total_score <- data()
    
    sorted_data <- data_with_total_score %>%
      arrange(desc(TotalScore))
    
    groups <- cut(sorted_data$TotalScore,
                  breaks = as.numeric(input$num_groups),
                  labels = FALSE)
    
    item_specific_data <- sorted_data %>%
      mutate(Group = as.numeric(groups))
    
    selected_item <- input$item_select
    
    if (!is.numeric(item_specific_data[[selected_item]])) {
      shiny::showNotification("Selected item contains non-numeric data. Converting to numeric.", type = "warning")
      item_specific_data[[selected_item]] <- as.numeric(as.factor(item_specific_data[[selected_item]]))
    }
    
    item_specific_results <- item_specific_data %>%
      group_by(Group) %>%
      summarize(mean_item = mean(!!sym(selected_item), 
                                 na.rm = TRUE))
    
    output$item_response_curves <- renderPlotly({
      p <- ggplot(item_specific_results,
                  aes(x = as.numeric(Group), y = mean_item, group = 1)) +
        geom_smooth(method = "loess",
                    se = FALSE) +
        geom_point(size = 2) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        scale_x_continuous(breaks = 1:max(item_specific_results$Group)) +
        coord_cartesian(ylim = c(0, 1)) +
        theme_minimal() +
        labs(
          title = paste(selected_item, "Correct Answer Percentages by Group"),
          x = "Group",
          y = "Correct Answer Percentage"
        )
      
      ggplotly(p)
    })
  })
  
  observe({
    if (is.null(input$data) && !input$sample_data) {
      shiny::showNotification(
        "Please upload data on the first page or use sample data before performing calculations on other pages.",
        type = "warning"
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)
