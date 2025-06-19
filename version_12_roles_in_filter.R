# =================================================================
# 1. Load Libraries
# =================================================================
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(shinythemes)
library(tidyr)
library(stringr)
library(forcats)
library(ggrepel)
library(scales)

# =================================================================
# 2. Load and Prepare Data
# =================================================================
# Load base survey data and client identifier data
survey_data <- read_csv("Riskwise_data/cleaned_survey_data.csv", show_col_types = FALSE)
survey_client_id <- read_csv("Riskwise_data/Client_ID_A.csv", show_col_types = FALSE)

# Add the 'role' Column based on client responses
client_data_with_roles <- survey_client_id %>%
  mutate(
    role = case_when(
      (`Are you a Grower (Land Owner)?` == "Yes" & !is.na(`Are you a Grower (Land Owner)?`)) |
        (`Are you a Grower (Non Owner)?` == "Yes" & !is.na(`Are you a Grower (Non Owner)?`))   ~ "Grower",
      `Are you an Advisor / Consultant?` == "Yes" & !is.na(`Are you an Advisor / Consultant?`) ~ "Advisor",
      TRUE ~ "Other"
    )
  )

# Select and prepare the role data for joining
roles_to_join <- client_data_with_roles %>%
  select(client_id = `RiskWi$e ID`, role)

# Join the role information to the main survey dataset
survey_data <- survey_data %>%
  left_join(roles_to_join, by = "client_id")

#check 
rows_after_join <- nrow(survey_data)
message(paste("Number of rows after joining with roles data:", rows_after_join))

# Keep only the first occurrence of each unique client_id
survey_data <- survey_data %>%
  distinct(client_id, .keep_all = TRUE)

nrow(survey_data)  # after removing duplicates

# --- A: Prepare "Decision Ranking" Data ---
decision_ranking_long <- survey_data %>%
  select(starts_with("how_difficult"),
         region = which_region_are_your_farming_activities_based,
         grower_group = which_group_are_you_associated_with,
         role) %>% # Propagate role
  pivot_longer(cols = starts_with("how_difficult"), names_to = "decision_name", values_to = "difficulty_level") %>%
  mutate(
    decision_name = str_to_title(str_replace_all(str_remove(decision_name, "how_difficult_do_you_find_making_decisions_about_"), "_", " ")),
    decision_name = str_replace(decision_name, "Etc", "(etc)"),
    decision_name = str_replace(decision_name, "Type Mix Fallowing", "(Type / Mix / Fallowing etc)"),
    decision_name = str_replace(decision_name, "Marketing Grain Sales And Contracts", "Marketing Strategies (Grain sales and contracts)")
  ) %>%
  filter(!is.na(difficulty_level), difficulty_level != "Not Relevant") %>%
  mutate(difficulty_level = factor(difficulty_level, levels = c("Very Difficult", "Somewhat Difficult", "Not Difficult")))

# --- B: Prepare "Risk Attitude" and other data ---
risk_attitude_df <- survey_data %>%
  select(
    client_id,
    what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    what_is_your_risk_attitude_when_making,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision,
    role # Propagate role
  ) %>%
  rename(
    intuition_vs_data      = what_balance_guides_you_between_intuition_gut_feel_informed_by_past_experience_and_numerical_calculation_data_driven_when_making_a,
    advisor_balance        = what_balance_do_you_believe_an_advisor_consultant_is_guided_by_when_advising_on,
    production_vs_profit = what_balance_do_you_take_between_production_or_profitability_expectations_when_making,
    comfort_with_process = how_comfortable_are_you_with_your_decision_process_not_outcome_when_making_current,
    risk_attitude          = what_is_your_risk_attitude_when_making
  ) %>%
  mutate(across(c(intuition_vs_data, advisor_balance, production_vs_profit, comfort_with_process, risk_attitude), ~replace_na(., 0)))


# --- Count and Replace NA values in risk_attitude_df ---
numeric_score_columns <- c(
  "intuition_vs_data", "advisor_balance", "production_vs_profit",
  "comfort_with_process", "risk_attitude"
)
risk_attitude_df <- risk_attitude_df %>%
  mutate(across(all_of(numeric_score_columns), ~replace_na(., 0)))


# --- C. Prepare "Review Process" Data ---
review_process_df <- survey_data %>%
  select(
    client_id,
    review_type = have_you_undertaken_any_review_of_how_you_made_your_last,
    region = which_region_are_your_farming_activities_based,
    grower_group = which_group_are_you_associated_with,
    risk_decision = selected_risk_decision,
    role # Propagate role
  ) %>%
  filter(!is.na(review_type))

# --- D. Prepare Filter Choices and Global Variables ---
all_regions <- c("All Regions", sort(unique(na.omit(survey_data$which_region_are_your_farming_activities_based))))
all_groups <- c("All Groups", sort(unique(na.omit(survey_data$which_group_are_you_associated_with))))
all_roles <- c("All Roles", "Grower", "Advisor", "Other") # NEW: Choices for the role filter
decision_choices <- c("All Decisions", sort(unique(na.omit(survey_data$selected_risk_decision))))
client_choices <- sort(unique(na.omit(risk_attitude_df$client_id)))

plot_vars <- c("Risk Attitude" = "risk_attitude", "Decision Ranking" = "decision_ranking",
               "Review Process" = "review_process",
               "Intuition vs. Calculation" = "intuition_vs_data", "Comfort with Process" = "comfort_with_process",
               "Production vs. Profit" = "production_vs_profit", "Advisor Balance" = "advisor_balance")

# --- E. Create Region-to-Group Mapping ---
region_group_map <- list(
  "WA" = c("Corrigin Farm Improvement Group", "Facey Group", "Mingenew Irwin Group (MIG)", "South-East Premium Wheat Growers Association (SEPWA)", "Stirlings to Coast Farmers (SCF)", "The Liebe Group", "Western Australian No-Tillage Farmers Association (WANTFA)", "West Midlands Group (WMG)"),
  "NSW" = c("AgGrow Agronomy", "Agricultural Marketing and Production Systems (AMPS)", "Central West Farming Systems (CWFS)", "FarmLink", "Grain Orana Alliance (GOA)", "Holbrook Landcare", "Irrigation Farmers Network", "Irrigation Research and Extension Committee (IREC)", "Riverine Plains", "Southern Growers"),
  "QLD" = c("Capella", "Gindie", "Roma", "St George", "Goondiwindi", "Meandarra", "Brigalow"),
  "VIC and SA Mallee [BCG Lead]" = c("Coomandook Agriculture Bureau", "Field Applied Research (FAR Australia)", "Mallee Sustainable Farming (MSF)", "Birchup Cropping Group (BCG)"),
  "SA Eyre - [AIR EP Lead]" = c("Buckleboo Farm Improvement Group (BFIG)", "EPAG Research Trust", "Air EP"),
  "SA Central [Hart Lead]" = c("Mid North High Rainfall Zone (MNHRZ)", "Murray Plains Farmers (MPF)", "Northern Sustainable Soils (NSS)", "Upper North Farming Systems (UNFS)", "HART")
)

# =================================================================
# 3. Define the User Interface (UI)
# =================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Farm Decision-Making Dashboard"),
  
  fluidRow(
    column(3,
           wellPanel(
             selectInput("variable_selector", "Select Metric to Display:",
                         choices = names(plot_vars), selected = "Risk Attitude")
           )
    ),
    column(3,
           wellPanel(
             conditionalPanel(
               condition = "input.variable_selector != 'Decision Ranking'",
               selectInput("compare_mode", "View Mode:",
                           choices = c("Single Population" = "single",
                                       "Compare two populations" = "compare_two",
                                       "Compare me to my peers" = "compare_me"),
                           selected = "single")
             )
           )
    ),
    column(3,
           wellPanel(
             uiOutput("pop1_filter_header"),
             # NEW: Added Role Filter for Population 1
             selectInput("role_filter1", "Role:", choices = all_roles),
             selectInput("region_filter1", "Region:", choices = all_regions),
             selectInput("group_filter1", "Group:", choices = all_groups),
             conditionalPanel(
               condition = "!['Decision Ranking', 'Review Process'].includes(input.variable_selector)",
               selectInput("decision_filter1", "Specific Decision:", choices = decision_choices)
             )
           )
    ),
    column(3,
           conditionalPanel(
             condition = "input.variable_selector != 'Decision Ranking'",
             conditionalPanel(
               condition = "input.compare_mode == 'compare_me'",
               h4("My Response Selector", align = "center"),
               wellPanel(
                 selectInput("client_selector", "Select Your ID:", choices = client_choices),
                 hr(),
                 uiOutput("user_response_text_ui")
               )
             ),
             conditionalPanel(
               condition = "input.compare_mode == 'compare_two'",
               uiOutput("pop2_filter_header"),
               wellPanel(
                 # NEW: Added Role Filter for Population 2
                 selectInput("role_filter2", "Role:", choices = all_roles),
                 selectInput("region_filter2", "Region:", choices = all_regions, selected = all_regions[1]),
                 selectInput("group_filter2", "Group:", choices = all_groups),
                 conditionalPanel(
                   condition = "input.variable_selector != 'Review Process'",
                   selectInput("decision_filter2", "Specific Decision:", choices = decision_choices)
                 )
               )
             )
           )
    )
  ),
  hr(),
  fluidRow(
    column(12,
           uiOutput("summary_output_ui"),
           plotOutput("main_plot", height = "500px")
    )
  )
)


# =================================================================
# 4. Define the Server Logic
# =================================================================
server <- function(input, output, session) {
  
  difficulty_colors <- c("Not Difficult" = "#2ca02c", "Somewhat Difficult" = "#ffbb78", "Very Difficult" = "#d62728")
  
  observeEvent(input$region_filter1, {
    selected_region <- input$region_filter1
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter1", choices = choices)
  })
  
  observeEvent(input$region_filter2, {
    selected_region <- input$region_filter2
    if (selected_region == "All Regions") {
      choices <- all_groups
    } else {
      choices <- c("All Groups", region_group_map[[selected_region]])
    }
    updateSelectInput(session, "group_filter2", choices = choices)
  })
  
  observeEvent(input$variable_selector, {
    if (input$variable_selector == "Decision Ranking") {
      updateSelectInput(session, "compare_mode", selected = "single")
    }
  }, ignoreInit = TRUE)
  
  # UPDATED: The filtering function now includes logic for the role filter
  filtered_data <- function(pop_num = 1) {
    # Get filters for the selected population
    role_filter     <- if(pop_num == 1) input$role_filter1 else input$role_filter2
    region_filter   <- if(pop_num == 1) input$region_filter1 else input$region_filter2
    group_filter    <- if(pop_num == 1) input$group_filter1 else input$group_filter2
    decision_filter <- if(pop_num == 1) input$decision_filter1 else input$decision_filter2
    
    # Select the base dataset
    data <- switch(input$variable_selector, "Decision Ranking" = decision_ranking_long, "Review Process" = review_process_df, risk_attitude_df)
    
    # Apply filters sequentially, checking if they are not null and not "All"
    if (!is.null(role_filter) && role_filter != "All Roles") {
      data <- data %>% filter(role == role_filter)
    }
    if (!is.null(region_filter) && region_filter != "All Regions") {
      data <- data %>% filter(region == region_filter)
    }
    if (!is.null(group_filter) && group_filter != "All Groups") {
      data <- data %>% filter(grower_group == group_filter)
    }
    if (!is.null(decision_filter) && !input$variable_selector %in% c("Decision Ranking", "Review Process") && decision_filter != "All Decisions") {
      data <- data %>% filter(risk_decision == decision_filter)
    }
    
    return(data)
  }
  
  selected_user_data <- reactive({
    req(input$compare_mode == "compare_me", input$client_selector)
    df <- if (input$variable_selector == "Review Process") review_process_df else risk_attitude_df
    df %>% filter(client_id == input$client_selector)
  })
  
  plot_population_sizes <- reactive({
    get_size <- function(data_df, metric_name) {
      # For decision ranking, count is number of responses (rows)
      if (metric_name == "Decision Ranking") {
        return(if (is.null(data_df)) 0 else nrow(data_df))
      }
      # For others, it's the number of unique clients
      return(if (is.null(data_df) || nrow(data_df) == 0) 0 else n_distinct(data_df$client_id))
    }
    
    pop1_data <- filtered_data(1)
    pop1_size <- get_size(pop1_data, input$variable_selector)
    
    pop2_size <- NULL
    if (input$compare_mode == "compare_two") {
      pop2_data <- filtered_data(2)
      pop2_size <- get_size(pop2_data, input$variable_selector)
    }
    list(pop1_size = pop1_size, pop2_size = pop2_size)
  })
  
  output$pop1_filter_header <- renderUI({
    h4(paste0("Population 1 / Peers Filters (n=", plot_population_sizes()$pop1_size, ")"), align="center")
  })
  
  output$pop2_filter_header <- renderUI({
    h4(paste0("Population 2 Filters (n=", plot_population_sizes()$pop2_size, ")"), align="center")
  })
  
  # ... rest of server logic is unchanged ...
  
  output$user_response_text_ui <- renderUI({
    req(input$compare_mode == 'compare_me')
    user_data <- selected_user_data()
    response_text <- if (input$variable_selector == "Review Process") {
      user_data$review_type[1]
    } else if (input$variable_selector != "Decision Ranking") {
      user_data[[plot_vars[input$variable_selector]]][1]
    } else {
      return(NULL)
    }
    if (is.na(response_text)) return(p("No response recorded."))
    div(p(strong("Your ID:"), br(), input$client_selector), p(strong("My Response:"), br(), paste0("'", response_text, "'")))
  })
  
  output$summary_output_ui <- renderUI({
    req(input$variable_selector == "Risk Attitude")
    if (input$compare_mode == "compare_two") tableOutput("comparison_summary_table")
    else if (input$compare_mode == "single") uiOutput("single_summary_bar")
  })
  
  output$single_summary_bar <- renderUI({
    data <- filtered_data(1); risk_data <- data %>% filter(!is.na(risk_attitude)); if(nrow(risk_data) == 0) return(p("No data for this filter."))
    summary_stats <- risk_data %>% mutate(category = case_when(risk_attitude <= 1 ~ "Very Risk Avoidant", risk_attitude <= 4 ~ "Risk Avoidant", risk_attitude == 5 ~ "Neutral", risk_attitude <= 8 ~ "Risk Tolerant", TRUE ~ "Very Risk Tolerant")) %>% count(category) %>% mutate(percentage = round(n / sum(n) * 100))
    category_order <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant"); summary_cols <- lapply(category_order, function(cat) {percent_val <- summary_stats$percentage[summary_stats$category == cat]; if (length(percent_val) == 0) percent_val <- 0; column(2, style="border:1px solid #ddd; text-align:center; padding:5px; margin:0 5px;", tags$b(cat), tags$p(paste0(percent_val, "%")))}); fluidRow(column(1), summary_cols)
  })
  
  output$comparison_summary_table <- renderTable({
    category_order_vector <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
    get_population_percentages <- function(df, pop_label) {
      category_order_vector <- c("Very Risk Avoidant", "Risk Avoidant", "Neutral", "Risk Tolerant", "Very Risk Tolerant")
      
      # Create a zero-filled template. This is the base for our output.
      template <- tibble(
        Category = factor(category_order_vector, levels = category_order_vector),
        n = 0
      )
      
      df_filtered <- df %>% filter(!is.na(risk_attitude))
      
      # If data exists, calculate actual counts
      if (nrow(df_filtered) > 0) {
        summary_counts <- df_filtered %>%
          mutate(Category = case_when(
            risk_attitude <= 1 ~ "Very Risk Avoidant",
            risk_attitude <= 4 ~ "Risk Avoidant",
            risk_attitude == 5 ~ "Neutral",
            risk_attitude <= 8 ~ "Risk Tolerant",
            TRUE ~ "Very Risk Tolerant"
          )) %>%
          count(Category)
        
        # Update the template with the real counts
        summary_df <- rows_update(template, summary_counts, by = "Category")
        total_n <- sum(summary_df$n)
      } else {
        # If no data, use the zero-filled template
        summary_df <- template
        total_n <- 0
      }
      
      # Calculate percentages and format for display
      summary_df %>%
        mutate(
          percentage = if (total_n > 0) n / total_n else 0,
          display_percentage = paste0(round(percentage * 100), "%")
        ) %>%
        select(Category, display_percentage) %>%
        pivot_wider(names_from = Category, values_from = display_percentage) %>%
        mutate(Population = pop_label) %>%
        select(Population, all_of(category_order_vector))
    }
    bind_rows(get_population_percentages(filtered_data(1), "Population 1"),
              get_population_percentages(filtered_data(2), "Population 2"))
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  output$main_plot <- renderPlot({
    if (input$variable_selector == "Review Process") {
      if (input$compare_mode == "single" || input$compare_mode == "compare_me") {
        plot_data <- filtered_data(1) %>% count(review_type) %>% arrange(desc(review_type)) %>%
          mutate(percentage = n / sum(n), ypos = cumsum(percentage) - 0.5 * percentage, label_text = scales::percent(percentage, accuracy = 1))
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data for selected filters.", size=6) + theme_void())
        ggplot(plot_data, aes(x = 2, y = percentage, fill = review_type)) +
          geom_col(width = 1, color = "white") + coord_polar(theta = "y", start = 0) +
          geom_label_repel(aes(y = ypos, label = label_text), size = 6, show.legend = FALSE, nudge_x = 1, segment.color = "black") +
          scale_fill_brewer(palette = "Pastel1", name = "Review Process", labels = function(x) str_wrap(x, width = 25)) +
          xlim(0.5, 3.5) + theme_void() + theme(legend.title=element_text(face="bold", size=14), legend.text = element_text(size=12))
      } else if (input$compare_mode == "compare_two") {
        data1 <- filtered_data(1) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = "Population 1")
        data2 <- filtered_data(2) %>% count(review_type) %>% mutate(percentage = n/sum(n), population = "Population 2")
        plot_data <- bind_rows(data1, data2)
        if(nrow(plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No data.", size=6) + theme_void())
        ggplot(plot_data, aes(x=review_type, y=percentage, fill=population)) +
          geom_col(position = "dodge") + scale_y_continuous(labels = scales::percent) +
          scale_x_discrete(labels = function(x) str_wrap(x, width=15)) +
          labs(title = "Comparison of Review Processes", x = "Review Type", y = "Percentage of Respondents") +
          theme_minimal(base_size = 14) + theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"))
      }
    } else {
      selected_var_name <- plot_vars[input$variable_selector]
      all_scores_template <- tibble(value = 0:10)
      if (input$compare_mode == "compare_two") {
        process_plot_data <- function(df, pop_name, var) {
          df_filtered <- df %>% filter(!is.na(.data[[var]]))
          total_n <- nrow(df_filtered)
          if (total_n == 0) {
            return(all_scores_template %>% mutate(percentage = 0, population = pop_name))
          }
          df_filtered %>%
            count(.data[[var]], name = "n") %>%
            rename(value = 1) %>%
            right_join(all_scores_template, by = "value") %>%
            mutate(n = replace_na(n, 0), percentage = n / total_n, population = pop_name)
        }
        combined_data <- bind_rows(process_plot_data(filtered_data(1), "Population 1", selected_var_name),
                                   process_plot_data(filtered_data(2), "Population 2", selected_var_name))
        if(nrow(combined_data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
        ggplot(combined_data, aes(x=value, y=percentage, color=population)) +
          geom_line(size=1.2) + geom_point(size=4) +
          scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2), expand = c(0,0)) +
          scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, .15))) +
          labs(title=paste("Comparison of", input$variable_selector), x="Score (0-10)", y="Percentage of Respondents", color="Population") +
          theme_minimal(base_size=15) +
          theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
      } else if (input$compare_mode == "compare_me") {
        req(input$variable_selector != "Decision Ranking")
        peers_data <- filtered_data(1)
        user_data <- selected_user_data()
        user_value <- user_data[[selected_var_name]]
        peers_filtered <- peers_data %>% filter(!is.na(.data[[selected_var_name]]))
        total_n_peers <- nrow(peers_filtered)
        peers_plot_data <- if (total_n_peers == 0) {
          all_scores_template %>% mutate(percentage = 0)
        } else {
          peers_filtered %>%
            count(.data[[selected_var_name]], name = "count") %>% rename(value = 1) %>%
            right_join(all_scores_template, by = "value") %>%
            mutate(count = replace_na(count, 0), percentage = count / total_n_peers)
        }
        if (nrow(peers_plot_data) == 0) return(ggplot() + annotate("text", 0.5, 0.5, label="No peer data.", size=6) + theme_void())
        p <- ggplot(peers_plot_data, aes(x = value, y = percentage)) +
          geom_line(color = "#006699", size = 1.2) + geom_point(color = "#006699", size = 4) +
          scale_x_continuous(limits = c(-0.5, 10.5), breaks = seq(0, 10, 2)) +
          scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.03, .15))) +
          labs(title = paste("Your Response vs. Peer Distribution for", input$variable_selector), x = "Score (0-10)", y = "Percentage of Respondents") +
          theme_minimal(base_size = 15) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        if (length(user_value) > 0 && !is.na(user_value)) {
          band_width <- 0.5
          p <- p +
            geom_rect(aes(xmin = user_value - band_width, xmax = user_value + band_width, ymin = 0, ymax = Inf), fill = "#006699", alpha = 0.8) +
            geom_text(aes(x = user_value, y = (max(peers_plot_data$percentage, 0.1) * 0.5)), label = "My Response", color = "white", fontface = "bold", size = 5, angle = 90)
        }
        p
      } else {
        data <- filtered_data(1)
        if (input$variable_selector == "Decision Ranking") {
          if(nrow(data)==0) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No ranking data.",size=6)+theme_void())
          summary_ranking <- data %>%
            count(decision_name, difficulty_level) %>% group_by(decision_name) %>% mutate(percentage = n / sum(n)) %>%
            ungroup() %>% group_by(decision_name) %>% mutate(sort_metric = sum(percentage[difficulty_level == "Very Difficult"])) %>%
            ungroup() %>% mutate(decision_name = fct_reorder(decision_name, sort_metric))
          ggplot(summary_ranking, aes(y = decision_name, x = percentage, fill = difficulty_level)) +
            geom_col() + scale_x_continuous(labels=scales::percent, expand=c(0,0)) +
            scale_fill_manual(values=difficulty_colors, name="Difficulty Level") +
            labs(title="Decision Ranking by Assessed Difficulty", x="Percentage of Respondents", y="Decision Type") +
            theme_minimal(base_size=14) +
            theme(legend.position="bottom", plot.title=element_text(hjust=0.5, face="bold"), axis.text.y=element_text(size=12,face="bold"))
        } else {
          if(nrow(data)==0 || all(is.na(data[[selected_var_name]]))) return(ggplot()+annotate("text",x=0.5,y=0.5,label="No data.",size=6)+theme_void())
          data_filtered <- data %>% filter(!is.na(.data[[selected_var_name]]))
          total_n <- nrow(data_filtered)
          plot_data <- if(total_n == 0) {
            all_scores_template %>% mutate(percentage = 0)
          } else {
            data_filtered %>%
              count(.data[[selected_var_name]], name="count") %>% rename(value=1) %>%
              right_join(all_scores_template, by = "value") %>%
              mutate(count = replace_na(count, 0), percentage = count / total_n)
          }
          ggplot(plot_data, aes(x=value, y=percentage)) +
            geom_line(color="#006699",size=1.2) + geom_point(color="#006699",size=4) +
            scale_x_continuous(limits=c(0,10), breaks=seq(0,10,2), expand = c(0,0)) +
            scale_y_continuous(labels=scales::percent, expand = expansion(mult = c(0.03, 0.15))) +
            labs(title=paste("Distribution of",input$variable_selector), x="Score (0-10)", y="Percentage of Respondents") +
            theme_minimal(base_size=15) + theme(plot.title=element_text(hjust=0.5,face="bold"))
        }
      }
    }
  })
}

# =================================================================
# 5. Run the Shiny App
# =================================================================
shinyApp(ui = ui, server = server)