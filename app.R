library(janitor)
library(shiny)
library(shinydashboard)
library(tidyverse)

my_studies <- read_csv('parkinsons_studies.csv') %>%
  clean_names() %>%
  select(-c(study_id))

# fyi
column_names <- colnames(my_studies) %>%
  as.data.frame()

# group studies by type of impulsivity as referred to in-text
final_studies <- my_studies %>%
  mutate(response_inhibition = ifelse(go_nogo == "1" | stop_signal == "1" | countermanding_task_reaching_sst == "1" |  
                                      conditional_stop_signal_task == "1" | stop_change_paradigm == "1" | hayling_test == "1",
                                      "yes",    # identify all response inhibition studies 
                                      "no"),
         response_conflict = ifelse(simon == "1" | stroop == "1" | manual_congruency_task == "1" | eriksen_flanker == "1" |  
                                    picture_word_interference_task_lexical_semantic_inhibition == "1", 
                                    "yes",
                                    "no"),
         oculomotor = ifelse(pro_anti_saccades == "1" | saccadic_visually_guided_memory_guided_etc == "1" | saccadic_smooth_pursuit == "1" |
                            saccadic_gng == "1" | saccadic_simon == "1" | saccadic_other == "1",
                             "yes",
                             "no"),
         delayed_gratification = ifelse(delay_discounting == "1",
                           "yes",
                           "no"),
         decision_making_ambiguity = ifelse(iowa_gambling == "1" | bart == "1" | deal_or_no_deal_ambiguous_decision_making == "1",
                           "yes",
                           "no"),
         decision_making_objective = ifelse(game_of_dice == "1" | cambridge_gambling_task == "1" | vancouver_gambling_task == "1" |
                                            monetary_decision_making == "1" | probability_associated_gambling_task_decision_making_under_risk == "1" |
                                            framing_paradigm == "1" | investment_task == "1",
                           "yes",
                           "no"),
         personality_traits = ifelse(barratt_imp_scale == "1" | temperament_and_character_inventory == "1" | upps == "1" |
                                     behavioural_inhibition_systems_behavioural_approach_systems == "1" | snaith_hamilton_pleasure_scale == "1" |
                                       tridimensional_personality_questionnaire == "1" | zuckerman_kuhlman_personality_questionnaire == "1",
                                 "yes",
                                 "no"),
         set_switching = ifelse(trail_making_test == "1" | odd_man_out == "1" | task_switching == "1" | rewarded_switching_task == "1",
                           "yes",
                           "no"),
         priming_effects = ifelse(negative_priming == "1" | identity_priming_task == "1" | location_priming_task == "1" | masked_priming_task == "1",
                           "yes",
                           "no")
         ) %>%                
  select(authors, year, title, participants, measures, pwp_v_hc, pwp_v_icb, pwp_between_groups_other, pwp_within_groups,   # keep columns of interest
         response_inhibition, response_conflict, oculomotor, delayed_gratification, decision_making_ambiguity, # and keep the new ones we just made
         decision_making_objective, personality_traits, set_switching, priming_effects)                                                



# RESPONSE INHIBITION
# RESPONSE CONFLICT
# 44	stimulus_response_associations ???
# OCULOMOTOR
# DELAYED GRATIFICATION
# DECISION-MAKING UNDER AMBIGUOUS RISK
# 29	economics_task ???
# 35	associative_learning_learning_through_reward_and_punishment_d_mu_ambiguity ???
# DECISION-MAKING UNDER OBJECTIVE RISK
# PERSONALITY TRAITS
# SET-SHIFTING/COGNITIVE FLEXIBILITY
# PRIMING EFFECTS



# 39	color_trails_test_tmt
# 45	beads_task
# 47	gambling_task
# 48	oddball_paradigm
# 56	inhibition_of_return
# 57	covert_orienting_task_inhibition_of_return
# 58	category_fluency
# 59	verbal_fluency
# 60	semantic_priming
# 61	random_number_generation
# 62	selective_attention_task
# 63	attention_network_test
# 64	moving_dots_task
# 65	cue_target_detection_task
# 66	conners_performance_test
# 67	continuous_performance_test
# 68	probabilistic_classification_task_reward_and_punishment_learning
# 69	probabilistic_selection_task
# 70	reversal_learning_reward_and_punishment
# 71	intra_extra_dimensional_set_shifting
# 72	holt_laury_task
# 73	excluded_letter_fluency_test
# 74	x2_back_task
# 75	rewarded_reaction_time
# 76	incentive_value_task
# 77	verb_noun_production_task
# 78	dimensional_integration_task
# 79	orienting_of_attention
# 80	salience_attribution_test
# 81	cued_activation_inhibition_task
# 82	anagram_task
# 83	calculation_task
# 84	binary_choice_task
# 85	valuation_rating_task
# 86	choice_titration_task
# 87	virtual_reality_motor_inhibition_task
# 88	temporal_bisection_task
# 89	temporal_trisection_task
# 90	crossed_response_inhibition_task
# 91	simple_rt_task

# 97	ignored_repetition_task
# 98	repetition_priming
# 99	choice_reaction_task_paper_rock_scissors
# 100	virtual_multiple_errand_test
# 101	anti_cue_keypress_task
# 102	temporal_interval_task
# 103	working_memory_task
# 104	wisconsin_card_sorting_task
# 105	marble_task
# 106	interference_task
# 108	lexical_ambiguity
# 110	visuomotor_tracking_task
# 111	reversal_learning


ui <- dashboardPage(
  dashboardHeader(title = "Impulsivity and inhibitory control in Parkinson's", titleWidth = 450),
  dashboardSidebar(
    checkboxGroupInput("show_vars", "Columns in Sys Review data to show:",
                         names(final_studies), selected = names(final_studies))#,
    #  downloadButton("downloadData", "Download"),
   #   helpText("Click the columns you're interested in and download your choices as a .csv")
    ),
  dashboardBody(
    tabsetPanel(id = "dataset",
                tabPanel("final_studies",
                         div(style = 'overflow-x: scroll',
                             DT::dataTableOutput("mytable1")
                             )
                         )
                )
    )
  )

server <- function(input, output) {
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(final_studies[, input$show_vars, drop = FALSE], rownames = FALSE, list(pageLength = 33, info = FALSE, lengthMenu = list(c(32, -1), c("32", "All"))))
    #DT::datatable(my_apps, options = list(lengthMenu = c(5,30,50), pageLength = 35))
    
  })
  
  datasetInput <- reactive({
    final_studies[, input$show_vars]
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("your_choices", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  

}

shinyApp(ui, server)
