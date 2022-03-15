#######################################################################################
# Shiny application for hazard functions in parametric time to event modelling        #
# Author: Rob Christiaan van Wijk (ORCID: https://orcid.org/0000-0001-7247-1360)      #
# Version: 1.0                                                                        #
# Date: March 11th, 2022                                                              #
#                                                                                     #
# Reference: Rob C van Wijk, Ulrika S H Simonsson, Finding the right hazard function  #
#           for time to event modelling: a tutorial and Shiny app (2022, manuscript   #
#           under review)                                                             #
#                                                                                     #
# R version 4.0.4 (2021-02-15)                                                        #
# packages: shiny version 1.6.0                                                       #
#           tidyverse version 1.3.0                                                   #
#                                                                                     #
#######################################################################################

#####################
# Set-up the script #
#####################

# Load the required packages

library(shiny) #version 1.6.0
library(tidyverse) #version 1.3.0 which includes ggplot2 (version 3.3.3) purrr (version 0.3.4) tibble (version 3.1.0) dplyr (version 1.0.5) tidyr (version 1.1.3) stringr (version 1.4.0) readr (version 1.4.0) forcats (version 0.5.1)

# Create default theme for ggplot2

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

###################################################################################################
# Initial values for the sliders                                                                  #
# set minimum and maximum as variables to facilitate updating the slider/manual input combination #
###################################################################################################

# Time

time     = 180
time_min = 0
time_max = 365

# Drug effect on hazard functions (exponential, Gompertz, Weibull, lognormal, log-logistic, circadian) as a whole under the proportional hazard assumption (coefficients, can be calculated to hazard ratios (hr) by exponentiation)

drug_exp       = 0       
drug_gompertz  = 0
drug_weibull   = 0
drug_lognormal = 0
drug_logl      = 0
drug_circadian = 0
hr_min         = -2
hr_max         = 2 

# Drug effect on individual parameters of the hazard functions, in relative term (%, will multiply the parameter)

drug_lambda = 0
drug_gamma  = 0
drug_mu     = 0
drug_sigma  = 0
drug_amp    = 0
drug_period = 0
drug_phase  = 0
drug_min    = -100
drug_max    = 100

# Parameters of the exponential hazard function

lambda_constant     = 0.05
lambda_constant_min = 0
lambda_constant_max = 1

# Parameters of the Gompertz hazard function

lambda_gompertz     = 0.02
lambda_gompertz_min = 0
lambda_gompertz_max = 1
gamma_gompertz      = -0.001
gamma_gompertz_min  = -0.1
gamma_gompertz_max  = 0.1

# Parameters of the Weibull hazard function

lambda_weibull     = 0.02
lambda_weibull_min = 0
lambda_weibull_max = 1
gamma_weibull      = 0.8
gamma_weibull_min  = -2
gamma_weibull_max  = 2

# Parameters of the log-normal hazard function

mu        = 5
mu_min    = 0
mu_max    = 10
sigma     = 2
sigma_min = 0
sigma_max = 5

# Parameters of the log-logistic hazard function

lambda_logl     = 0.02
lambda_logl_min = 0
lambda_logl_max = 1
gamma_logl      = 1.1
gamma_logl_min  = 0
gamma_logl_max  = 2

# Parameters of the circadian hazard function

lambda_circadian     = 0.02
lambda_circadian_min = 0
lambda_circadian_max = 1
amp                  = 0.1
amp_min              = 0
amp_max              = 1
period               = 180
period_min           = 0
period_max           = 365
phase                = 0
phase_min            = -150
phase_max            = 150


####################################
# Shiny application user interface #
####################################

ui <- fluidPage(
  
  # formatting of the sliders in different colours
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black; border: black}")),
  tags$style(HTML(".js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-15 .irs-single, .js-irs-15 .irs-bar-edge, .js-irs-15 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-16 .irs-single, .js-irs-16 .irs-bar-edge, .js-irs-16 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-17 .irs-single, .js-irs-17 .irs-bar-edge, .js-irs-17 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-18 .irs-single, .js-irs-18 .irs-bar-edge, .js-irs-18 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-19 .irs-single, .js-irs-19 .irs-bar-edge, .js-irs-19 .irs-bar {background: orange; border: orange}")),
  tags$style(HTML(".js-irs-20 .irs-single, .js-irs-20 .irs-bar-edge, .js-irs-20 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-21 .irs-single, .js-irs-21 .irs-bar-edge, .js-irs-21 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-22 .irs-single, .js-irs-22 .irs-bar-edge, .js-irs-22 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-23 .irs-single, .js-irs-23 .irs-bar-edge, .js-irs-23 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-24 .irs-single, .js-irs-24 .irs-bar-edge, .js-irs-24 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-25 .irs-single, .js-irs-25 .irs-bar-edge, .js-irs-25 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-26 .irs-single, .js-irs-26 .irs-bar-edge, .js-irs-26 .irs-bar {background: red; border: red}")),
  tags$style(HTML(".js-irs-27 .irs-single, .js-irs-27 .irs-bar-edge, .js-irs-27 .irs-bar {background: red; border: red}")),

  # title of the application
  
  titlePanel('Shiny application for hazard functions in parametric time to event modelling'),
  
  # Instruction and reference panel at the top of the page (across the max of 12 columns)
  
  fluidRow(
    column(12, p(strong('Instructions'))),
    column(12, p('Graphical exploration of different hazard functions for time to event analysis. These functions can be compared with an non-parametric estimation of the hazard in the time to event data based on the kernel based visual hazard comparison (',a('Goulooze et al, AAPS 20: 5, 2018', href = 'http://dx.doi.org/10.1208/s12248-017-0162-9', target="_blank" ),') or hazard based visual predictive check (',a('Huh & Hutmacher, J Pharmacokinet Pharmacodyn, 43:57-71, 2016', href = 'http://dx.doi.org/10.1007/s10928-015-9454-9', target="_blank"),').')),
    column(12, p("Time and hazard function parameters can be set using the sliders. Alternatively, they can be set in the input box below the slider which has no lower or upper constraint. Effects of covariates/treatment on function as a whole (proportional hazard with hazard ratio output) or on the function's individual parameters on the relative scale can be selected in the same manner. Graphs can be downloaded in tiff format using the provided link, with all relevant parameter values automatically given in the filename for reproducibility.")),
    column(12, ),
    column(12, p(em('Reference: Rob C van Wijk, Ulrika S H Simonsson, Finding the right hazard function for time to event modelling: a tutorial and Shiny app (2022, manuscript under review)'))),
    column(12, p(em('This work is licensed under a ', a('Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License', href = 'https://creativecommons.org/licenses/by-nc-sa/4.0/', target="_blank" ), '.')))),

  # Middle part of the application divided into 4 columns (parameter sliders) and hazard functions (graphical element)
  
  fluidRow(
    column(4, hr(), p(strong('Parameter values')), hr()),
    column(8, hr(), p(strong('Hazard function over time')), hr())),
  
  fluidRow(
    column(2,
           sliderInput('TIME',
                       p('Time duration'), 
                       min = time_min, 
                       max = time_max, 
                       value = time, 
                       step = 1),
           numericInput('TIME_NUM', 
                        '',
                        value = time, 
                        step = 1),
           
           sliderInput('LAMBDA_GOMPERTZ',
                       p('Gompertz model: Lambda'), 
                       min = lambda_gompertz_min, 
                       max = lambda_gompertz_max, 
                       value = lambda_gompertz, 
                       step = 0.001),
           
           numericInput('LAMBDA_GOMPERTZ_NUM', 
                        '',
                        value = lambda_gompertz, 
                        step = 0.001),
           sliderInput('LAMBDA_WEIBULL',
                       p('Weibull model: Lambda'), 
                       min = lambda_weibull_min, 
                       max = lambda_weibull_max, 
                       value = lambda_weibull, 
                       step = 0.001),
           numericInput('LAMBDA_WEIBULL_NUM', 
                        '',
                        value = lambda_weibull, 
                        step = 0.001),
           sliderInput('MU',
                       p('Log-normal model: Mu'), 
                       min = mu_min, 
                       max = mu_max, 
                       value = mu, 
                       step = 0.01),
           numericInput('MU_NUM', 
                        '',
                        value = mu, 
                        step = 0.01),
           sliderInput('LAMBDA_LOGL',
                       p('Log-logistic model: Lambda'), 
                       min = lambda_logl_min, 
                       max = lambda_logl_max, 
                       value = lambda_logl, 
                       step = 0.001),
           numericInput('LAMBDA_LOGL_NUM', 
                        '',
                        value = lambda_logl, 
                        step = 0.001),
           sliderInput('LAMBDA_CIRCADIAN',
                       p('Circadian mode: Lambda'), 
                       min = lambda_circadian_min, 
                       max = lambda_circadian_max, 
                       value = lambda_circadian, 
                       step = 0.001),
           numericInput('LAMBDA_CIRCADIAN_NUM', 
                        '',
                        value = lambda_circadian, 
                        step = 0.001),
           sliderInput('PERIOD',
                       p('Circadian mode: Period'), 
                       min = period_min, 
                       max = period_max, 
                       value = period, 
                       step = 1),
           numericInput('PERIOD_NUM', 
                        '',
                        value = period, 
                        step = 1)
    ),
    column(2, 
           
           sliderInput('LAMBDA_CONSTANT',
                       p('Exponential model: Lambda'), 
                       min = lambda_constant_min, 
                       max = lambda_constant_max, 
                       value = lambda_constant, 
                       step = 0.001),
           numericInput('LAMBDA_CONSTANT_NUM', 
                        '',
                        value = lambda_constant, 
                        step = 0.001),
           sliderInput('GAMMA_GOMPERTZ',
                       p('Gompertz model: Gamma'), 
                       min = gamma_gompertz_min, 
                       max = gamma_gompertz_max, 
                       value = gamma_gompertz, 
                       step = abs(gamma_gompertz) * 0.1),
           numericInput('GAMMA_GOMPERTZ_NUM', 
                        '',
                        value = gamma_gompertz, 
                        step = abs(gamma_gompertz) * 0.1),
           sliderInput('GAMMA_WEIBULL',
                       p('Weibull model: Gamma'), 
                       min = gamma_weibull_min, 
                       max = gamma_weibull_max, 
                       value = gamma_weibull, 
                       step = 0.01),
           numericInput('GAMMA_WEIBULL_NUM', 
                        '',
                        value = gamma_weibull, 
                        step = 0.01),
           sliderInput('SIGMA',
                       p('Log-normal model: Sigma'), 
                       min = sigma_min, 
                       max = sigma_max, 
                       value = sigma, 
                       step = 0.01),          
           numericInput('SIGMA_NUM', 
                        '',
                        value = sigma, 
                        step = 0.01),
           sliderInput('GAMMA_LOGL',
                       p('Log-logistic model: Gamma'), 
                       min = gamma_logl_min, 
                       max = gamma_logl_max, 
                       value = gamma_logl, 
                       step = 0.01),
           numericInput('GAMMA_LOGL_NUM', 
                        '',
                        value = gamma_logl, 
                        step = 0.01),
           sliderInput('AMP',
                       p('Circadian model: Amplitude'), 
                       min = amp_min, 
                       max = amp_max, 
                       value = amp, 
                       step = 0.01),
           numericInput('AMP_NUM', 
                        '',
                        value = amp, 
                        step = 0.01),
           sliderInput('PHASE',
                       p('Circadian model: Phase'), 
                       min = phase_min, 
                       max = phase_max, 
                       value = phase, 
                       step = 1),
           numericInput('PHASE_NUM', 
                        '',
                        value = phase, 
                        step = 1)
    ),
    
    # Right hand side of the middle panel with graphical hazard functions and the download button to download the graph
    
    column(4,
           downloadLink("downloadConstant", "Download Exponential plot"),
           plotOutput("CONSTANT"), 
           
           downloadLink("downloadWeibull", "Download Weibull plot"),
           plotOutput("WEIBULL"),

           downloadLink("downloadLoglogistic", "Download Log-logistic plot"),
           plotOutput("LOGLOGISTIC")
    ),
    column(4,
           downloadLink("downloadGompertz", "Download Gompertz plot"),
           plotOutput("GOMPERTZ"), 
           
           downloadLink("downloadLognormal", "Download Log-normal plot"),
           plotOutput("LOGNORMAL"), 
           
           downloadLink("downloadCircadian", "Download Circadian plot"),
           plotOutput("CIRCADIAN")
    )
  ),
  
  # Bottom panel with the covariate/treatment effect on the hazard function as a whole 
  
  fluidRow(
    column(12, 
           hr(), 
           p(strong('Effect of covariates/treatment on hazard function (proportional hazard)')), 
           p(em('Hazard ratio is calculated by exponentiating the coefficient, displayed here with 3 significant digits.')),
           hr())),
  fluidRow(
    column(2,
           sliderInput('DRUG_EXP',
                       p('Coefficient on exponential function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_exp, 
                       step = 0.1),
           numericInput('DRUG_EXP_NUM', 
                        '',
                        value = drug_exp, 
                        step = 0.1),
           textOutput('HR_EXP')
           
    ),
    
    column(2,           
           sliderInput('DRUG_GOMPERTZ',
                       p('Coefficient on Gompertz function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_gompertz, 
                       step = 0.1),
           numericInput('DRUG_GOMPERTZ_NUM', 
                        '',
                        value = drug_gompertz, 
                        step = 0.1),
           textOutput('HR_GOMPERTZ')
           
    ),
    
    column(2,
           sliderInput('DRUG_WEIBULL',
                       p('Coefficient on Weibull function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_weibull, 
                       step = 0.1),
           numericInput('DRUG_WEIBULL_NUM', 
                        '',
                        value = drug_weibull, 
                        step = 0.1),
           textOutput('HR_WEIBULL')
    ),
    
    column(2, 
           sliderInput('DRUG_LOGNORMAL',
                       p('Coefficient on Log-normal function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_lognormal, 
                       step = 0.1),
           numericInput('DRUG_LOGNORMAL_NUM', 
                        '',
                        value = drug_lognormal, 
                        step = 0.1),
           textOutput('HR_LOGNORMAL')
    ),
    
    column(2, 
           
           sliderInput('DRUG_LOGL',
                       p('Coefficient on Log-logistic function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_logl, 
                       step = 0.1),
           numericInput('DRUG_LOGL_NUM', 
                        '',
                        value = drug_logl, 
                        step = 0.1),
           textOutput('HR_LOGL')
    ),
    
    column(2, 
           sliderInput('DRUG_CIRCADIAN',
                       p('Coefficient on Circadian function'), 
                       min = hr_min, 
                       max = hr_max, 
                       value = drug_circadian, 
                       step = 0.1),
           numericInput('DRUG_CIRCADIAN_NUM', 
                        '',
                        value = drug_circadian, 
                        step = 0.1),
           textOutput('HR_CIRCADIAN')
    )

    
  ), 
  
  # Bottom panel with the covariate/treatment effect on the hazard function's individual parameters
  
  fluidRow(
    column(12, hr(), p(strong("Effect of covariates/treatment on function's individual parameters")), hr())),
  fluidRow(
    column(2,
    sliderInput('DRUG_LAMBDA',
                p('Effect on lambda (%)'), 
                min = drug_min, 
                max = drug_max, 
                value = drug_lambda, 
                step = 1, 
                post = '%'),
    numericInput('DRUG_LAMBDA_NUM', 
                 '',
                 value = drug_lambda, 
                 step = 1),
    
    sliderInput('DRUG_PHASE',
                p('Effect on phase (%)'), 
                min = drug_min, 
                max = drug_max, 
                value = drug_phase, 
                step = 1, 
                post = '%'),
    numericInput('DRUG_PHASE_NUM', 
                 '',
                 value = drug_phase, 
                 step = 1),
    p(em('In case of phase=0, set phase equal to period to study the relative drug effect.'))
    
    ),
    
    column(2,
           sliderInput('DRUG_GAMMA',
                       p('Effect on gamma (%)'), 
                       min = drug_min, 
                       max = drug_max, 
                       value = drug_gamma, 
                       step = 1, 
                       post = '%'),
           numericInput('DRUG_GAMMA_NUM', 
                        '',
                        value = drug_gamma, 
                        step = 1)
           
    ),
    
    column(2, 
    sliderInput('DRUG_MU',
                p('Effect on mu (%)'), 
                min = drug_min, 
                max = drug_max, 
                value = drug_mu, 
                step = 1, 
                post = '%'),
    numericInput('DRUG_MU_NUM', 
                 '',
                 value = drug_mu, 
                 step = 1)
    ),
    
    column(2, 
           
           sliderInput('DRUG_SIGMA',
                       p('Effect on sigma (%)'), 
                       min = drug_min, 
                       max = drug_max, 
                       value = drug_sigma, 
                       step = 1, 
                       post = '%'),
           numericInput('DRUG_SIGMA_NUM', 
                        '',
                        value = drug_sigma, 
                        step = 1)
    ),
  
    column(2, 
    sliderInput('DRUG_AMP',
                p('Effect on amplitude (%)'), 
                min = drug_min, 
                max = drug_max, 
                value = drug_amp, 
                step = 1, 
                post = '%'),
    numericInput('DRUG_AMP_NUM', 
                 '',
                 value = drug_amp, 
                 step = 1)
    ),

  column(2, 
         
         sliderInput('DRUG_PERIOD',
                     p('Effect on period (%)'), 
                     min = drug_min, 
                     max = drug_max, 
                     value = drug_period, 
                     step = 1, 
                     post = '%'),
         numericInput('DRUG_PERIOD_NUM', 
                      '',
                      value = drug_period, 
                      step = 1)
  )
  
  )

)

##########
# Server #
##########

server <- function(input, output, session){
  
  #Refresh and synchronize numeric and slider for the same input, so that when the slider is changed, the manual input also changes accordingly, and vice versa (including the minimum and maximum for the slider)
  
  observeEvent(input$TIME, {
    updateNumericInput(session, 'TIME_NUM', value = input$TIME, min = ifelse(input$TIME > time_min, time_min, input$TIME), max = ifelse(input$TIME < time_max, time_max, input$TIME))
  })
  observeEvent(input$TIME_NUM, {
    updateSliderInput(session, 'TIME', value = input$TIME_NUM, min = ifelse(input$TIME_NUM > time_min, time_min, input$TIME_NUM), max = ifelse(input$TIME_NUM < time_max, time_max, input$TIME_NUM))
  })
  
  observeEvent(input$LAMBDA_CONSTANT, {
    updateNumericInput(session, 'LAMBDA_CONSTANT_NUM', value = input$LAMBDA_CONSTANT, min = ifelse(input$LAMBDA_CONSTANT > lambda_constant_min, lambda_constant_min, input$LAMBDA_CONSTANT), max = ifelse(input$LAMBDA_CONSTANT < lambda_constant_max, lambda_constant_max, input$LAMBDA_CONSTANT))
  })
  observeEvent(input$LAMBDA_CONSTANT_NUM, {
    updateSliderInput(session, 'LAMBDA_CONSTANT', value = input$LAMBDA_CONSTANT_NUM, min = ifelse(input$LAMBDA_CONSTANT_NUM > lambda_constant_min, lambda_constant_min, input$LAMBDA_CONSTANT_NUM), max = ifelse(input$LAMBDA_CONSTANT_NUM < lambda_constant_max, lambda_constant_max, input$LAMBDA_CONSTANT_NUM))
  })
  
  observeEvent(input$LAMBDA_GOMPERTZ, {
    updateNumericInput(session, 'LAMBDA_GOMPERTZ_NUM', value = input$LAMBDA_GOMPERTZ, min = ifelse(input$LAMBDA_GOMPERTZ > lambda_gompertz_min, lambda_gompertz_min, input$LAMBDA_GOMPERTZ), max = ifelse(input$LAMBDA_GOMPERTZ < lambda_gompertz_max, lambda_gompertz_max, input$LAMBDA_GOMPERTZ))
  })
  observeEvent(input$LAMBDA_GOMPERTZ_NUM, {
    updateSliderInput(session, 'LAMBDA_GOMPERTZ', value = input$LAMBDA_GOMPERTZ_NUM, min = ifelse(input$LAMBDA_GOMPERTZ_NUM > lambda_gompertz_min, lambda_gompertz_min, input$LAMBDA_GOMPERTZ_NUM), max = ifelse(input$LAMBDA_GOMPERTZ_NUM < lambda_gompertz_max, lambda_gompertz_max, input$LAMBDA_GOMPERTZ_NUM))
  })
  
  observeEvent(input$GAMMA_GOMPERTZ, {
    updateNumericInput(session, 'GAMMA_GOMPERTZ_NUM', value = input$GAMMA_GOMPERTZ, min = ifelse(input$GAMMA_GOMPERTZ > gamma_gompertz_min, gamma_gompertz_min, input$GAMMA_GOMPERTZ), max = ifelse(input$GAMMA_GOMPERTZ < gamma_gompertz_max, gamma_gompertz_max, input$GAMMA_GOMPERTZ))
  })
  observeEvent(input$GAMMA_GOMPERTZ_NUM, {
    updateSliderInput(session, 'GAMMA_GOMPERTZ', value = input$GAMMA_GOMPERTZ_NUM, min = ifelse(input$GAMMA_GOMPERTZ_NUM > gamma_gompertz_min, gamma_gompertz_min, input$GAMMA_GOMPERTZ_NUM), max = ifelse(input$GAMMA_GOMPERTZ_NUM < gamma_gompertz_max, gamma_gompertz_max, input$GAMMA_GOMPERTZ_NUM))
  })
  
  observeEvent(input$LAMBDA_WEIBULL, {
    updateNumericInput(session, 'LAMBDA_WEIBULL_NUM', value = input$LAMBDA_WEIBULL, min = ifelse(input$LAMBDA_WEIBULL > lambda_weibull_min, lambda_weibull_min, input$LAMBDA_WEIBULL), max = ifelse(input$LAMBDA_WEIBULL < lambda_weibull_max, lambda_weibull_max, input$LAMBDA_WEIBULL))
  })
  observeEvent(input$LAMBDA_WEIBULL_NUM, {
    updateSliderInput(session, 'LAMBDA_WEIBULL', value = input$LAMBDA_WEIBULL_NUM, min = ifelse(input$LAMBDA_WEIBULL_NUM > lambda_weibull_min, lambda_weibull_min, input$LAMBDA_WEIBULL_NUM), max = ifelse(input$LAMBDA_WEIBULL_NUM < lambda_weibull_max, lambda_weibull_max, input$LAMBDA_WEIBULL_NUM))
  })
  
  observeEvent(input$GAMMA_WEIBULL, {
    updateNumericInput(session, 'GAMMA_WEIBULL_NUM', value = input$GAMMA_WEIBULL, min = ifelse(input$GAMMA_WEIBULL > gamma_weibull_min, gamma_weibull_min, input$GAMMA_WEIBULL), max = ifelse(input$GAMMA_WEIBULL < gamma_weibull_max, gamma_weibull_max, input$GAMMA_WEIBULL))
  })
  observeEvent(input$GAMMA_WEIBULL_NUM, {
    updateSliderInput(session, 'GAMMA_WEIBULL', value = input$GAMMA_WEIBULL_NUM, min = ifelse(input$GAMMA_WEIBULL_NUM > gamma_weibull_min, gamma_weibull_min, input$GAMMA_WEIBULL_NUM), max = ifelse(input$GAMMA_WEIBULL_NUM < gamma_weibull_max, gamma_weibull_max, input$GAMMA_WEIBULL_NUM))
  })
  
  observeEvent(input$MU, {
    updateNumericInput(session, 'MU_NUM', value = input$MU, min = ifelse(input$MU > mu_min, mu_min, input$MU), max = ifelse(input$MU < mu_max, mu_max, input$MU))
  })
  observeEvent(input$MU_NUM, {
    updateSliderInput(session, 'MU', value = input$MU_NUM, min = ifelse(input$MU_NUM > mu_min, mu_min, input$MU_NUM), max = ifelse(input$MU_NUM < mu_max, mu_max, input$MU_NUM))
  })
  
  observeEvent(input$SIGMA, {
    updateNumericInput(session, 'SIGMA_NUM', value = input$SIGMA, min = ifelse(input$SIGMA > sigma_min, sigma_min, input$SIGMA), max = ifelse(input$SIGMA < sigma_max, sigma_max, input$SIGMA))
  })
  observeEvent(input$SIGMA_NUM, {
    updateSliderInput(session, 'SIGMA', value = input$SIGMA_NUM, min = ifelse(input$SIGMA_NUM > sigma_min, sigma_min, input$SIGMA_NUM), max = ifelse(input$SIGMA_NUM < sigma_max, sigma_max, input$SIGMA_NUM))
  })
  
  observeEvent(input$LAMBDA_LOGL, {
    updateNumericInput(session, 'LAMBDA_LOGL_NUM', value = input$LAMBDA_LOGL, min = ifelse(input$LAMBDA_LOGL > lambda_logl_min, lambda_logl_min, input$LAMBDA_LOGL), max = ifelse(input$LAMBDA_LOGL < lambda_logl_max, lambda_logl_max, input$LAMBDA_LOGL))
  })
  observeEvent(input$LAMBDA_LOGL_NUM, {
    updateSliderInput(session, 'LAMBDA_LOGL', value = input$LAMBDA_LOGL_NUM, min = ifelse(input$LAMBDA_LOGL_NUM > lambda_logl_min, lambda_logl_min, input$LAMBDA_LOGL_NUM), max = ifelse(input$LAMBDA_LOGL_NUM < lambda_logl_max, lambda_logl_max, input$LAMBDA_LOGL_NUM))
  })
  
  observeEvent(input$GAMMA_LOGL, {
    updateNumericInput(session, 'GAMMA_LOGL_NUM', value = input$GAMMA_LOGL, min = ifelse(input$GAMMA_LOGL > gamma_logl_min, gamma_logl_min, input$GAMMA_LOGL), max = ifelse(input$GAMMA_LOGL < gamma_logl_max, gamma_logl_max, input$GAMMA_LOGL))
  })
  observeEvent(input$GAMMA_LOGL_NUM, {
    updateSliderInput(session, 'GAMMA_LOGL', value = input$GAMMA_LOGL_NUM, min = ifelse(input$GAMMA_LOGL_NUM > gamma_logl_min, gamma_logl_min, input$GAMMA_LOGL_NUM), max = ifelse(input$GAMMA_LOGL_NUM < gamma_logl_max, gamma_logl_max, input$GAMMA_LOGL_NUM))
  })
  
  observeEvent(input$GAMMA_WEIBULL, {
    updateNumericInput(session, 'GAMMA_WEIBULL_NUM', value = input$GAMMA_WEIBULL, min = ifelse(input$GAMMA_WEIBULL > gamma_weibull_min, gamma_weibull_min, input$GAMMA_WEIBULL), max = ifelse(input$GAMMA_WEIBULL < gamma_weibull_max, gamma_weibull_max, input$GAMMA_WEIBULL))
  })
  observeEvent(input$GAMMA_WEIBULL_NUM, {
    updateSliderInput(session, 'GAMMA_WEIBULL', value = input$GAMMA_WEIBULL_NUM, min = ifelse(input$GAMMA_WEIBULL_NUM > gamma_weibull_min, gamma_weibull_min, input$GAMMA_WEIBULL_NUM), max = ifelse(input$GAMMA_WEIBULL_NUM < gamma_weibull_max, gamma_weibull_max, input$GAMMA_WEIBULL_NUM))
  })  
  
  observeEvent(input$LAMBDA_CIRCADIAN, {
    updateNumericInput(session, 'LAMBDA_CIRCADIAN_NUM', value = input$LAMBDA_CIRCADIAN, min = ifelse(input$LAMBDA_CIRCADIAN > lambda_circadian_min, lambda_circadian_min, input$LAMBDA_CIRCADIAN), max = ifelse(input$LAMBDA_CIRCADIAN < lambda_circadian_max, lambda_circadian_max, input$LAMBDA_CIRCADIAN))
  })
  observeEvent(input$LAMBDA_CIRCADIAN_NUM, {
    updateSliderInput(session, 'LAMBDA_CIRCADIAN', value = input$LAMBDA_CIRCADIAN_NUM, min = ifelse(input$LAMBDA_CIRCADIAN_NUM > lambda_circadian_min, lambda_circadian_min, input$LAMBDA_CIRCADIAN_NUM), max = ifelse(input$LAMBDA_CIRCADIAN_NUM < lambda_circadian_max, lambda_circadian_max, input$LAMBDA_CIRCADIAN_NUM))
  })
  
  observeEvent(input$AMP, {
    updateNumericInput(session, 'AMP_NUM', value = input$AMP, min = ifelse(input$AMP > amp_min, amp_min, input$AMP), max = ifelse(input$AMP < amp_max, amp_max, input$AMP))
  })
  observeEvent(input$AMP_NUM, {
    updateSliderInput(session, 'AMP', value = input$AMP_NUM, min = ifelse(input$AMP_NUM > amp_min, amp_min, input$AMP_NUM), max = ifelse(input$AMP_NUM < amp_max, amp_max, input$AMP_NUM))
  })
  
  observeEvent(input$PERIOD, {
    updateNumericInput(session, 'PERIOD_NUM', value = input$PERIOD, min = ifelse(input$PERIOD > period_min, period_min, input$PERIOD), max = ifelse(input$PERIOD < period_max, period_max, input$PERIOD))
  })
  observeEvent(input$PERIOD_NUM, {
    updateSliderInput(session, 'PERIOD', value = input$PERIOD_NUM, min = ifelse(input$PERIOD_NUM > period_min, period_min, input$PERIOD_NUM), max = ifelse(input$PERIOD_NUM < period_max, period_max, input$PERIOD_NUM))
  })
  
  observeEvent(input$PHASE, {
    updateNumericInput(session, 'PHASE_NUM', value = input$PHASE, min = ifelse(input$PHASE > phase_min, phase_min, input$PHASE), max = ifelse(input$PHASE < phase_max, phase_max, input$PHASE))
  })
  observeEvent(input$PHASE_NUM, {
    updateSliderInput(session, 'PHASE', value = input$PHASE_NUM, min = ifelse(input$PHASE_NUM > phase_min, phase_min, input$PHASE_NUM), max = ifelse(input$PHASE_NUM < phase_max, phase_max, input$PHASE_NUM))
  })
  
  observeEvent(input$DRUG_LAMBDA, {
    updateNumericInput(session, 'DRUG_LAMBDA_NUM', value = input$DRUG_LAMBDA, min = ifelse(input$DRUG_LAMBDA > drug_min, drug_min, input$DRUG_LAMBDA), max = ifelse(input$DRUG_LAMBDA < drug_max, drug_max, input$DRUG_LAMBDA))
  })
  observeEvent(input$DRUG_LAMBDA_NUM, {
    updateSliderInput(session, 'DRUG_LAMBDA', value = input$DRUG_LAMBDA_NUM, min = ifelse(input$DRUG_LAMBDA_NUM > drug_min, drug_min, input$DRUG_LAMBDA_NUM), max = ifelse(input$DRUG_LAMBDA_NUM < drug_max, drug_max, input$DRUG_LAMBDA_NUM))
  })
  
  observeEvent(input$DRUG_GAMMA, {
    updateNumericInput(session, 'DRUG_GAMMA_NUM', value = input$DRUG_GAMMA, min = ifelse(input$DRUG_GAMMA > drug_min, drug_min, input$DRUG_GAMMA), max = ifelse(input$DRUG_GAMMA < drug_max, drug_max, input$DRUG_GAMMA))
  })
  observeEvent(input$DRUG_GAMMA_NUM, {
    updateSliderInput(session, 'DRUG_GAMMA', value = input$DRUG_GAMMA_NUM, min = ifelse(input$DRUG_GAMMA_NUM > drug_min, drug_min, input$DRUG_GAMMA_NUM), max = ifelse(input$DRUG_GAMMA_NUM < drug_max, drug_max, input$DRUG_GAMMA_NUM))
  })
  
  observeEvent(input$DRUG_MU, {
    updateNumericInput(session, 'DRUG_MU_NUM', value = input$DRUG_MU, min = ifelse(input$DRUG_MU > drug_min, drug_min, input$DRUG_MU), max = ifelse(input$DRUG_MU < drug_max, drug_max, input$DRUG_MU))
  })
  observeEvent(input$DRUG_MU_NUM, {
    updateSliderInput(session, 'DRUG_MU', value = input$DRUG_MU_NUM, min = ifelse(input$DRUG_MU_NUM > drug_min, drug_min, input$DRUG_MU_NUM), max = ifelse(input$DRUG_MU_NUM < drug_max, drug_max, input$DRUG_MU_NUM))
  })
  
  observeEvent(input$DRUG_SIGMA, {
    updateNumericInput(session, 'DRUG_SIGMA_NUM', value = input$DRUG_SIGMA, min = ifelse(input$DRUG_SIGMA > drug_min, drug_min, input$DRUG_SIGMA), max = ifelse(input$DRUG_SIGMA < drug_max, drug_max, input$DRUG_SIGMA))
  })
  observeEvent(input$DRUG_SIGMA_NUM, {
    updateSliderInput(session, 'DRUG_SIGMA', value = input$DRUG_SIGMA_NUM, min = ifelse(input$DRUG_SIGMA_NUM > drug_min, drug_min, input$DRUG_SIGMA_NUM), max = ifelse(input$DRUG_SIGMA_NUM < drug_max, drug_max, input$DRUG_SIGMA_NUM))
  })
  
  observeEvent(input$DRUG_AMP, {
    updateNumericInput(session, 'DRUG_AMP_NUM', value = input$DRUG_AMP, min = ifelse(input$DRUG_AMP > drug_min, drug_min, input$DRUG_AMP), max = ifelse(input$DRUG_AMP < drug_max, drug_max, input$DRUG_AMP))
  })
  observeEvent(input$DRUG_AMP_NUM, {
    updateSliderInput(session, 'DRUG_AMP', value = input$DRUG_AMP_NUM, min = ifelse(input$DRUG_AMP_NUM > drug_min, drug_min, input$DRUG_AMP_NUM), max = ifelse(input$DRUG_AMP_NUM < drug_max, drug_max, input$DRUG_AMP_NUM))
  })
  
  observeEvent(input$DRUG_PERIOD, {
    updateNumericInput(session, 'DRUG_PERIOD_NUM', value = input$DRUG_PERIOD, min = ifelse(input$DRUG_PERIOD > drug_min, drug_min, input$DRUG_PERIOD), max = ifelse(input$DRUG_PERIOD < drug_max, drug_max, input$DRUG_PERIOD))
  })
  observeEvent(input$DRUG_PERIOD_NUM, {
    updateSliderInput(session, 'DRUG_PERIOD', value = input$DRUG_PERIOD_NUM, min = ifelse(input$DRUG_PERIOD_NUM > drug_min, drug_min, input$DRUG_PERIOD_NUM), max = ifelse(input$DRUG_PERIOD_NUM < drug_max, drug_max, input$DRUG_PERIOD_NUM))
  })  
  
  observeEvent(input$DRUG_PHASE, {
    updateNumericInput(session, 'DRUG_PHASE_NUM', value = input$DRUG_PHASE, min = ifelse(input$DRUG_PHASE > drug_min, drug_min, input$DRUG_PHASE), max = ifelse(input$DRUG_PHASE < drug_max, drug_max, input$DRUG_PHASE))
  })
  observeEvent(input$DRUG_PHASE_NUM, {
    updateSliderInput(session, 'DRUG_PHASE', value = input$DRUG_PHASE_NUM, min = ifelse(input$DRUG_PHASE_NUM > drug_min, drug_min, input$DRUG_PHASE_NUM), max = ifelse(input$DRUG_PHASE_NUM < drug_max, drug_max, input$DRUG_PHASE_NUM))
  })
  
  observeEvent(input$DRUG_EXP, {
    updateNumericInput(session, 'DRUG_EXP_NUM', value = input$DRUG_EXP, min = ifelse(input$DRUG_EXP > hr_min, hr_min, input$DRUG_EXP), max = ifelse(input$DRUG_EXP < hr_max, hr_max, input$DRUG_EXP))
  })
  observeEvent(input$DRUG_EXP_NUM, {
    updateSliderInput(session, 'DRUG_EXP', value = input$DRUG_EXP_NUM, min = ifelse(input$DRUG_EXP_NUM > hr_min, hr_min, input$DRUG_EXP_NUM), max = ifelse(input$DRUG_EXP_NUM < hr_max, hr_max, input$DRUG_EXP_NUM))
  })
  
  observeEvent(input$DRUG_GOMPERTZ, {
    updateNumericInput(session, 'DRUG_GOMPERTZ_NUM', value = input$DRUG_GOMPERTZ, min = ifelse(input$DRUG_GOMPERTZ > hr_min, hr_min, input$DRUG_GOMPERTZ), max = ifelse(input$DRUG_GOMPERTZ < hr_max, hr_max, input$DRUG_GOMPERTZ))
  })
  observeEvent(input$DRUG_GOMPERTZ_NUM, {
    updateSliderInput(session, 'DRUG_GOMPERTZ', value = input$DRUG_GOMPERTZ_NUM, min = ifelse(input$DRUG_GOMPERTZ_NUM > hr_min, hr_min, input$DRUG_GOMPERTZ_NUM), max = ifelse(input$DRUG_GOMPERTZ_NUM < hr_max, hr_max, input$DRUG_GOMPERTZ_NUM))
  })

  observeEvent(input$DRUG_WEIBULL, {
    updateNumericInput(session, 'DRUG_WEIBULL_NUM', value = input$DRUG_WEIBULL, min = ifelse(input$DRUG_WEIBULL > hr_min, hr_min, input$DRUG_WEIBULL), max = ifelse(input$DRUG_WEIBULL < hr_max, hr_max, input$DRUG_WEIBULL))
  })
  observeEvent(input$DRUG_WEIBULL_NUM, {
    updateSliderInput(session, 'DRUG_WEIBULL', value = input$DRUG_WEIBULL_NUM, min = ifelse(input$DRUG_WEIBULL_NUM > hr_min, hr_min, input$DRUG_WEIBULL_NUM), max = ifelse(input$DRUG_WEIBULL_NUM < hr_max, hr_max, input$DRUG_WEIBULL_NUM))
  })
  
  observeEvent(input$DRUG_LOGNORMAL, {
    updateNumericInput(session, 'DRUG_LOGNORMAL_NUM', value = input$DRUG_LOGNORMAL, min = ifelse(input$DRUG_LOGNORMAL > hr_min, hr_min, input$DRUG_LOGNORMAL), max = ifelse(input$DRUG_LOGNORMAL < hr_max, hr_max, input$DRUG_LOGNORMAL))
  })
  observeEvent(input$DRUG_LOGNORMAL_NUM, {
    updateSliderInput(session, 'DRUG_LOGNORMAL', value = input$DRUG_LOGNORMAL_NUM, min = ifelse(input$DRUG_LOGNORMAL_NUM > hr_min, hr_min, input$DRUG_LOGNORMAL_NUM), max = ifelse(input$DRUG_LOGNORMAL_NUM < hr_max, hr_max, input$DRUG_LOGNORMAL_NUM))
  })
  
  observeEvent(input$DRUG_LOGL, {
    updateNumericInput(session, 'DRUG_LOGL_NUM', value = input$DRUG_LOGL, min = ifelse(input$DRUG_LOGL > hr_min, hr_min, input$DRUG_LOGL), max = ifelse(input$DRUG_LOGL < hr_max, hr_max, input$DRUG_LOGL))
  })
  observeEvent(input$DRUG_LOGL_NUM, {
    updateSliderInput(session, 'DRUG_LOGL', value = input$DRUG_LOGL_NUM, min = ifelse(input$DRUG_LOGL_NUM > hr_min, hr_min, input$DRUG_LOGL_NUM), max = ifelse(input$DRUG_LOGL_NUM < hr_max, hr_max, input$DRUG_LOGL_NUM))
  })
  
  observeEvent(input$DRUG_CIRCADIAN, {
    updateNumericInput(session, 'DRUG_CIRCADIAN_NUM', value = input$DRUG_CIRCADIAN, min = ifelse(input$DRUG_CIRCADIAN > hr_min, hr_min, input$DRUG_CIRCADIAN), max = ifelse(input$DRUG_CIRCADIAN < hr_max, hr_max, input$DRUG_CIRCADIAN))
  })
  observeEvent(input$DRUG_CIRCADIAN_NUM, {
    updateSliderInput(session, 'DRUG_CIRCADIAN', value = input$DRUG_CIRCADIAN_NUM, min = ifelse(input$DRUG_CIRCADIAN_NUM > hr_min, hr_min, input$DRUG_CIRCADIAN_NUM), max = ifelse(input$DRUG_CIRCADIAN_NUM < hr_max, hr_max, input$DRUG_CIRCADIAN_NUM))
  })

#Calculating the hazard ratio based on the coefficient input with 3 significant digits
  
  output$HR_EXP <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_EXP), digits = 3))
  })
  output$HR_GOMPERTZ <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_GOMPERTZ), digits = 3))
  })
  output$HR_WEIBULL <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_WEIBULL), digits = 3))
  })
  output$HR_LOGNORMAL <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_LOGNORMAL), digits = 3))
  })
  output$HR_LOGL <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_LOGL), digits = 3))
  })
  output$HR_CIRCADIAN <- reactive({
    paste("Hazard ratio:", signif(exp(input$DRUG_CIRCADIAN), digits = 3))
  })
  
  #Create simulation dataset 
  
  simulation <- reactive({
    req(input$TIME,
        input$LAMBDA_CONSTANT,
        input$LAMBDA_GOMPERTZ,
        input$GAMMA_GOMPERTZ,        
        input$LAMBDA_WEIBULL,
        input$GAMMA_WEIBULL,
        input$MU,
        input$SIGMA,
        input$LAMBDA_LOGL,
        input$GAMMA_LOGL,
        input$LAMBDA_CIRCADIAN,
        input$AMP,
        input$PERIOD,
        input$PHASE,
        input$DRUG_LAMBDA,
        input$DRUG_GAMMA,
        input$DRUG_MU,
        input$DRUG_SIGMA,
        input$DRUG_AMP,
        input$DRUG_PERIOD,
        input$DRUG_PHASE,
        input$DRUG_EXP,
        input$DRUG_GOMPERTZ,
        input$DRUG_WEIBULL,
        input$DRUG_LOGNORMAL,
        input$DRUG_LOGL,
        input$DRUG_CIRCADIAN
    )
    
    data.frame(time = 0:input$TIME) %>%
      
      #hazard functions (including alternative parameterizations for weibull and log-logistic)
      mutate(constant = input$LAMBDA_CONSTANT) %>%
      mutate(weibull = input$LAMBDA_WEIBULL * input$GAMMA_WEIBULL * (input$LAMBDA_WEIBULL * time)**(input$GAMMA_WEIBULL - 1)) %>%
      mutate(weibull2 = input$LAMBDA_WEIBULL * input$GAMMA_WEIBULL * (time)**(input$GAMMA_WEIBULL - 1)) %>%
      mutate(gompertz = input$LAMBDA_GOMPERTZ * exp(input$GAMMA_GOMPERTZ * time)) %>%
      mutate(lognormal = ((input$SIGMA * time * sqrt(2*pi)) ** (-1) * exp(-0.5 * ((log(time) - input$MU)/input$SIGMA)**2)) / (1 - pnorm(((log(time) - input$MU)/input$SIGMA)))) %>% 
      mutate(logl = (input$LAMBDA_LOGL * input$GAMMA_LOGL * (input$LAMBDA_LOGL * time)**(input$GAMMA_LOGL - 1)) / (1 + (input$LAMBDA_LOGL * time)**(input$GAMMA_LOGL))) %>%
      mutate(logl2 = (input$LAMBDA_LOGL * input$GAMMA_LOGL * (time)**(input$GAMMA_LOGL - 1)) / (1 + (input$LAMBDA_LOGL * time)**(input$GAMMA_LOGL))) %>%
      mutate(circadian = input$LAMBDA_CIRCADIAN * (1 + input$AMP * sin(((2 * pi) / (input$PERIOD)) * (time + input$PHASE)))) %>% 
      
      #drug effect on function (proportional hazard)
      mutate(constant_drug_hr = constant * exp(input$DRUG_EXP)) %>%
      mutate(weibull_drug_hr = weibull * exp(input$DRUG_WEIBULL)) %>%
      mutate(weibull2_drug_hr = weibull2 * exp(input$DRUG_WEIBULL)) %>%
      mutate(gompertz_drug_hr = gompertz * exp(input$DRUG_GOMPERTZ)) %>%
      mutate(lognormal_drug_hr = lognormal * exp(input$DRUG_LOGNORMAL)) %>%
      mutate(logl_drug_hr = logl * exp(input$DRUG_LOGL)) %>%
      mutate(logl2_drug_hr = logl2 * exp(input$DRUG_LOGL)) %>%
      mutate(circadian_drug_hr = circadian * exp(input$DRUG_CIRCADIAN)) %>%
      
      #drug effect on individual parameters
      mutate(constant_drug = input$LAMBDA_CONSTANT * (1 + input$DRUG_LAMBDA/100)) %>%
      mutate(weibull_drug = input$LAMBDA_WEIBULL * (1 + input$DRUG_LAMBDA/100) * input$GAMMA_WEIBULL * (1 + input$DRUG_GAMMA/100) * (input$LAMBDA_WEIBULL * (1 + input$DRUG_LAMBDA/100) * time)**(input$GAMMA_WEIBULL * (1 + input$DRUG_GAMMA/100) - 1)) %>%
      mutate(weibull2_drug = input$LAMBDA_WEIBULL * (1 + input$DRUG_LAMBDA/100) * input$GAMMA_WEIBULL * (1 + input$DRUG_GAMMA/100) * (time)**(input$GAMMA_WEIBULL * (1 + input$DRUG_GAMMA/100) - 1)) %>%
      mutate(gompertz_drug = input$LAMBDA_GOMPERTZ * (1 + input$DRUG_LAMBDA/100) * exp(input$GAMMA_GOMPERTZ * (1 + input$DRUG_GAMMA/100) * time)) %>% 
      mutate(lognormal_drug = ((input$SIGMA * (1 + input$DRUG_SIGMA/100) * time * sqrt(2*pi)) ** (-1) * exp(-0.5 * ((log(time) - input$MU * (1 + input$DRUG_MU/100))/(input$SIGMA * (1 + input$DRUG_SIGMA/100)))**2)) / (1 - pnorm(((log(time) - (input$MU * (1 + input$DRUG_MU/100)))/(input$SIGMA * (1 + input$DRUG_SIGMA/100)))))) %>% 
      mutate(logl_drug = (input$LAMBDA_LOGL * (1 + input$DRUG_LAMBDA/100) * input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100) * (input$LAMBDA_LOGL * (1 + input$DRUG_LAMBDA/100) * time)**(input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100) - 1)) / (1 + (input$LAMBDA_LOGL * (1 + input$DRUG_LAMBDA/100) * time)**(input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100)))) %>%
      mutate(logl2_drug = (input$LAMBDA_LOGL * (1 + input$DRUG_LAMBDA/100) * input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100) * (time)**(input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100) - 1)) / (1 + (input$LAMBDA_LOGL * (1 + input$DRUG_LAMBDA/100) * time)**(input$GAMMA_LOGL * (1 + input$DRUG_GAMMA/100)))) %>%
      mutate(circadian_drug = input$LAMBDA_CIRCADIAN * (1 + input$DRUG_LAMBDA/100) * (1 + input$AMP * (1 + input$DRUG_AMP/100) * sin(((2 * pi) / (input$PERIOD * (1 + input$DRUG_PERIOD/100))) * (time + input$PHASE * (1 + input$DRUG_PHASE/100))))) 
      
      }
  )
  
  #Create plots including covariate/treatment effect in orange (hazard ratio) or red (individual parameter effects)
  
  plotCONSTANT <- reactive({
    ggplot(simulation(), aes(time, constant)) + 
      geom_line(aes(y = constant_drug_hr), col = 'orange') + 
      geom_line(aes(y = constant_drug), col = 'red') + 
      geom_line() + 
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, max(simulation()$constant, simulation()$constant_drug_hr, simulation()$constant_drug, na.rm=T)*1.25), name = expression(Hazard~~(time^'-1'))) + 
      ggtitle(paste('Exponential model\nlambda = ', input$LAMBDA_CONSTANT, ifelse(input$DRUG_EXP != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_EXP), digits = 3), sep = ''), ''), ifelse(input$DRUG_LAMBDA != 0, paste('\ndrug effect (red) on lambda = ', input$DRUG_LAMBDA, '%', sep = ''), ''), sep = ''))
  }
  )
  
  output$CONSTANT <- renderPlot({
    print(plotCONSTANT())

  }
  )
  
  plotGOMPERTZ <- reactive({
    ggplot(simulation(), aes(time, gompertz)) + 
      geom_line(aes(y = gompertz_drug_hr), col = 'orange') + 
      geom_line(aes(y = gompertz_drug), col = 'red') + 
      geom_line() + 
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, max(simulation()$gompertz, simulation()$gompertz_drug_hr, simulation()$gompertz_drug, na.rm=T)*1.25), name = expression(Hazard~~(time^'-1'))) + 
      ggtitle(paste('Gompertz model\nlambda = ', input$LAMBDA_GOMPERTZ, ', gamma = ', input$GAMMA_GOMPERTZ, ifelse(input$DRUG_GOMPERTZ != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_GOMPERTZ), digits = 3), sep = ''), ''), ifelse(input$DRUG_LAMBDA != 0, paste('\ndrug effect (red) on lambda = ', input$DRUG_LAMBDA, '%', sep = ''), ''), ifelse(input$DRUG_GAMMA != 0, paste('\ndrug effect (red) on gamma = ', input$DRUG_GAMMA, '%', sep = ''), ''), sep = ''))
  }
  )
  
  output$GOMPERTZ <- renderPlot({
    print(plotGOMPERTZ())
    
  }
  ) 
  
  plotWEIBULL <- reactive({
    ggplot(simulation(), aes(time, weibull)) + 
      geom_line(aes(y = weibull_drug_hr), col = 'orange') + 
      geom_line(aes(y = weibull2_drug_hr), col = 'orange', linetype = 'dashed') + 
      geom_line(aes(y = weibull_drug), col = 'red') + 
      geom_line(aes(y = weibull2_drug), col = 'red', linetype = 'dashed') + 
      geom_line() + 
      geom_line(aes(time, weibull2), linetype = 'dashed') +
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, NA), name = expression(Hazard~~(time^'-1'))) + 
      ggtitle(paste('Weibull model\ndashed: alternative parameterization\nlambda = ', input$LAMBDA_WEIBULL, ', gamma = ', input$GAMMA_WEIBULL, ifelse(input$DRUG_WEIBULL != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_WEIBULL), digits = 3), sep = ''), ''), ifelse(input$DRUG_LAMBDA != 0, paste('\ndrug effect (red) on lambda = ', input$DRUG_LAMBDA, '%', sep = ''), ''), ifelse(input$DRUG_GAMMA != 0, paste('\ndrug effect (red) on gamma = ', input$DRUG_GAMMA, '%', sep = ''), ''), sep = ''))

  }
  )
  
  output$WEIBULL <- renderPlot({
    print(plotWEIBULL())
    
  }
  ) 
  
  plotLOGNORMAL <- reactive({
    ggplot(simulation(), aes(time, lognormal)) + 
      geom_line(aes(y = lognormal_drug_hr), col = 'orange') + 
      geom_line(aes(y = lognormal_drug), col = 'red') + 
      geom_line() + 
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, max(simulation()$lognormal, simulation()$lognormal_drug_hr, simulation()$lognormal_drug, na.rm=T)*1.25), name = expression(Hazard~~(time^'-1'))) + 
      ggtitle(paste('Log-normal model\nmu= ', input$MU, ', sigma = ', input$SIGMA, ifelse(input$DRUG_LOGNORMAL != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_LOGNORMAL), digits = 3), sep = ''), ''), ifelse(input$DRUG_MU != 0, paste('\ndrug effect (red) on mu = ', input$DRUG_MU, '%', sep = ''), ''), ifelse(input$DRUG_SIGMA != 0, paste('\ndrug effect (red) on sigma = ', input$DRUG_SIGMA, '%', sep = ''), ''), sep = ''))
    
  }
  )
  
  output$LOGNORMAL <- renderPlot({
    print(plotLOGNORMAL())
    
  }
  ) 

  
  plotLOGLOGISTIC <- reactive({
    ggplot(simulation(), aes(time, logl)) +
      geom_line(aes(y = logl_drug_hr), col = 'orange') + 
      geom_line(aes(y = logl2_drug_hr), col = 'orange', linetype = 'dashed') + 
      geom_line(aes(y = logl_drug), col = 'red') + 
      geom_line(aes(y = logl2_drug), col = 'red', linetype = 'dashed') + 
      geom_line() +
      geom_line(aes(time, logl2), linetype = 'dashed') +
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, NA), name = expression(Hazard~~(time^'-1'))) +
      ggtitle(paste('Log-logistic model\ndashed: alternative parameterization\nlambda = ', input$LAMBDA_LOGL, ', gamma = ', input$GAMMA_LOGL, ifelse(input$DRUG_LOGL != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_LOGL), digits = 3), sep = ''), ''), ifelse(input$DRUG_LAMBDA != 0, paste('\ndrug effect (red) on lambda = ', input$DRUG_LAMBDA, '%', sep = ''), ''), ifelse(input$DRUG_GAMMA != 0, paste('\ndrug effect (red) on gamma = ', input$DRUG_GAMMA, '%', sep = ''), ''), sep = ''))
    
  }
  )
  
  output$LOGLOGISTIC <- renderPlot({
    print(plotLOGLOGISTIC())
    
  }
  ) 
  plotCIRCADIAN <- reactive({
    ggplot(simulation(), aes(time, circadian)) + 
      geom_line(aes(y = circadian_drug_hr), col = 'orange') + 
      geom_line(aes(y = circadian_drug), col = 'red') + 
      geom_line() + 
      scale_x_continuous(name = 'Time') +
      scale_y_continuous(limits = c(0, max(simulation()$circadian, simulation()$circadian_drug_hr, simulation()$circadian_drug, na.rm=T)*1.25), name = expression(Hazard~~(time^'-1'))) + 
      ggtitle(paste('Circadian model\nlambda = ', input$LAMBDA_CIRCADIAN, 
                    ', amplitude = ', input$AMP,
                    ',\nperiod = ', input$PERIOD,
                    ', phase = ', input$PHASE, 
                    ifelse(input$DRUG_CIRCADIAN != 0, paste('\nhazard ratio (orange) = ', signif(exp(input$DRUG_CIRCADIAN), digits = 3), sep = ''), ''), 
                    ifelse(input$DRUG_LAMBDA != 0, paste('\ndrug effect (red) on lambda = ', input$DRUG_LAMBDA, '%', sep = ''), ''), 
                    ifelse(input$DRUG_AMP != 0, paste('\ndrug effect (red) on amplitude = ', input$DRUG_AMP, '%', sep = ''), ''), 
                    ifelse(input$DRUG_PERIOD != 0, paste('\ndrug effect (red) on period = ', input$DRUG_PERIOD, '%', sep = ''), ''), 
                    ifelse(input$DRUG_PHASE != 0, paste('\ndrug effect (red) on phase = ', input$DRUG_PHASE, '%', sep = ''), ''), 
                    sep = ''))
    
  }
  )
  
  output$CIRCADIAN <- renderPlot({
    print(plotCIRCADIAN())
    
  }
  ) 
  
  #Create downloadable to download the plot with the filename reflecting the selected parameter and covariate/treatment effect values, where relevant (with none selected, the ifelse statement will just print "" to the file name)
  
  output$downloadConstant <- downloadHandler(
    filename = function() {
      paste("HazardTool_Exponential_lambda_", 
            input$LAMBDA_CONSTANT, 
            ifelse(input$DRUG_EXP != 0, paste('_hr_', signif(exp(input$DRUG_EXP), digits = 3), sep = ''), ''),
            ifelse(input$DRUG_LAMBDA != 0, paste('_druglambda_', input$DRUG_LAMBDA, sep = ''), ''),
            ".tiff", sep="")
    },
    content = function(file) {
      tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
      print(plotCONSTANT())
      dev.off()
    }
  )
  
   output$downloadGompertz <- downloadHandler(
     filename = function() {
       paste("HazardTool_Gompertz_lambda_", input$LAMBDA_GOMPERTZ, "_gamma_", input$GAMMA_GOMPERTZ, 
             ifelse(input$DRUG_GOMPERTZ != 0, paste('_hr_', signif(exp(input$DRUG_GOMPERTZ), digits = 3), sep = ''), ''),
             ifelse(input$DRUG_LAMBDA != 0, paste('_druglambda_', input$DRUG_LAMBDA, sep = ''), ''),
             ifelse(input$DRUG_GAMMA != 0, paste('_druggamma_', input$DRUG_GAMMA, sep = ''), ''),
             ".tiff", sep="")
     },
     content = function(file) {
       tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
       print(plotGOMPERTZ())
       dev.off()
     }
   )
   
   output$downloadWeibull <- downloadHandler(
    filename = function() {
      paste("HazardTool_Weibull_lambda_", input$LAMBDA_WEIBULL, "_gamma_", input$GAMMA_WEIBULL, 
            ifelse(input$DRUG_WEIBULL != 0, paste('_hr_', signif(exp(input$DRUG_WEIBULL), digits = 3), sep = ''), ''),
            ifelse(input$DRUG_LAMBDA != 0, paste('_druglambda_', input$DRUG_LAMBDA, sep = ''), ''),
            ifelse(input$DRUG_GAMMA != 0, paste('_druggamma_', input$DRUG_GAMMA, sep = ''), ''),
            ".tiff", sep="")
    },
    content = function(file) {
      tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
      print(plotWEIBULL())
      dev.off()
    }
  )
 
   output$downloadLognormal <- downloadHandler(
     filename = function() {
       paste("HazardTool_Lognormal_mu_", input$MU, "_sigma_", input$SIGMA, 
             ifelse(input$DRUG_LOGNORMAL != 0, paste('_hr_', signif(exp(input$DRUG_LOGNORMAL), digits = 3), sep = ''), ''),
             ifelse(input$DRUG_MU != 0, paste('_drugmu_', input$DRUG_MU, sep = ''), ''),
             ifelse(input$DRUG_SIGMA != 0, paste('_drugsigma_', input$DRUG_SIGMA, sep = ''), ''),
             ".tiff", sep="")
     },
     content = function(file) {
       tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
       print(plotLOGNORMAL())
       dev.off()
     }
   )
   
  output$downloadLoglogistic <- downloadHandler(
    filename = function() {
      paste("HazardTool_Loglogistic_lambda_", input$LAMBDA_LOGL, "_gamma_", input$GAMMA_LOGL, 
            ifelse(input$DRUG_LOGL != 0, paste('_hr_', signif(exp(input$DRUG_LOGL), digits = 3), sep = ''), ''),
            ifelse(input$DRUG_LAMBDA != 0, paste('_druglambda_', input$DRUG_LAMBDA, sep = ''), ''),
            ifelse(input$DRUG_GAMMA != 0, paste('_druggamma_', input$DRUG_GAMMA, sep = ''), ''),
            ".tiff", sep="")
    },
    content = function(file) {
      tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
      print(plotLOGLOGISTIC())
      dev.off()
    }
  )
  
  output$downloadCircadian <- downloadHandler(
    filename = function() {
      paste("HazardTool_Circadian_lambda_", input$LAMBDA_CIRCADIAN, "_amplitude_", input$AMP, "_period_", input$PERIOD, "_phase_", input$PHASE, 
            ifelse(input$DRUG_CIRCADIAN != 0, paste('_hr_', signif(exp(input$DRUG_CIRCADIAN), digits = 3), sep = ''), ''),
            ifelse(input$DRUG_LAMBDA != 0, paste('_druglambda_', input$DRUG_LAMBDA, sep = ''), ''),
            ifelse(input$DRUG_AMP != 0, paste('_drugamplitude_', input$DRUG_AMP, sep = ''), ''),
            ifelse(input$DRUG_PERIOD != 0, paste('_drugperiod_', input$DRUG_PERIOD, sep = ''), ''),
            ifelse(input$DRUG_PHASE != 0, paste('_drugphase_', input$DRUG_PHASE, sep = ''), ''),
            ".tiff", sep="")
    },
    content = function(file) {
      tiff(file, width = 10, height = 10, unit = 'cm', res = 300)
      print(plotCIRCADIAN())
      dev.off()
    }
  )
  
  
}


############### 
# Run the app #
###############
shinyApp(ui = ui, server = server)
