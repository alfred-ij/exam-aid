#---------------------------------------------libraries and other sources---------------------------------------------
library(shiny)
library(shinydashboard)
library(visNetwork)
library(shinyFeedback)
library(fontawesome)
library(shinyjs)
library(dplyr) #df manipulation
library(ggplot2) #plotting 
library(kableExtra); library(knitr) #knitting
library(tidyverse) #df manipulation
library(methods) # class structure
library(Dict) #dictionary
library(latex2exp) #latex text
library(tinsel) #decorator functions
library(tidyr) #df manipulation
library(shinycssloaders)
library(DT)
library(here)
source('discreteProbs.R')
source('discreteProducts.R')

#---------------------------------------------helper functions---------------------------------------------
plot_survivalDist_info <- function(user_inputs,type,radix=10000,select=2,x_range=list(20,110)){
  #initialize class object
  survivalDist_info_object <- discreteProbs$new(
    age_range = list(0,130), #distribution age limits
    mort_law = user_inputs['mort_law'], #underlying mortality law
    frac_asump = user_inputs['frac_asump'], #fraction assumption
    m = user_inputs['m'], #m-th periodicity
    other_params = user_inputs['mort_params'] #other parameters
  )
  #plot distribution ranges...t_p_x and t_q_x
  if (type=='whole'){
    survivalDist_info_plot <- 
      data.frame(t = user_inputs['t']) %>%
      mutate(p=sapply(t, survivalDist_info_object$p_x_t, x=user_inputs['x'])) %>%
      mutate(q=sapply(t, survivalDist_info_object$q_x_t, x=user_inputs['x'])) %>%
      ggplot() +
      geom_point(aes(x=t, y=p), color='blue') +
      geom_point(aes(x=t, y=q), color='red') +
      labs( 
        title = paste0('Discrete Survival Distribitions for a Life-Aged (', user_inputs['x'],')'),
        x = TeX(r'(Discrete Future Period $(t)$)'),
        y = TeX(r'(Survival Probabilities: $_tp_x$ (blue) and $_tq_x$ (red))'),
        subtitle = paste0("Based on: ", user_inputs['mort_law'],"'s Law of Mortality for ",user_inputs['t'][1],' till ', tail(user_inputs['t'],1), ' future periods.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      scale_colour_manual(values = cols) 
    #return plot
    return(survivalDist_info_plot)
  }else if (type=='frac'){
    survivalDist_info_plot <- 
      data.frame(s = as.numeric(seq(0, 0.9, 0.1))) %>%
      mutate(p=sapply(s, survivalDist_info_object$frac_p_x_s, x=user_inputs['x'], select=FALSE)) %>%
      mutate(q=sapply(s, survivalDist_info_object$frac_q_x_s, x=user_inputs['x'], select=FALSE)) %>%
      ggplot() +
      geom_point(aes(x=s, y=p), color='blue') +
      geom_point(aes(x=s, y=q), color='red') +
      labs( 
        title = paste0('Discrete Survival Distribitions for a Life-Aged (', user_inputs['x'],')'),
        x = TeX(r'(Discrete Future Period $(t)$)'),
        y = TeX(r'(Survival Probabilities: $_sp_x$ (blue) and $_sq_x$ (red))'),
        subtitle = paste0("Based on: ", user_inputs['mort_law'],"'s Law of Mortality"),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      scale_colour_manual(values = cols) 
    #return plot
    return(survivalDist_info_plot)    
  }else if (type=='complete'){
    survivalDist_info_df <- 
      data.frame(t = 129:(user_inputs['x']+1)) %>%
      mutate(p=sapply(130-t, survivalDist_info_object$p_x_t, x=user_inputs['x'])) 
    #compute complete-life variables
    survivalDist_info_num <- sum(survivalDist_info_df$p)
    #return plot
    return(survivalDist_info_num)
  }else if (type == 'life'){
    survivalDist_info_plot <- 
      data.frame(t = user_inputs['t']) %>%
      mutate(l=sapply(t, survivalDist_info_object$lt_x_t, x=user_inputs['x'], radix=radix)) %>%
      ggplot() +
      geom_point(aes(x=t, y=l), color='blue') +
      labs( 
        title = paste0('Discrete Life-Table for lives aged (', user_inputs['x']+user_inputs['t'][1],') till (' ,user_inputs['x']+tail(user_inputs['t'],1),')'),
        x = TeX(r'(Discrete Future Period $(t)$)'),
        y = TeX(r'(Life-Table Cohort: $l_{(x+t)}$ (blue))'),
        subtitle = paste0("Based on: ", user_inputs['mort_law'],"'s Law of Mortality for ",user_inputs['t'][1],' till ', tail(user_inputs['t'],1), ' future periods, with a radix of ', radix, '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      scale_colour_manual(values = cols) 
    #return plot
    return(survivalDist_info_plot)
  } else if (type == 'select'){
    survivalDist_info_plot <- 
      data.frame(t = user_inputs['t']) %>%
      mutate(select_p=sapply(t, survivalDist_info_object$select_p_x_t, x=user_inputs['x'], d=select)) %>%
      mutate(p=sapply(t, survivalDist_info_object$p_x_t, x=user_inputs['x'])) %>%
      ggplot() +
      geom_point(aes(x=t, y=select_p), color='red') +
      geom_point(aes(x=t, y=p), color='blue') +
      labs( 
        title = paste0('Discrete Survival Distribitions for a Life-Aged (', user_inputs['x'],')'),
        x = TeX(r'(Discrete Future Period $(t)$)'),
        y = TeX(r'(Survival Probabilities: $_tp_x$ (blue) and $_tp_{\[x\]}$ (red))'),
        subtitle = paste0("Based on: ", user_inputs['mort_law'],"'s Law of Mortality for ",user_inputs['t'][1],' till ', tail(user_inputs['t'],1), ' future periods, with a select period of ', select, '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      scale_colour_manual(values = cols) 
    #return plot
    return(survivalDist_info_plot)
  } else if(type=='ultimate'){
    d <- select; age_list <- x_range
    #create data frame with desired life-table values
    survivalDist_info_plot <- data.frame(age_x=c((age_list[[1]]-d):age_list[[2]])) %>% 
      #define ultimate period age
      mutate(!!paste('age+',d,sep=''):=age_x+d) %>% 
      #define ultimate period life-table value
      mutate(!!paste('l_x+',d,sep=''):=sapply(!!sym(paste('age+',d,sep='')),
                                              survivalDist_info_object$lt_x_t,t=0,
                                              radix=radix,
                                              x0=age_list[[1]])) 
    for(select_index in c(1:d)){ 
      #loop through periods
      temp_name <- paste(select_index,"_p_[x]",sep='') 
      #initialize survival feature name
      survivalDist_info_plot <- survivalDist_info_plot %>%
        #compute select period survival probabilities
        mutate(!!temp_name:=sapply(age_x,survivalDist_info_object$select_p_x_t,t=select_index,d=d)) 
      if (select_index > 1){ #setup period survival probabilities
        #initialize survival feature name...select-1
        previous_temp_name <- paste(select_index-1,"_p_[x]",sep='') 
        #initialize survival feature name...select
        current_temp_name <- paste(select_index,"_p_[x]",sep='') 
        #initialize survival feature name...select-1
        new_p_name <- paste("1_p_[x]+",select_index-1,sep='') 
        #initialize life-table feature name...select
        new_l_name <- paste("l_[x]+",select_index-1,sep='') 
        survivalDist_info_plot <- survivalDist_info_plot %>%
          #compute survival probabilities
          mutate(!!new_p_name:= !!sym(current_temp_name) / !!sym(previous_temp_name)) %>% 
          #compute life-table values
          mutate(!!new_l_name:= !!sym(paste('l_x+',d,sep=''))/!!sym(new_p_name)) 
        survivalDist_info_plot[1:d,new_l_name] <- NA
      }
      if (select_index == d){
        #initialize life-table feature name
        l_name <- 'l_[x]' 
        survivalDist_info_plot <- survivalDist_info_plot %>%
          #compute life-table values
          mutate(!!l_name := !!sym(paste('l_x+',d,sep=''))/!!sym(paste(select_index,"_p_[x]",sep=''))) 
        survivalDist_info_plot[1:d,l_name] <- NA
      }
    }
    return(survivalDist_info_plot)
  }
}
plot_fom_info <- function(user_inputs){
  #initialize class object
  fom_info_object <- discreteProbs$new(
    age_range = list(0,130), #distribution age limits
    mort_law = user_inputs['mort_law'], #underlying mortality law
    frac_asump = user_inputs['frac_asump'], #fraction assumption
    m = user_inputs['m'], #m-th periodicity
    other_params = user_inputs['mort_params'] #other parameters
  )
  #plot distribution ranges...t_p_x and t_q_x
  fom_info_plot <- 
    data.frame(t = user_inputs['t']) %>%
    mutate(mu=sapply(t, fom_info_object$mu_x_t, x = user_inputs['x'])) %>%
    mutate(f=sapply(t, fom_info_object$f_x_t, x = user_inputs['x'])) %>%
    ggplot() +
    geom_point(aes(x=t, y=mu), color='blue') +
    labs(
      title = paste0('Discrete Force of Mortality for a Life-Aged (', user_inputs['x'],')'),
      x = TeX(r'(Discrete Future Period $(t)$)'),
      y = TeX(r'(Force of Mortality: $mu_{x+t}$)'),
      subtitle = paste0("Based on: ", user_inputs['mort_law'],"'s Law of Mortality for ",user_inputs['t'][1],' till ', tail(user_inputs['t'],1), ' future periods.'),
      color='Life Age (x)'
    ) +
    theme_dark() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    scale_colour_manual(values = cols)
  return(fom_info_plot)
}
plot_product_info <- function(user_input,type){
  #initialize class object
  product_info_object <- discreteProducts$new(
    age_range = list(0,130), #distribution age limits
    mort_law = user_input['mort_law'], #underlying mortality law
    frac_asump = user_input['frac_asump'], #fraction assumption
    m = user_input['m'], #m-th periodicity
    other_params = user_input['mort_params'], #other parameters
    i = user_input['i'], #interest rate
    annuity_schedule = user_input['annuity_schedule'] #annuity schedule
  )
  
  if(type=='wholeInsurance'){
    #plot distribution ranges...t_p_x and t_q_x
    product_info_plot <- 
      data.frame(t = user_input['t']) %>%
      mutate(x_t = t+user_input['x']) %>%
      mutate(w_A=sapply(x_t, product_info_object$whole_A_x, moment = 1)) %>%
      ggplot() +
      geom_point(aes(x=x_t, y=w_A), color='blue') +
      labs(
        title = paste0('Discrete Whole Life Insurance'),
        x = TeX(r'(Discrete Life Ages $(x+t)$)'),
        y = TeX(r'(Whole Life Insurance: $A_{x+t}$)'),
        subtitle = paste0("Based on: ", user_input['mort_law'],"'s Law of Mortality for lives aged ",user_input['t'][1]+user_input['x'],' till ', tail(user_input['t'],1)+user_input['x'], '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      scale_colour_manual(values = cols)
    return(product_info_plot)
  } else if(type == 'termInsurance'){
    product_info_plot <- 
      data.frame(t = user_input['t']) %>%
      mutate(x_t = t+user_input['x']) %>%
      mutate(t_A=sapply(x_t, product_info_object$term_A_x, moment = 1, n = user_input['n'])) %>%
      ggplot() +
      geom_point(aes(x=x_t, y=t_A), color='blue') +
      labs(
        title = paste0('Discrete Term Life Insurance'),
        x = TeX(r'(Discrete Life Ages $(x+t)$)'),
        y = TeX(r'(Term Life Insurance)'),
        subtitle = paste0("Based on: ", user_input['mort_law'],"'s Law of Mortality for lives aged ",user_input['t'][1]+user_input['x'],' till ', tail(user_input['t'],1)+user_input['x'], ' with a term period of ',user_input['n'], '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      scale_colour_manual(values = cols)
    return(product_info_plot)
  } else if(type == 'endowmentInsurance'){
    product_info_plot <- 
      data.frame(t = user_input['t']) %>%
      mutate(x_t = t+user_input['x']) %>%
      mutate(e_A=sapply(x_t, product_info_object$endowment_A_x, moment = 1, n = user_input['n'])) %>%
      ggplot() +
      geom_point(aes(x=x_t, y=e_A), color='blue') +
      labs(
        title = paste0('Discrete Endowment Life Insurance'),
        x = TeX(r'(Discrete Life Ages $(x+t)$)'),
        y = TeX(r'(Endowment Insurance)'),
        subtitle = paste0("Based on: ", user_input['mort_law'],"'s Law of Mortality for lives aged ",user_input['t'][1]+user_input['x'],' till ', tail(user_input['t'],1)+user_input['x'], ' with a term period of ',user_input['n'], '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      scale_colour_manual(values = cols)
    return(product_info_plot)
  }else if(type=='insuranceTable'){
    product_info_table <- 
      data.frame(
        label = c(
          '\\(A_x\\): Whole Life Insurance',
          '\\(\\require{enclose} A_{\\acute{x} \\colon \\; \\enclose{actuarial}{n}}\\): Term Life Insurance',
          '\\(\\require{enclose} A_{x \\colon \\enclose{actuarial}{n}}\\): Endowment Insurance',
          '\\(_{u|}A_x\\): Deferred Whole Life Insurance',
          '\\(\\require{enclose} _{u|}A_{\\acute{x} \\colon \\; \\enclose{actuarial}{n}}\\): Deferred Term Life Insurance',
          '\\(_nE_x \\): Pure Endowment Insurance'
        ),
        value = c(
          product_info_object$whole_A_x(x=user_input['x'],moment=1),
          product_info_object$term_A_x(x=user_input['x'],n=user_input['n'],moment=1),
          product_info_object$endowment_A_x(x=user_input['x'],n=user_input['n'],moment=1),
          product_info_object$deferred_whole_A_x(x=user_input['x'],u=user_input['u'],moment=1),
          product_info_object$deferred_term_A_x(x=user_input['x'],n=user_input['n'],u=user_input['u'],moment=1),
          product_info_object$n_E_x(x=user_input['x'],n=user_input['n'])
        )
      )
    return(product_info_table)
  }else if(type=='wholeAnnuity'){
    product_info_plot <- 
      data.frame(t = user_input['t']) %>%
      mutate(x_t = t+user_input['x']) %>%
      mutate(w_a=sapply(x_t, product_info_object$whole_a_x)) %>%
      ggplot() +
      geom_point(aes(x=x_t, y=w_a), color='blue') +
      labs(
        title = paste0('Discrete Whole Life Annuitiy'),
        x = TeX(r'(Discrete Life Ages $(x+t)$)'),
        y = TeX(r'(Whole Life Annuity)'),
        subtitle = paste0("Based on: ", user_input['mort_law'],"'s Law of Mortality for lives aged ",user_input['t'][1]+user_input['x'],' till ', tail(user_input['t'],1)+user_input['x'], '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      scale_colour_manual(values = cols)
    return(product_info_plot)
  }else if(type=='termAnnuity'){
    product_info_plot <- 
      data.frame(t = user_input['t']) %>%
      mutate(x_t = t+user_input['x']) %>%
      mutate(t_a=sapply(x_t, product_info_object$term_a_x,n=user_input['n'])) %>%
      ggplot() +
      geom_point(aes(x=x_t, y=t_a), color='blue') +
      labs(
        title = paste0('Discrete Term Life Annuitiy'),
        x = TeX(r'(Discrete Life Ages $(x+t)$)'),
        y = TeX(r'(Term Life Annuity)'),
        subtitle = paste0("Based on: ", user_input['mort_law'],"'s Law of Mortality for lives aged ",user_input['t'][1]+user_input['x'],' till ', tail(user_input['t'],1)+user_input['x'], ' with a term period of ',user_input['n'], '.'),
        color='Life Age (x)'
      ) +
      theme_dark() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      scale_colour_manual(values = cols)
    return(product_info_plot)
  }else if(type=='annuityTable'){
    product_info_table <- 
      data.frame(
        label = c(
          '\\(\\ddot{a}_x\\): Whole Life Annuity',
          '\\(\\require{enclose} \\ddot{a}_{\\acute{x} \\colon \\; \\enclose{actuarial}{n}}\\): Term Life Annuity',
          '\\(_{u|}\\ddot{a}_x\\): Deferred Whole Life Annuity',
          '\\(\\require{enclose} _{u|}\\ddot{a}_{\\acute{x} \\colon \\; \\enclose{actuarial}{n}}\\): Deferred Term Life Annuity'
        ),
        value = c(
          product_info_object$whole_a_x(x=user_input['x']),
          product_info_object$term_a_x(x=user_input['x'],n=user_input['n']),
          product_info_object$deferred_whole_a_x(x=user_input['x'],u=user_input['u']),
          product_info_object$deferred_term_a_x(x=user_input['x'],n=user_input['n'],u=user_input['u'])
        )
      )
    return(product_info_table)
  }
}
#---------------------------------------------custom settings---------------------------------------------
custom_width <- 325
#---------------------------------------------components definition---------------------------------------------
  #---------------------------------------------header definition---------------------------------------------
header <- dashboardHeader(
  title = tagList( #dashboard main title
    icon("laptop-code",class = "pull-right"),
    "Long-Term Actuarial Modeling"
  ),
  titleWidth = custom_width #match custom width in sidebar
)
  #---------------------------------------------sidebar definition---------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu( #sidebar menu
    menuItem('§1: Overview', icon = icon('clipboard-list',class = "pull-right"), tabName = 'overview'),
    menuItem("§2: Insurable Interest", icon = icon("user-injured",class = "pull-right"), tabName = "insurable"),
    menuItem("§3: Survival Models", icon = icon("heart-pulse",class = "pull-right"), tabName = "survival"),
    menuItem("§4: Life-Contingent Products", icon = icon("cart-shopping",class = "pull-right"), tabName = "product"),
    menuItem('§5: Policy Values', icon = icon('money-bill-trend-up',class = "pull-right"), tabName = 'profit'),
    menuItem('§6: Notes', icon = icon('note-sticky',class = "pull-right"), tabName = 'notes')
  ),
  width = custom_width #custom width matched with header
)
  #---------------------------------------------body definition definition---------------------------------------------
body <- dashboardBody(
  #---------------------------------------------css styling---------------------------------------------
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js?config=TeX-MML-AM_CHTML", defer = NA),
    tags$script(HTML("
        MathJax.Hub.Config({
          extensions: ['enclose.js'], // Enable the enclose extension
          TeX: {
            Macros: {
              // You can define additional custom macros here if needed
            }
          }
        });
      "))
  ),
  tags$head(
    tags$style(HTML("
    .centered-row {
      display: flex;
      justify-content: center;
    }
  "))
  ),
  tags$head(
    tags$style(HTML("
    .centered-tabBox .description-content {
      padding: 10px;
      font-size: 14px;
      line-height: 1.5;
      overflow-y: auto;
      max-height: '50%';
      box-sizing: border-box;
    }"))
  ),
  tags$head( #css for centralizing tabBox
    tags$style(HTML("
        .centered-tabBox {
          display: flex;
          justify-content: center;
        }
      "))
  ),
    tags$head(
    tags$style(HTML("
      .box-content {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
    "))
  ),
  tags$head( #css for creating white space
    tags$style(HTML("
        .white-space {
          height: 30px;  /* Adjust the height as needed */
        }
      "))
  ),
  tags$head( #css for creating white space
    tags$style(HTML("
        .white-space-mini {
          height: 10px;  /* Adjust the height as needed */
        }
      "))
  ),
  #---------------------------------------------overview tab---------------------------------------------
  tabItems( #overview tab content
    tabItem(tabName = "overview",
            h2(
              class = "centralized-h2",
              tagList(
                "Overview",
                icon("clipboard-list",class = "pull-center")
              ),
              class = "heading-with-icon"
            ),
            h3(
              class = "left-h4",
              tagList(
                "Prelude",
                icon("section",class = "pull-left")
              ),
              class = "heading-with-icon"
            ),
            tags$style(HTML("
              .box-header .box-title {
                width: 100%;
                text-align: center;
              }
            ")),
            div(class = "white-space"),
            fluidRow( #prelude content
              class = "centered-tabBox",
              box( #overview title and 5 W's description
                title = tagList(shiny::icon("magnifying-glass-chart"), 'Welcome to the Long-term Fundamentals of Actuarial Mathematics dashboard!'),
                solidHeader = TRUE, status = "primary", width = 10,
                "Using the 5 W's framework to break down the goals and motivations behind the creation of the dashboard provides clarity, structure, comprehensiveness, accessibility for all users. By addressing each of the 5 W's, this introduction not only explains the what, why, how, who, and when of the Long-term Fundamentals of Actuarial Mathematics dashboard but also highlights the thoughtfulness and effort behind its creation. Applying this framework to our project for the 'Long-term Fundamentals of Actuarial Mathematics' dashboard helps clarify its purpose, scope, and benefits."
              )
            ),
            useShinyjs(),  # Initialize shinyjs
            fluidRow(
              tags$div(class = "custom-tabbox",
                 tags$div(class = "introNote",
                    actionButton("infoButton", "", icon = icon("comment-dots")))),
              hidden(div(id = "infoPanel", 
                         HTML("• In the context of explaining the Long-Term Fundamentals of Actuarial Mathematics dashboard using the 5 W's framework, <b>How</b> is used instead of <b>Where</b> because:"),
                         br(),
                         HTML("&emsp; - How addresses the methodology, and techniques used to develop and implement the dashboard; explains the steps, tools, and programming principles applied to create the dashboard; which is essential for understanding the project's execution and functionality."),
                         br(),
                         HTML("&emsp; - Where typically refers to the physical or virtual location where something occurs; in the context of a software tool like a dashboard; 'where' is less relevant because the dashboard is not tied to a specific location but rather to its usage and application context.")
                         ))),
            div(class = "white-space-mini"),
            fluidRow( # 5 W's explanation and break-down
              class = "centered-tabBox",
              tabBox(
                title = tagList(shiny::icon("clipboard-question"), "The 5 W's of this tool"),
                id = "abstract", height = "100px", side = 'left', width = '10',
                tabPanel("1. What is this tool?", 
                         "This tool is an examination, study, and learning assistant designed specifically for actuarial topics. It provides a comprehensive understanding of various actuarial subjects, offering an alternative to existing literature by integrating data science perspectives. The content is adapted from the latest Society of Actuaries (SOA) syllabus, ensuring it covers essential topics such as insurable interest, survival models, life-contingent products, policy value, and more."),
                tabPanel("2. Why is this tool relevant?", 
                         "This tool is relevant because it provides a unique learning approach by combining traditional actuarial science with modern data science techniques. This fusion allows for more dynamic and interactive learning experiences, which can enhance comprehension and retention of complex actuarial concepts. It also serves as a supplementary resource for actuarial students and educators, providing additional support and an alternative perspective to standard textbooks."),
                tabPanel("3. How is it implemented?", 
                         "The implementation of this tool is through Shiny dashboards, an R package that allows for the creation of interactive web applications. These dashboards provide an intuitive and user-friendly interface for exploring actuarial topics, making the learning process more engaging. The use of interactive elements like plots, sliders, and input fields allows users to visualize and manipulate data, fostering a deeper understanding of the material."),
                tabPanel("4. Who will benefit from it?", 
                         "The primary beneficiaries of this tool are actuarial students and educators. Students can use the dashboards to supplement their learning, gaining a better grasp of actuarial concepts through interactive and practical examples. Educators can utilize the tool to enhance their teaching methods, providing a more dynamic and engaging classroom experience. Additionally, data science enthusiasts interested in actuarial science will find this tool valuable as it bridges the gap between the two fields."),
                tabPanel("5. When can this tool be used?", 
                         "This tool can be used at any point in the actuarial learning journey, whether for initial learning, revision, or deepening understanding of specific topics. Its flexibility and adaptability make it a useful resource for ongoing education and professional development in actuarial and data science.")
              )),
            div(class = "white-space-mini"),
            h3(
              class = "left-h4",
              tagList(
                "Catalog", #catalog section for table of content in succeeding sections
                icon("section",class = "pull-left")
              ),
              class = "heading-with-icon"
            ),
            fluidRow(
              class = "centered-tabBox",
              box( #syllabus information
                title = tagList(shiny::icon("folder-open",class = "pull-right"), "SOA FAM Examination Study Information"),
                solidHeader = TRUE, status = "warning", width = 10,
                "The Society of Actuaries (SOA) offers various resources for Exam FAM preparation, which include regularly updated syllabi, past exam questions with solutions, practice question videos with solutions, study method advice, and tips for multiple choice exams. These resources can be accessed on the FAM study page on the SOA website.", br(), HTML("&emsp;- FAM study page link: www.soa.org/education/exam-req/edu-exam-fam/study/")
              )
            ),
            fluidRow( #catalog content
              class = "centered-tabBox",
              box( #insurable interest content
                title = tagList(shiny::icon("user-injured",class = "pull-right"), "Insurable Interest"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§7)", br(),
                "- Definition and application of concept" , br(),
                "- Identify long-term life contingent products"
              ),
              box( #survival models content
                title = tagList(shiny::icon("heart-pulse",class = "pull-right"), "Survival Models"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§8 & §9) ", br(),
                "- Parametric survival models" , br(),
                "- Life-tables" , br(),
                "- Force of Mortality" , br(),
                "- Moments of Curtate and Complete Future-Lifetime random variables", br(),
                "- Actuarial notations", br(),
                "- Life-table relationship with survival models", br(),
                "- Selection", br(),
                "- Parametric & Non-parametric Estimators"
              ),
              box( #products content
                title = tagList(shiny::icon("shopping-cart",class = "pull-right"), "Life-Contingent Products"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§10) ", br(),
                "- Present value valuation of Life-contingent products" , br(),
                "- Distribution properties of Life-contingent products" , br(),
                "- Fractional Age and Claim Acceleration approches" , br(),
                "- Relationships between various products", br(),
                "- Actuarial notations", br(),
                "- Impact of variations in underlying survival models"
              ),
              box( #policy values content
                title = tagList(shiny::icon("money-bill-trend-up",class = "pull-right"), "Policy Values"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§11) ", br(),
                "- Future Loss random variable for various products" , br(),
                "- Premium values based on Equivalence or Portfolio Percentile Principles" , br(),
                "- Net, Gross, and Modified-Net premium policies" , br(),
                "- Impact of changes in underlying survival models", br(),
                "- Extra Risk"
              )
            )
    ),
  #---------------------------------------------insurable interest tab---------------------------------------------
    tabItem(tabName = "insurable", #insurable interest content
            h2(
              class = "centralized-h2",
              tagList(
                "Insurable Interest",
                icon("user-injured",class = "pull-center")
              ),
              class = "heading-with-icon"
            ),
            tabsetPanel(
              tabPanel(
                "§1: background",
                div(class = "white-space"),
                h3(
                  class = "left-h4",
                  tagList(
                    "Brief History of Actuarial Science", #catalog section for table of content in succeeding sections
                    icon("section",class = "pull-left")
                  ),
                  class = "heading-with-icon"
                ),
                div(class = "white-space-mini"),
                fluidRow(
                  class = "centered-tabBox",
                  width = 12,
                  height = "300px",
                  visNetworkOutput("briefHistory_flowchart", height = "300px",width = '70%'),
                  column(
                    width = 8,
                    htmlOutput("briefHistory_nodeDescription"))),
                div(class = "white-space-mini"),
                h3(
                  class = "left-h4",
                  tagList(
                    "Concept of Insurable Interest", #catalog section for table of content in succeeding sections
                    icon("section",class = "pull-left")
                  ),
                  class = "heading-with-icon"
                ),
                fluidRow(
                  class = "centered-tabBox",
                  tabBox(
                    title = tagList(shiny::icon("clipboard-question"), "The 5 W's of Insurable Interest"),
                    id = "insurableIntro", height = "100px", side = 'left', width = 12,
                    tabPanel("1. What is insurable interest?", 
                             HTML("<b>Definition: </b>"),"Insurable interest is a legal and financial stake in the insured subject, whether it be a person, property, or event. It is a requirement for the issuance of an insurance policy and ensures that the policyholder will suffer a genuine loss or financial hardship if the insured event occurs.",
                             br(),
                             HTML("<b>Purpose: </b>"),"The purpose of insurable interest is to prevent gambling and ensure that insurance remains a mechanism for risk management rather than speculation."
                    ),
                    tabPanel("2. When is insurable interest relevant?", 
                             HTML("<b>Policy Inception: </b>"),"Insurable interest must exist at the time of policy inception for the policy to be valid. In life insurance, it typically must exist at the time of policy purchase.",
                             br(),
                             HTML("<b>Claim Time: </b>"),"For property and casualty insurance, insurable interest must often exist both at the time of policy inception and at the time of loss."
                    ),
                    tabPanel("3. Where does insurable interest apply?", 
                             HTML("<b>Life Insurance: </b>"),"Insurable interest is required between the policyholder and the insured, commonly in relationships such as family members, business partners, or creditors and debtors.",
                             br(),
                             HTML("<b>Property Insurance: </b>"),"Insurable interest applies to the policyholder's ownership or financial interest in property, such as a home, car, or business assets.",
                             br(),
                             HTML("<b>Health Insurance: </b>"),"Insurable interest is relevant in health policies where the policyholder typically has an insurable interest in their own health or that of family members."
                    ),
                    tabPanel("4. Why is insurable interest important?", 
                             HTML("<b>Legal Requirement: </b>"),"It is a legal requirement to establish that the policyholder has a legitimate interest in the insured subject. This prevents fraudulent activities and ensures that insurance serves its intended purpose of risk mitigation.",
                             br(),
                             HTML("<b>Moral Hazard Reduction: </b>"),"By requiring insurable interest, insurers reduce the risk of moral hazard, where the policyholder might otherwise have an incentive to cause a loss.",
                             br(),
                             HTML("<b>Economic Protection: </b>"),"Insurable interest protects the policyholder from financial loss, ensuring they have a real stake in the insured subject's safety or well-being."
                    ),
                    tabPanel("5. Who is involved in insurable interest?", 
                             HTML("<b>Policyholder: </b>"),"The individual or entity purchasing the insurance policy.",
                             br(),
                             HTML("<b>Insured: </b>"),"The person whose life or property is covered by the policy.",
                             br(),
                             HTML("<b>Insurance Company: </b>"),"The entity providing the insurance coverage.",
                             br(),
                             HTML("<b>Beneficiaries: </b>"),"Individuals or entities who will receive the insurance benefits."
                    )
                  )
                ),
                div(class = "white-space-mini"),
                useShinyjs(),  # Initialize shinyjs
                fluidRow(
                  tags$div(class = "custom-tabbox",tags$div(actionButton("infoButton", "", icon = icon("comment-dots")))),
                  hidden(div(id = "insurablePanel", 
                             HTML("• How is insurable interest determined?"),
                             br(),
                             HTML("&emsp; - <b> Life Insurance </b>: Insurable interest is generally determined by relationships of blood, marriage, or financial dependence. For example, spouses, parents, and business partners typically have an insurable interest in each other's lives."),
                             br(),
                             HTML("&emsp; - <b> Property Insurance </b>: Insurable interest is determined by ownership, possession, or direct financial involvement. For instance, homeowners have an insurable interest in their property, and businesses have an insurable interest in their assets."),
                             br(),
                             HTML("&emsp; - <b> Verification </b>: Insurers may require documentation or proof of the relationship or financial stake to verify insurable interest before issuing a policy. This may include birth certificates, marriage certificates, business contracts, or financial statements.")
                  )))
              ),
              tabPanel(
                "§2: long-term life contingent products",
                tabsetPanel(
                  tabPanel(
                    "§2.1: insurance contracts",
                    tabsetPanel(
                      tabPanel(
                        "§2.1.1: traditional insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Traditional Insurance Contracts',
                            id = 'traditionalIntro', width = 10,
                            tabPanel("Whole-Life","Provides lifetime coverage with fixed premiums and a cash value component that accumulates over time. The cash value can be borrowed against or used to pay premiums."),
                            tabPanel("Term-Life","Offers coverage for a specified period (e.g., 10, 20, or 30 years). It is typically more affordable than whole life insurance and does not accumulate cash value."),
                            tabPanel("Endowment","Pays out a lump sum either on the insured's death or after a specified period. It combines elements of savings and protection.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Traditional Insurance Contracts",
                            width = 10,status='warning',
                            "19th Century: These products were developed to meet the growing demand for financial protection and savings mechanisms. The establishment of life insurance companies and the use of life tables allowed for more accurate pricing and risk assessment."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Traditional Insurance Contracts",
                            width = 10,status='warning',
                            "Traditional insurance products, while providing essential protection and savings features, were often rigid in terms of premiums and benefits. The need for more flexible and customizable insurance solutions led to the development of modern insurance products."
                          ))
                      ),
                      tabPanel(
                        "§2.1.2: modern insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Modern Insurance Contracts',
                            id = 'modernIntro', width = 10,
                            tabPanel("Universal Life","Offers flexible premiums and death benefits, with a cash value component that earns interest based on market rates. Policyholders can adjust their premiums and death benefits to better suit their financial situation."),
                            tabPanel("Variable Life","Allows policyholders to invest the cash value in various investment options, such as stocks and bonds. This can potentially yield higher returns, though it also comes with higher risk."),
                            tabPanel("Indexed Universal Life","Ties the cash value growth to a stock market index, offering potential for higher returns with some level of protection against market downturns.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Modern Insurance Contracts",
                            width = 10, status='warning',
                            "Late 20th Century: The development of universal and variable life insurance products was driven by the need for more flexible financial solutions that could adapt to changing economic conditions and personal financial situations."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution to Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Modern Insurance Contracts",
                            width = 10,status='warning',
                            "The transition from traditional to modern insurance products reflects a broader trend towards greater flexibility, investment options, and customization in financial services. Modern products are designed to better meet the diverse and changing needs of policyholders."
                          ))
                      ),
                      tabPanel(
                        "§2.1.3: long-term health insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Long-Term Health Insurance Contracts',
                            id = 'longTermIntro', width = 10,
                            tabPanel("Long-Term Care","Covers the cost of long-term care services, such as nursing home care, home health care, and assisted living. It addresses the financial risks associated with extended care needs."),
                            tabPanel("Disablility","Provides income replacement if the policyholder becomes unable to work due to a disability. It helps maintain financial stability during periods of disability.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Long-Term Health Insurance Contracts",
                            width = 10,status='warning',
                            "Late 20th Century: The growing recognition of the financial risks associated with aging and disability led to the development and expansion of long-term care and disability insurance products."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution from Traditional Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Long-Term Health Insurance Contracts",
                            width = 10,status='warning',
                            "Traditional health insurance often did not cover long-term care or provide adequate income replacement during periods of disability. The evolution towards specialized long-term care and disability insurance products reflects the need for more comprehensive coverage of health-related financial risks."
                          ))
                      ),
                      tabPanel(
                        "§2.1.4: mutual & proprietary insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Mutual Insurance Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Mutual Insurance',
                            id = 'mutualIntro', width = 10,
                            tabPanel("Definition","Owned by policyholders, with profits distributed as dividends or used to reduce future premiums. Mutual insurers prioritize policyholder benefits."),
                            tabPanel("Types","Can include both traditional and modern life insurance products."),
                            tabPanel("Historical Context","Mutual insurance companies were among the earliest forms of insurance providers, emphasizing policyholder ownership and benefits.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Proprietary Insurance Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Proprietary Insurance',
                            id = 'proprietaryIntro', width = 10,
                            tabPanel("Definition","Owned by shareholders, with profits distributed as dividends to shareholders. Proprietary insurers balance policyholder interests with shareholder returns."),
                            tabPanel("Types","Can include both traditional and modern life insurance products."),
                            tabPanel("Historical Context","Proprietary insurance companies emerged to attract investment capital and expand market offerings.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Relationship Between Mutual and Proprietary Contracts",
                            icon("section",class = "pull-left")
                          )),
                          div(class = "white-space-mini"),
                          fluidRow(
                            class = "centered-tabBox",
                            box(
                              title = "Mutual and Proprietary Contracts",
                              width = 10,status='warning',
                              "Both mutual and proprietary insurance companies offer similar products, but their ownership structures and profit distribution methods differ. Mutual insurers focus on policyholder benefits, while proprietary insurers aim to balance policyholder and shareholder interests."
                            ))
                      ),
                      tabPanel(
                        "§2.1.5: insurance contract underwriting",
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Underwriting Process",
                            icon("section",class = "pull-left")
                          )),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = "The 5 W's of Underwriting",
                            id = "insurableIntro", height = "100px", side = 'left', width = 12,
                            tabPanel("1. What is underwriting?", 
                                     "Underwriting involves multiple stakeholders within an insurance company. The primary individuals involved are underwriters, who assess the risk of insuring applicants. Other key participants include medical professionals who provide health evaluations, actuarial teams who help in risk assessment and pricing, and insurance agents who gather initial applicant information."
                            ),
                            tabPanel("2. When is underwriting relevant?", 
                                     "Underwriting occurs after an insurance application is submitted but before the policy is issued. It is a critical step in the insurance process, ensuring that the insurance company can manage its risk effectively. The specific timeline can vary, but underwriting is typically done within a few weeks of receiving the application."
                            ),
                            tabPanel("3. Where does underwriting happen?", 
                                     "Underwriting is conducted within insurance companies, specifically within their underwriting departments. With the advent of technology, some aspects of underwriting can now be done remotely or through automated systems. Additionally, medical underwriting may involve clinics and laboratories where applicants undergo health evaluations and testing."
                            ),
                            tabPanel("4. Why is underwriting important?", 
                                     "The primary purpose of underwriting is to protect the insurance company from taking on excessive risk, which could lead to financial losses. By carefully evaluating each applicant, underwriters ensure that the premiums collected are adequate to cover potential claims. This process helps maintain the financial stability of the insurance company and allows it to offer competitive rates to its policyholders."
                            ),
                            tabPanel("5. Who is involved in underwriting?", 
                                     "Underwriting involves multiple stakeholders within an insurance company. The primary individuals involved are underwriters, who assess the risk of insuring applicants. Other key participants include medical professionals who provide health evaluations, actuarial teams who help in risk assessment and pricing, and insurance agents who gather initial applicant information."
                            )
                          )
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = "How Underwriting Works",
                            id = "insurableIntro", height = "100px", side = 'left', width = 12,
                            tabPanel("1. Application Submission", 
                                     "The applicant submits a detailed application form, providing personal, medical, and financial information."
                            ),
                            tabPanel("2. Medical Underwriting", 
                                     HTML("<b>Medical History: </b>"),"Reviewing medical records and health questionnaires.",
                                     br(),
                                     HTML("<b>Examinations: </b>"),"Conducting physical exams, blood tests, and other medical evaluations as necessary.",
                                     br(),
                                     HTML("<b>Specialist Reports: </b>"),"Obtaining reports from medical specialists if required."
                            ),
                            tabPanel("3. Financial Underwriting", 
                                     HTML("<b>Income Verification: </b>"),"Assessing the applicant's income and financial stability.",
                                     br(),
                                     HTML("<b>Lifestyle Factors: </b>"),"Evaluating factors such as smoking, alcohol consumption, and hazardous activities."
                            ),
                            tabPanel("4. Risk Classification", 
                                     "Classifying the applicant into risk categories (e.g., preferred, standard, substandard) based on the underwriting assessment."
                            ),
                            tabPanel("5. Premium Calculation", 
                                     "Calculating the premium based on the risk classification, policy type, and coverage amount."
                            ),
                            tabPanel("6. Policy Issuance", 
                                     "Issuing the policy with the agreed terms, conditions, and premium rates."
                            )
                          )
                        ),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Brief History of Underwriting",
                            icon("section",class = "pull-left")
                          )),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          width = 12,
                          height = "300px",
                          visNetworkOutput("underwriting_flowchart", height = "300px",width = '70%'),
                          column(
                            width = 8,
                            htmlOutput("underwriting_nodeDescription", class = "description-content"))
                          )
                      )
                    )
                  ),
                  tabPanel(
                    "§2.2: annuity contracts",
                    h2(
                      tagList(
                        "Brief History of Annuity Contracts",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 12,
                      height = "300px",
                      visNetworkOutput("annuityIntro_flowchart", height = "300px",width = '70%'),
                      column(
                        width = 8,
                        htmlOutput("annuityIntro_nodeDescription", class = "description-content"))
                    ),
                    div(class = "white-space-mini"),
                    h2(
                      tagList(
                        "Accrual Process",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      width = 10,
                      class = "centered-tabBox",
                      box(
                        title = "Accural Process for Annuity Contracts",
                        "Annuities are typically acquired through a purchase process involving underwriting to assess the applicant's age, health status, and financial needs. The underwriting process ensures that the annuity is priced accurately based on the risk profile of the individual."
                      )
                    )
                  ),
                  tabPanel(
                    "§2.3: pension contracts",
                    h2(
                      tagList(
                        "Brief History of Pension Contracts",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 12,
                      height = "300px",
                      visNetworkOutput("pensionIntro_flowchart", height = "300px",width = '70%'),
                      column(
                        width = 8,
                        htmlOutput("pensionIntro_nodeDescription", class = "description-content"))
                    ),
                    div(class = "white-space-mini"),
                    h2(
                      tagList(
                        "Accrual Process",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      width = 10,
                      class = "centered-tabBox",
                      box(
                        title = "Accural Process for Pension Contracts",
                        "Pension benefits accrue over time based on contributions from both employers and employees. In defined benefit plans, benefits are typically calculated based on a formula considering years of service and salary. In defined contribution plans, benefits depend on the amount contributed and the investment performance of those contributions."
                      )
                    )
                  ),
                  tabPanel(
                    "§2.4: other contracts",
                    div(class = "white-space "),
                    h2(
                      tagList(
                        "Health and Disability Insurance",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 10,
                      tabBox(
                        width = 10,
                        title = 'Health and Disability Insurance',
                        tabPanel("Brief History",
                                 tabsetPanel(
                                   tabPanel(
                                     "19th Century",
                                     "Health insurance emerged with mutual aid societies and early commercial policies. These policies were simple and often provided limited coverage for basic medical expenses."
                                   ),
                                   tabPanel(
                                     "20th Century",
                                     "The expansion of health and disability insurance included the growth of employer-sponsored health plans and the introduction of Medicare and Medicaid. These plans provided more comprehensive coverage and were typically acquired through employers or government programs."
                                   ),
                                   tabPanel(
                                     "21st Century",
                                     "The 21st century saw significant changes in health insurance, including the Affordable Care Act (ACA) in the United States. The ACA aimed to increase access to health insurance, improve coverage quality, and reduce costs for individuals and families."
                                   ))
                        ),
                        tabPanel("Accural Process",
                                 "Health and disability insurance policies are typically acquired through an application process involving underwriting to assess the applicant's health status and risk factors. This process ensures that premiums are appropriately priced based on the individual's health and potential for claims.")
                      )
                    ),
                    div(class = "white-space"),
                    h2(
                      tagList(
                        "Long-Term Care Insurance",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 10,
                      tabBox(
                        width = 10,
                        title = 'Long-Term Care Insurance',
                        tabPanel("Brief History",
                                 tabsetPanel(
                                   tabPanel(
                                     "Late 20th Century",
                                     "Long-term care insurance developed to cover extended care services like nursing home and home health care. These policies were designed to address the financial risks of aging and long-term care needs."
                                   ),
                                   tabPanel(
                                     "21st Century",
                                     "The market for long-term care insurance has expanded, offering various coverage options and benefit structures. Policies may include features like inflation protection and hybrid products that combine long-term care coverage with life insurance or annuities."
                                   ))
                        ),
                        tabPanel("Accural Process",
                                 "Long-term care insurance is typically acquired through a detailed application process involving underwriting to assess the applicant's health and long-term care needs. This underwriting process ensures that the policy is priced based on the risk of the individual requiring long-term care services in the future.")
                      )
                    )
                  )
                )
              )
            )
    ),
  #---------------------------------------------survival models tab---------------------------------------------
    tabItem(tabName = "survival",
            fluidPage(
              #---------------------------------------------survival models: styling css---------------------------------------------
              tags$head(
                tags$style(HTML("
                                /* Centralize tabsetPanel tabs */
                                .nav-tabs {
                                    display: flex;
                                    justify-content: center;
                                }
                                .nav-tabs .nav-item {
                                    float: none;
                                }
                                /* Style for dashboardSidebar text */
                                .sidebar-menu li a {
                                    font-size: 16px;
                                    color: #ffffff;
                                }
                                .sidebar-menu li a:hover {
                                    background-color: #444444;
                                }
                                .centralized-h2 {
                                    text-align: center;
                                }
                                .left-h4 {
                                    text-align: left;
                                }
                                "))
              ),
              h2(
                class = "centralized-h2",
                tagList(
                  "Survival Models",
                  icon("heart-pulse",class = "pull-center")
                ),
                class = "heading-with-icon"
              ),
              withMathJax(),
              tabsetPanel(
                #---------------------------------------------survival models: overview tab---------------------------------------------
                tabPanel('§1: overview',
                         withMathJax(),
                         tabsetPanel(
                           tabPanel(
                             '§1.1: introduction',
                             div(class = "white-space-mini"),
                             h3(
                               class = "left-h4",
                               tagList(
                                 "Introduction to Survival Distributions",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             div(class = "white-space-mini"),
                             fluidRow(
                               class = "centered-tabBox",
                             box(
                               width = 10,
                               'In the field of survival analysis, the primary focus is on the time until an event of interest occurs, such as failure of a component or death of an organism. This time-to-event data is characterized by survival distributions, which provide a probabilistic framework for modeling and analyzing such events.'
                             )),
                             tabsetPanel(
                               tabPanel(
                                 "Cumulative Distribution Function (CDF)",
                                 fluidRow(
                                   class = "centered-tabBox",
                                   box(
                                     width = 10,
                                     HTML('
                                      The Cumulative Distribution Function (CDF) of a random variable \\(T\\), which represents the time to the event, is denoted by \\(F(t)\\) and is defined as: $$F(t) = P(T \\leq t)$$
                                      This function gives the probability that the event of interest occurs on or before time \\(t\\). The CDF is non-decreasing and ranges from 0 to 1, capturing the accumulated probability up to a given time.
                                      ')
                                   ))
                               ),
                               tabPanel(
                                 "Probability Density Function (PDF)",
                                 fluidRow(
                                   class = "centered-tabBox",
                                   box(
                                     width = 10,
                                     HTML('
                                      The Probability Density Function (PDF), denoted by \\(f(t)\\), is the derivative of the CDF and represents the rate of change of the CDF with respect to time: $$f(t) = \\frac{dF(t)}{dt}$$
                                      The PDF represents the instantaneous rate of occurrence of the event at time \\(t\\).Unlike the CDF, the PDF can take on values greater than 1 but must integrate to 1 over the range of possible values for \\(T\\), ensuring the total probability is conserved.
                                      ')
                                   ))
                               ),
                               tabPanel(
                                 "Survival Function",
                                 fluidRow(
                                   class = "centered-tabBox",
                                   box(
                                     width = 10,
                                     HTML('
                                      The survival function, \\(S(t)\\), complements the CDF and represents the probability that the event has not occurred by time \\(t\\): $$S(t) = P(T > t) = 1 - F(t)$$
                                      This function is crucial in survival analysis as it directly relates to the survival experience of subjects over time.
                                      ')
                                   ))
                               ),
                               tabPanel(
                                 "Relationship Between PDF, CDF, and Survival Function",
                                 fluidRow(
                                   class = "centered-tabBox",
                                   box(
                                     width = 10,
                                     HTML('
                                      The interplay between the PDF, CDF, and survival function is foundational in survival analysis: $$f(t) = -\\frac{dS(t)}{dt}$$
                                      Given \\(F(t) = 1 - S(t)\\), the PDF can also be expressed in terms of the survival function. These relationships allow us to understand the distribution of survival times and the likelihood of events occurring within specified time frames.
                                      ')
                                   ))
                               )
                             ),
                             div(class = "white-space-mini"),
                             h3(
                               class = "left-h4",
                               tagList(
                                 "Conditions for a Valid Survival Model",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             fluidRow(
                               class = "centered-tabBox",
                               tabsetPanel(
                                 tabPanel(
                                   'Non-Negativity',
                                   div(class = "white-space-mini"),
                                   box(
                                     width = 10,
                                     'The survival time \\(T\\) must be non-negative: $$(T \\geq 0)$$'
                                   )
                                 ),
                                 tabPanel(
                                   'Proper Distribution',
                                   div(class = "white-space-mini"),
                                   box(
                                     width = 10,
                                     'The CDF \\(F(t)\\) must approach 1 as \\(t\\) approaches infinity, ensuring the event eventually occurs.'
                                   )
                                 ),
                                 tabPanel(
                                   'Monotonicity',
                                   div(class = "white-space-mini"),
                                   box(
                                     width = 10,
                                     "The CDF \\(F(t)\\) must be non-decreasing, and the survival function \\(S(t)\\) must be non-increasing, reflecting the passage of time and the increasing probability of the event's occurrence."
                                   )
                                 )
                               )
                             ),
                             div(class = "white-space-mini"),
                             h3(
                               class = "left-h4",
                               tagList(
                                 "Applications in Survival Analysis",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             fluidRow(
                               class = "centered-tabBox",
                               box(
                                 width = 10,
                                 HTML('
                                      Survival distributions and their associated functions are utilized extensively in various applications, such as: <br>
                                      &emsp; - <b>Medical Research:</b> Modeling patient survival times post-treatment. <br>
                                      &emsp; - <b>Engineering:</b> Predicting the failure times of mechanical systems. <br>
                                      &emsp; - <b>Insurance:</b> Estimating the time to claim occurrences. <br>
                                      Understanding and applying these concepts allows researchers and practitioners to make informed decisions based on the probabilistic behavior of time-to-event data.
                                      ')
                               )
                             )
                           ),
                           tabPanel(
                             '§1.2: survival distributions',
                             div(class = "white-space-mini"),
                             h3(
                               class = "left-h4",
                               tagList(
                                 "Survival Distributions Infographic",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             div(class = "white-space-mini"),
                             fluidRow(
                               withMathJax(),
                               useShinyFeedback(),
                               sidebarLayout(
                                 sidebarPanel(
                                   width = 3,
                                   withMathJax(),
                                   sliderInput('x', 'life age (\\(x\\))', 0, 130, 65),
                                   sliderInput('t', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                                   sliderInput('m', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                                   selectInput("frac_assump","fractional age assumption",choices = c("UDD",'Constant'),selected = 'UDD'),
                                   selectInput("mort_law","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                                   conditionalPanel(condition = "input.mort_law == 'Gompertz'",
                                                    numericInput(inputId = 'B1',label = '\\(B\\)',value = 0.00008),
                                                    numericInput(inputId = 'C1',label = '\\(C\\)',value = 1.07)),
                                   conditionalPanel(condition = "input.mort_law == 'Makeham'",
                                                    numericInput(inputId = 'A',label = '\\(A\\)',value = 0.00022),
                                                    numericInput(inputId = 'B2',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                                    numericInput(inputId = 'C2',label = '\\(C\\)',value = 1.124)),
                                   conditionalPanel(condition = "input.mort_law == 'Moivre'",
                                                    sliderInput('alpha','\\(\\alpha\\)',0,0.99,0.01)),
                                   actionButton(inputId='button',label='plot'),
                                   htmlOutput("validation_message")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       'Whole Age Survival Distributions',
                                       withSpinner(plotOutput('output_survivalDist_plot'))
                                     ), 
                                     tabPanel(
                                       'Fractional Age Survival Distributions',
                                       withSpinner(plotOutput('output_survivalDist_plot2'))
                                     )
                                   )
                                   ,
                                   div(class = "white-space-mini"),
                                   h4(
                                     class = "left-h4",
                                     tagList(
                                       "Mortality Asuumptions",
                                       icon("section",class = "pull-left")
                                     ),
                                     class = "heading-with-icon"
                                   ),
                                   tabsetPanel(
                                     tabPanel('Fractional Age Assumptions',
                                              tabsetPanel(
                                                tabPanel('Uniform Distribution of Deaths (UDD)',
                                                         'The UDD assumption posits that deaths are uniformly distributed throughout the year. This approach is straightforward, assuming an even probability of death at any point within the year. It’s particularly useful for simplifying calculations in life insurance and pension valuations.'
                                                         ),
                                                tabPanel('Constant Force of Mortality',
                                                         'The Constant Force of Mortality assumption suggests that the mortality rate remains constant over short intervals. This implies that the probability of death is the same at any point in time within the year, leading to an exponential distribution of survival times. This assumption is valuable for more detailed actuarial models where continuous mortality rates are required.'
                                                )
                                              )),
                                     tabPanel('Laws of Mortality',
                                              tabsetPanel(
                                                tabPanel("Moivre's Law",
                                                         HTML("Moivre's Law, introduced by Abraham de Moivre, provides a simplistic yet insightful model of mortality. It suggests a linear relationship between age and mortality rates, which can be expressed as follows when incorporating a general constant \\(\\alpha\\). The formula is: $$\\mu_x = \\frac{\\alpha}{\\omega-x}$$
                                                              where \\(\\mu_x\\). This linear decline model is a foundational concept in actuarial science, providing a baseline that can be refined with more complex models. Moivre's Law is particularly useful for its simplicity and ease of calculation, making it a valuable tool in the initial stages of mortality analysis.
                                                              ")
                                                  
                                                ),
                                                tabPanel("Gompertz's Law",
                                                         HTML("Gompertz's Law posits that the force of mortality increases exponentially with age. The formula is: $$\\mu_x = Bc^x$$
                                                            where \\(\\mu_x\\) is the force of mortality at age \\(x\\),  \\(B\\) is a constant, and \\(c\\) is the rate of increase.  
                                                            This exponential increase captures the observed pattern of mortality rates rising sharply with advancing age, making it a widely used and significant model in actuarial science. 
                                                            ")
                                                ),
                                                tabPanel("Makeham's Law",
                                                         "Makeham's Law extends Gompertz’s Law by adding a constant to account for age-independent mortality risks. The formula is: $$\\mu_x = A+Bc^x$$
                                                         where \\(\\mu_x\\) is the force of mortality at age \\(x\\),  \\(A\\) is the age-independent component,  \\(B\\) is a constant, and \\(c\\) is the rate of increase. This law is beneficial for incorporating both age-related and constant mortality risks, providing a more comprehensive mortality model.
                                                         "
                                                )))
                                     )
                                 )
                               )
                             )
                           )
                         )
                ),
                #---------------------------------------------survival models: force of mortality tab---------------------------------------------
                tabPanel(
                  '§2: force of mortality',
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      withMathJax(),
                      sliderInput('x2', 'life age (\\(x\\))', 0, 130, 65),
                      sliderInput('t2', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                      sliderInput('m2', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                      selectInput("frac_assump2","fractional age assumption",choices = c("UDD",'Constant'),selected = 'UDD'),
                      selectInput("mort_law2","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                      conditionalPanel(condition = "input.mort_law2 == 'Gompertz'",
                                       numericInput(inputId = 'B12',label = '\\(B\\)',value = 0.00008),
                                       numericInput(inputId = 'C12',label = '\\(C\\)',value = 1.07)),
                      conditionalPanel(condition = "input.mort_law2 == 'Makeham'",
                                       numericInput(inputId = 'A2',label = '\\(A\\)',value = 0.00022),
                                       numericInput(inputId = 'B22',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                       numericInput(inputId = 'C22',label = '\\(C\\)',value = 1.124)),
                      conditionalPanel(condition = "input.mort_law2 == 'Moivre'",
                                       sliderInput('alpha2','\\(\\alpha\\)',0,0.99,0.01)),
                      actionButton(inputId='button2',label='plot'),
                      htmlOutput("validation_message2")
                    ),
                    mainPanel(
                      withSpinner(plotOutput('output_fom_plot')),
                      div(class = "white-space"),
                      h4(
                        class = "left-h4",
                        tagList(
                          "Understanding the Force of Mortality",
                          icon("section",class = "pull-left")
                        ),
                        class = "heading-with-icon"
                      ),
                      div(class = "white-space-mini"),
                      tabsetPanel(
                        tabPanel(
                          'Introduction',
                          HTML('
                               The <b>Force of Mortality</b>, often denoted as <b>\\(\\mu_x\\)</b>, can be thought of as the rate at which individuals are expected to die at a particular age \\(x\\) conditional on having lived to age \\(x\\). <br>
                               This conditionality is key, as it acknowledges the survival of an individual up to age \\(x\\) and then assesses the immediate risk of death.
                               ')
                        ),
                        tabPanel(
                          'Mathematical Representation',
                          HTML('
                               To formalize this, consider the following formula that encapsulates the relationship between survival probabilities and the force of mortality: $$_tp_x \\cdot q_{(x+t)}$$
                               where: <br>
                               &emsp; - <b>\\(_tp_x\\)</b> is the probability that an individual aged \\(x\\) survives to age \\(x+t\\). <br>
                               &emsp; - <b>\\(q_{(x+t)}\\)</b> is the probability that an individual who has reached age \\(x+t\\) will die within the next year. <br> <br>
                               The force of mortality <b>\\(\\mu_x\\)</b> is essentially the limit of the mortality rate as the interval approaches zero. Mathematically, it is defined as: $$\\mu_x = \\lim_{\\Delta t \\to 0} \\frac{q_{(x+\\Delta t)}}{\\Delta t}$$
                               This definition captures the instantaneous risk of death at age \\(x\\), reflecting how the mortality rate changes continuously with age.')
                        ),
                        tabPanel(
                          'Practical Implications',
                          HTML('
                               <b>1</b>. Understanding the force of mortality allows actuaries to model and predict mortality patterns with greater precision. It provides a dynamic view of mortality risk that can adapt to varying ages and conditions, making it a powerful tool for applications such as life insurance, pension planning, and demographic studies. <br>
                               <b>2</b>. By analyzing the force of mortality, actuaries can better assess the probability of survival and death over different age intervals. This, in turn, informs the pricing of life insurance products, the valuation of pension liabilities, and the management of long-term financial risks.')
                        )
                      )
                    )
                  )      
                ),
                #---------------------------------------------survival models: curtate future life-time tab---------------------------------------------
                tabPanel('§3: curtate future life-time',
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             withMathJax(),
                             sliderInput('x3', 'life age (\\(x\\))', 0, 130, 65),
                             selectInput("mort_law3","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                             conditionalPanel(condition = "input.mort_law3 == 'Gompertz'",
                                              numericInput(inputId = 'B13',label = '\\(B\\)',value = 0.00008),
                                              numericInput(inputId = 'C13',label = '\\(C\\)',value = 1.07)),
                             conditionalPanel(condition = "input.mort_law3 == 'Makeham'",
                                              numericInput(inputId = 'A3',label = '\\(A\\)',value = 0.00022),
                                              numericInput(inputId = 'B23',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                              numericInput(inputId = 'C23',label = '\\(C\\)',value = 1.124)),
                             conditionalPanel(condition = "input.mort_law3 == 'Moivre'",
                                              sliderInput('alpha3','\\(\\alpha\\)',0,0.99,0.01)),
                             actionButton(inputId='button3',label='plot')
                           ),
                           mainPanel(
                             div(class = "white-space"),
                             h4(
                               class = "left-h4",
                               tagList(
                                 "Complete Future Lifetime (\\(e_x\\) || \\({e^{o}}_x\\))",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             div(class = "white-space"),
                             fluidRow(
                               width=10,
                               class='centered-tabBox',
                               valueBox(
                                 htmlOutput("output_completeLife_plot"), "\\(e_x = \\sum_{i=1}^{\\omega}{_ip_x} \\)", icon = icon("calculator"),
                                 color = "blue"
                               ),
                               valueBox(
                                 htmlOutput("output_completeLife_plot2"), "\\({e^{o}}_x = \\int_{i=1}^{\\omega}{_ip_x} \\approx e_x + \\frac{1}{2} \\)", icon = icon("calculator"),
                                 color = "blue"
                               )
                             ),
                             div(class = "white-space"),
                             h4(
                               class = "left-h4",
                               tagList(
                                 "Understanding Curtate Future Lifetime",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             div(class = "white-space-mini"),
                             tabsetPanel(
                               tabPanel(
                                 HTML("<b>\\(K_x\\)</b>: The Curtate Future Lifetime"),
                                 HTML('
                                      <b>\\(K_x\\)</b> represents the curtate future lifetime of an individual aged \\(x\\). It is defined as the number of complete years lived by an individual from age \\(x\\) until death. <br>
                                      Mathematically, if <b>\\(T_x\\)</b> is the future lifetime of an individual aged \\(x\\), then $$K_x = \\lfloor T_x \\rfloor$$
                                      &emsp; - where, \\(\\lfloor T_x \\rfloor\\) denotes the greatest integer less than or equal to <b>\\(T_x\\)</b>.<br><br>
                                      Essentially, <b>\\(K_x\\)</b> truncates the continuous future lifetime <b>\\(T_x\\)</b> to a whole number of years. This discrete approach to measuring lifetime is particularly useful in practical actuarial calculations, such as in the determination of life insurance benefits and annuity payments, where payments are typically made at the end of each year.')
                               ),
                               tabPanel(
                                 HTML("<b>\\(e_x\\)</b>: The Complete Expectation of Life"),
                                 HTML('
                                      <b>\\(e_x\\)</b>, known as the complete expectation of life, is the expected remaining lifetime of an individual aged  \\(x\\). It is given by the sum of the expected value of \\(K_x\\), reflecting the average number of years an individual is expected to live beyond age \\(x\\). Mathematically: $$e_x = \\sum_{k = 0}^{\\omega}{k \\cdot P(K_{x} = k)}$$
                                      &emsp; - where, \\(P(K_{x} = k)\\) is the probability that the curtate future lifetime \\(K_x\\) equals \\(k\\). <br><br>
                                      This summation takes into account all possible ages at death and their respective probabilities, providing a comprehensive measure of life expectancy.
                                      ')
                               ),
                               tabPanel(
                                 HTML("<b>\\({e^{o}}_x\\)</b>: The Curtate Expectation of Life"),
                                 HTML('
                                      While <b>\\(e_x\\)</b> is the complete expectation of life, <b>\\({e^{o}}_x\\)</b> is the curtate expectation of life. The curtate expectation of life <b>\\({e^{o}}_x\\)</b> is the expected value of the continuous future lifetime \\(T_x\\), representing the average number of years an individual aged \\(x\\) is expected to live, considering the full span of fractional years. Mathematically:  $${e^{o}}_x = \\int_{i=1}^{\\omega}{_ip_x}$$ <br>
                                      &emsp; - where, <b> \\(\\int_{i=1}^{\\omega}{_ip_x}\\)</b> is the integral of the survival function \\(S_{x}(i)\\) over the range of future periods. <br><br>
                                      This integral approximates the sum of the expected values of \\(K_x\\) over the range of future periods, providing a continuous measure of life expectancy.
                                      ')
                               ),
                               tabPanel(
                                 HTML("Comparison: <b>\\(e_x\\)</b> vs <b>\\({e^{o}}_x\\)</b>"),
                                 HTML('
                                      The key difference between <b>\\(e_x\\)</b> and <b>\\({e^{o}}_x\\)</b> lies in their treatment of fractional years: <br><br>
                                      &emsp; - <b>\\(e_x\\)</b>: Focuses on the number of complete years lived, providing a discrete measure of life expectancy.<br>
                                      &emsp; - <b>\\({e^{o}}_x\\)</b>: Considers the entire remaining lifespan, including fractional years, offering a more refined and continuous estimate. <br> <br>
                                      The distinction between these measures is crucial for actuaries and demographers. <b>\\(e_x\\)</b> simplifies calculations by focusing on complete years, which is often useful in financial and insurance contexts where benefits are paid annually. <br> <br>
                                      In contrast, <b>\\({e^{o}}_x\\)</b> provides a more accurate reflection of life expectancy by incorporating fractional years, which is important for more precise actuarial assessments and demographic studies.
                                      ')
                               )
                             )
                             
                             
                           )
                         )
                ),
                #---------------------------------------------survival models: life tables and selection tab---------------------------------------------
                tabPanel('§4: life tables and selection',
                         tabsetPanel(
                           #---------------------------------------------life tables and selection: life tables tab---------------------------------------------
                           tabPanel(
                             '§4.1: life tables',
                             sidebarLayout(
                               sidebarPanel(
                                 width = 3,
                                 withMathJax(),
                                 sliderInput('x4', 'life age (\\(x\\))', 0, 130, 65),
                                 sliderInput('t4', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                                 sliderInput('m4', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                                 sliderInput('radix', 'radix (\\(l_{x_0}\\))', 1, 100000, 50000),
                                 selectInput("mort_law4","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                                 conditionalPanel(condition = "input.mort_law4 == 'Gompertz'",
                                                  numericInput(inputId = 'B14',label = '\\(B\\)',value = 0.00008),
                                                  numericInput(inputId = 'C14',label = '\\(C\\)',value = 1.07)),
                                 conditionalPanel(condition = "input.mort_law4 == 'Makeham'",
                                                  numericInput(inputId = 'A4',label = '\\(A\\)',value = 0.00022),
                                                  numericInput(inputId = 'B24',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                                  numericInput(inputId = 'C24',label = '\\(C\\)',value = 1.124)),
                                 conditionalPanel(condition = "input.mort_law4 == 'Moivre'",
                                                  sliderInput('alpha4','\\(\\alpha\\)',0,0.99,0.01)),
                                 actionButton(inputId='button4',label='plot'),
                                 htmlOutput("validation_message4")
                               ),
                               
                               mainPanel(
                                 div(class = "white-space"),
                                 h4(
                                   class = "left-h4",
                                   tagList(
                                     "Ultimate Life-Table",
                                     icon("section",class = "pull-left")
                                   ),
                                   class = "heading-with-icon"
                                 ),
                                 withSpinner(plotOutput('output_lifeTable_plot')),
                                 div(class = "white-space"),
                                 h4(
                                   class = "left-h4",
                                   tagList(
                                     "Understanding Life-Tables",
                                     icon("section",class = "pull-left")
                                   ),
                                   class = "heading-with-icon"
                                 ),
                                 div(class = "white-space-mini"),
                                 fluidRow(
                                   width=10,
                                   class='centered-tabBox',
                                   tabsetPanel(
                                     tabPanel(
                                       'Importance and Role of Life-Tables',
                                       HTML("
                                            Life-tables summarize the mortality experience of a population, presenting key information such as the probability of surviving to a certain age and the expected number of remaining years of life for individuals at different ages. This data is vital for actuaries to assess and price life insurance policies, annuities, and pension plans. By leveraging life-tables, actuaries can estimate the likelihood of policyholders' survival over various time horizons, ensuring the financial stability of insurance products and retirement schemes.
                                            ")
                                     ),
                                     tabPanel(
                                       'Ultimate and Select Life-Tables',
                                       HTML("
                                            Life-tables can be categorized into two main types: ultimate and select life-tables. <br><br>
                                            &emsp; - <b>Ultimate Life-Tables:</b> These tables provide mortality rates that apply to the general population without considering the duration since underwriting or selection. They are based on the long-term mortality experience and are used for individuals who have been exposed to the risk of death for a significant period. <br><br>
                                            &emsp; - <b>Select Life-Tables:</b> These tables take into account the duration since underwriting or selection, reflecting the mortality rates of individuals who have recently undergone underwriting. Select life-tables are particularly useful in the early years after policy issuance when the effect of underwriting selection is most pronounced. 
                                            ")
                                     ),
                                     tabPanel(
                                       'Survival Probabilities and Life-Table Values',
                                       HTML("
                                            Survival probabilities and life-table values are intricately linked. The probability that an individual aged \\(x\\) will survive for \\(t\\) more years, denoted as \\(_tp_x\\), and the probability that an individual aged \\(x\\) will die within \\(t\\) years, denoted as \\(_tq_x\\) can be expressed in terms of life-table values \\(l_x\\) (the number of survivors at age \\(x\\)).
                                            $$_tp_x = \\frac{l_{x+t}}{l_x}$$
                                            $$_tq_x = 1 - {_t}p_x = \\frac{l_x - l_{x+t}}{l_x}$$
                                            These relationships highlight the direct connection between survival probabilities and the decrement functions in life-tables. <br><br>
                                            Life-table values can be computed recursively, which is crucial for constructing and updating life-tables. Starting from a given initial number of individuals, \\(l_0\\), the number of survivors at each subsequent age can be calculated using the mortality rate\\(q_x\\) (the probability of dying between ages \\(x\\) and \\(x+1\\):
                                            $$l_{x+1} = l_x \\cdot (1 - q_x)$$
                                            By applying this recursive formula, actuaries can systematically determine the number of survivors at each age, building a comprehensive life-table.
                                            ")
                                     )
                                   )
                                 )
                               )
                             )
                           ),
                           #---------------------------------------------life tables and selection: selection tab---------------------------------------------
                           tabPanel(
                             '§4.2: selection',
                             sidebarLayout(
                               sidebarPanel(
                                 width = 3,
                                 withMathJax(),
                                 sliderInput('x5', 'life age (\\(x\\))', 0, 130, 65),
                                 sliderInput('t5', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                                 sliderInput('m5', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                                 sliderInput('d', 'length of select period (\\(d\\))', 1, 130, 2),
                                 selectInput("mort_law5","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                                 conditionalPanel(condition = "input.mort_law5 == 'Gompertz'",
                                                  numericInput(inputId = 'B15',label = '\\(B\\)',value = 0.00008),
                                                  numericInput(inputId = 'C15',label = '\\(C\\)',value = 1.07)),
                                 conditionalPanel(condition = "input.mort_law5 == 'Makeham'",
                                                  numericInput(inputId = 'A5',label = '\\(A\\)',value = 0.00022),
                                                  numericInput(inputId = 'B25',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                                  numericInput(inputId = 'C25',label = '\\(C\\)',value = 1.124)),
                                 conditionalPanel(condition = "input.mort_law5 == 'Moivre'",
                                                  sliderInput('alpha5','\\(\\alpha\\)',0,0.99,0.01)),
                                 actionButton(inputId='button5',label='plot'),
                                 htmlOutput("validation_message5")
                               ),
                               
                               mainPanel(
                                 div(class = "white-space"),
                                 h4(
                                   class = "left-h4",
                                   tagList(
                                     HTML("Comparison of \\(_tp_x\\) and \\(_t{p_{[x]}}\\)"),
                                     icon("section",class = "pull-left")
                                   ),
                                   class = "heading-with-icon"
                                 ),
                                 withSpinner(plotOutput('output_selection_plot')),
                                 div(class = "white-space-mini"),
                                 useShinyjs(),  # Initialize shinyjs
                                 fluidRow(
                                   tags$div(class = "custom-tabbox",tags$div(actionButton("infoButton", "", icon = icon("comment-dots")))),
                                   hidden(div(id = "selectionPanel", 
                                              HTML("• What are the Impacts of Selection on Survival Probabilities?"),
                                              br(),
                                              HTML("&emsp; - <b> Higher Survival Probabilities in Select Cohorts </b>: Persons in select cohorts typically exhibit higher survival probabilities compared to their ultimate counterparts. This trend is evident in the plot where the red points (select survival probabilities) consistently lie above the blue points (ultimate survival probabilities) across most of the future periods. The selection process, which involves underwriting and risk assessment, identifies healthier individuals, resulting in lower initial mortality rates for these select cohorts."),
                                              br(),
                                              HTML("&emsp; - <b> Increasing Impact Over Time </b>: The impact of selection on survival probabilities becomes more pronounced the further out the select period extends. As shown in the plot, the disparity between select and ultimate survival probabilities widens initially, demonstrating that individuals in the select cohort benefit from their healthier status over a significant period."),
                                              br(),
                                              HTML("&emsp; - <b> Limitations of Long Select Periods </b>: While the plot effectively illustrates the impact of selection, it's important to note that examining extremely long select periods may not be practical in real-world scenarios. Such long select periods are seldom used in practice due to the diminishing impact of selection over time and the increasing convergence of select and ultimate mortality rates. However, the plot serves as a vivid representation of how selection influences survival probabilities, highlighting the importance of accurate mortality modeling in actuarial practices.")
                                   ))),
                                 div(class = "white-space"),
                                 h4(
                                   class = "left-h4",
                                   tagList(
                                     "Understanding Selection",
                                     icon("section",class = "pull-left")
                                   ),
                                   class = "heading-with-icon"
                                 ),
                                 div(class = "white-space-mini"),
                                 tabsetPanel(
                                   tabPanel(
                                     'Importance and Role of Selection',
                                     HTML("
                                            Selection plays a vital role in actuarial practices by ensuring that individuals with higher mortality risks are appropriately charged higher premiums or offered different insurance terms. This stratification helps manage risk and maintain the financial health of insurance companies. Selection allows for more accurate pricing of insurance products and better prediction of future claims, thereby reducing adverse selection and ensuring equitable treatment of policyholders.
                                            ")
                                   ),
                                   tabPanel(
                                     'Adjusting the Force of Mortality for Select Lives',
                                     HTML("
                                            The force of mortality, \\(\\mu\\), can be adjusted to accommodate select lives using a specific formula. The force of mortality for select lives, \\(\\mu_{[x]+s}\\), can be modified as follows: $$\\mu_{[x]+s} = {0.9}^{d-s} \\cdot \\mu_{x+d}$$
                                            where: <br>
                                            &emsp; - <b>s</b>: is the select period time index. <br><br>
                                            &emsp; - <b>d</b>: is the select period limit, such that \\(0 < s < d\\). <br><br>
                                            This formula reflects the reduction in mortality rates for individuals who have undergone underwriting, with the select effect diminishing over time.
                                            ")
                                   ),
                                   tabPanel(
                                     HTML('Computing Survival Probabilities and Life-Table Values with \\(\\mu_{[x]+s}\\)'),
                                     HTML("
                                            With the updated force of mortality \\(\\mu_{[x]+s}\\), other survival probabilities and life-table values can be computed to reflect the select mortality rates. The survival probability for a select individual can be expressed as: $$_tp_{[x]+s} = e^{-\\int_{0}^{t}{\\mu_{[x]+s+u}}du}$$
                                            where: <br>
                                            &emsp; - <b>\\(\\mu_{[x]+s+u}\\)</b>: is the force of mortality for select lives at age \\(x+s+u\\), such that \\((s+u) < d\\) <br><br>
                                            The corresponding life-table value, \\(l_{[x]+s}\\), can then be calculated recursively from the ultimate period using: $$l_{[x]+s} = \\frac{l_{x+d}}{_{d-s}p_{[x]+s}}$$
                                            These adjusted values provide a more accurate depiction of the survival probabilities and expected lifetimes for select individuals, enhancing the precision of actuarial assessments and product pricing.
                                            ")
                                   )
                                 )
                               )
                             )
                           )
                         )
                ),
                #---------------------------------------------survival models: notes tab---------------------------------------------
                tabPanel('§5: notes',
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             withMathJax(),
                             sliderInput('x6', 'life age (\\(x\\))', 0, 130, c(40,90)),
                             sliderInput('d6', 'length of select period (\\(d\\))', 1, 130, 2),
                             sliderInput('radix6', 'radix (\\(l_{x_0}\\))', 1, 100000, 50000),
                             selectInput("mort_law6","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                             conditionalPanel(condition = "input.mort_law6 == 'Gompertz'",
                                              numericInput(inputId = 'B16',label = '\\(B\\)',value = 0.00008),
                                              numericInput(inputId = 'C16',label = '\\(C\\)',value = 1.07)),
                             conditionalPanel(condition = "input.mort_law6 == 'Makeham'",
                                              numericInput(inputId = 'A6',label = '\\(A\\)',value = 0.00022),
                                              numericInput(inputId = 'B26',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                              numericInput(inputId = 'C26',label = '\\(C\\)',value = 1.124)),
                             conditionalPanel(condition = "input.mort_law6 == 'Moivre'",
                                              sliderInput('alpha6','\\(\\alpha\\)',0,0.99,0.01)),
                             actionButton(inputId='button6',label='plot'),
                             htmlOutput("validation_message6")
                           ),
                           mainPanel(
                             div(class = "white-space"),
                             h4(
                               class = "left-h4",
                               tagList(
                                 HTML("Ultimate and Select Life-Table (\\(l_{x} \\; \\& \\;  l_{[x]}\\))"),
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             div(class = "white-space-mini"),
                             column(withSpinner(DTOutput('output_ultimate_plot')),style = "overflow-y: scroll;overflow-x: scroll;",width = 12),
                             div(class = "white-space"),
                             h4(
                               class = "left-h4",
                               tagList(
                                 "Notes on Life-Tables",
                                 icon("section",class = "pull-left")
                               ),
                               class = "heading-with-icon"
                             ),
                             tabsetPanel(
                               tabPanel(
                                 'Introduction to Parametric and Non-Parametric Methods',
                                 HTML("
                                      Before delving into the importance of parametric methods in computing life-table values, it's essential to understand the difference between parametric and non-parametric methods. <br><br>
                                      &emsp; - <b>Parametric Methods:</b> Parametric methods involve assuming a specific statistical distribution or model for the data and then estimating the parameters of that model. These methods rely on predefined mathematical functions to describe the underlying patterns in the data. <br><br>
                                      &emsp;&emsp; - <b>Example:</b> <br>
                                      &emsp;&emsp;&emsp; - Using a <b>Gompertz</b> or <b>Makeham</b> model to describe mortality rates. <br>
                                      &emsp;&emsp; - <b>Key Characteristics:</b><br>
                                      &emsp;&emsp;&emsp; - <b>Assumptions:</b> Require assumptions about the distribution or form of the data.<br>
                                      &emsp;&emsp;&emsp; - <b>Parameters:</b> Estimate a finite number of parameters that define the model.<br>
                                      &emsp;&emsp;&emsp; - <b>Efficiency:</b> Can be very efficient with large datasets, providing smooth and stable estimates.<br><br>
                                      &emsp; - <b>Non-Parametric Methods:</b> Non-parametric methods do not assume any specific statistical distribution. Instead, they use the data itself to estimate the function or distribution.<br><br>
                                      &emsp;&emsp; - <b>Example:</b> <br>
                                      &emsp;&emsp;&emsp; - <b>Kaplan-Meier</b> estimator for survival probabilities. <br>
                                      &emsp;&emsp; - <b>Key Characteristics:</b><br>
                                      &emsp;&emsp;&emsp; - <b>Flexibility:</b> Do not require assumptions about the data's distribution.<br>
                                      &emsp;&emsp;&emsp; - <b>Data-Driven:</b> Use the empirical distribution of the data to make estimates.<br>
                                      &emsp;&emsp;&emsp; - <b>Adaptability:</b> Can adapt to different shapes and patterns in the data without being constrained by a predefined model.<br>
                                      ")
                               ),
                               tabPanel(
                                 "Importance of Parametric Methods",
                                 HTML("
                                      Parametric methods play a crucial role in actuarial science, especially in the computation of life-table values, which are vital for the pricing and reserving of life insurance policies, annuities, and pension plans.
                                      "
                                 ),
                                 div(class = "white-space-mini"),
                                 tabsetPanel(
                                   tabPanel(
                                     "Simplification and Estimation",
                                     HTML("
                                          Parametric methods simplify the process of estimating survival probabilities and other life-table values by using mathematical formulas. This reduces the need for extensive empirical data and makes it feasible to compute life-table values even when detailed mortality data is not available.
                                          ")
                                   ),
                                   tabPanel(
                                     "Flexibility and Application",
                                     HTML("
                                          hese methods provide flexibility in modeling various mortality patterns and can be adjusted to reflect different demographic and health characteristics of populations. Actuaries can tailor mortality assumptions to specific groups or insurance products.
                                          ")
                                   ),
                                   tabPanel(
                                     "Consistency and Smoothness",
                                     HTML("
                                          By using parametric functions, life-table values can be smoothed and made consistent, eliminating irregularities and anomalies that might be present in raw mortality data. This results in more reliable and stable mortality estimates.
                                          ")
                                   ),
                                   tabPanel(
                                     "Predictive Power",
                                     HTML("
                                          Parametric models, when fitted correctly, can provide strong predictive power, allowing actuaries to make informed projections about future mortality trends and patterns.
                                          ")
                                   )
                                 )
                               ),
                               tabPanel(
                                 "Real-Life Tables vs. Parametric Methods",
                                 tabsetPanel(
                                   tabPanel(
                                     "Real-Life Tables",
                                     HTML("
                                        Real-life tables, or empirical life-tables, are constructed from observed mortality data and reflect the actual survival experiences of a specific population. <br><br>
                                        <b>Advantages:</b><br>
                                        &emsp; - Highly accurate and reflective of actual mortality experiences. <br>
                                        &emsp; - Capture real-time changes and trends in mortality rates. <br><br>
                                        <b>Limitations:</b><br>
                                        &emsp; - Require extensive and detailed mortality data, which may not always be available. <br>
                                        &emsp; - Can be affected by irregularities and anomalies in the data, leading to less smooth mortality curves.
                                        ")
                                   ),
                                   tabPanel(
                                     "Parametric Methods",
                                     HTML("
                                        Parametric methods, as discussed, involve fitting mathematical models to mortality data. <br><br>
                                        <b>Advantages:</b><br>
                                        &emsp; - Require less detailed data compared to real-life tables, making them more practical in situations with limited mortality information. <br>
                                        &emsp; - Provide smoother and more stable life-table values by eliminating data irregularities. <br>
                                        &emsp; - Offer flexibility in modeling different mortality scenarios and can be adjusted for specific populations. <br><br>
                                        <b>Limitations:</b><br>
                                        &emsp; - Depend on the chosen mathematical model and its assumptions. If the model does not adequately capture the true mortality pattern, the resulting life-table values may be inaccurate. <br>
                                        &emsp; - The process of fitting parametric functions to mortality data can introduce estimation errors. <br>
                                        &emsp; - While providing general trends, parametric methods may fail to capture granular details present in empirical data.
                                        ")
                                   )
                                 )
                               )
                             )
                             )
                           )
                         )
                ))),
  #---------------------------------------------life-contingent products tab---------------------------------------------
    tabItem(tabName = "product",
            h2(
              class = "centralized-h2",
              tagList(
                "Life-Contingent Products",
                icon("cart-shopping",class = "pull-center")
              ),
              class = "heading-with-icon"
            ),
            tabsetPanel(
              #---------------------------------------------life-contingent products tab: life insurance products---------------------------------------------
              tabPanel(
                '§1: life insurance contracts',
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    withMathJax(),
                    sliderInput('x7', 'life age (\\(x\\))', 0, 130, 65),
                    sliderInput('t7', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                    sliderInput('m7', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                    sliderInput('i7', 'interest rate (\\(i\\))', 0, 1, 0.05),
                    sliderInput('n7', 'length of contract period (\\(n\\))', 1, 130, 5),
                    sliderInput('u7', 'length of deferral period (\\(u\\))', 0, 130, 0),
                    selectInput("mort_law7","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                    conditionalPanel(condition = "input.mort_law7 == 'Gompertz'",
                                     numericInput(inputId = 'B17',label = '\\(B\\)',value = 0.00008),
                                     numericInput(inputId = 'C17',label = '\\(C\\)',value = 1.07)),
                    conditionalPanel(condition = "input.mort_law7 == 'Makeham'",
                                     numericInput(inputId = 'A7',label = '\\(A\\)',value = 0.00022),
                                     numericInput(inputId = 'B27',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                     numericInput(inputId = 'C27',label = '\\(C\\)',value = 1.124)),
                    conditionalPanel(condition = "input.mort_law7 == 'Moivre'",
                                     sliderInput('alpha7','\\(\\alpha\\)',0,0.99,0.01)),
                    actionButton(inputId='button7',label='plot'),
                    htmlOutput("validation_message7")
                  ),
                  mainPanel(
                    div(class = "white-space"),
                    h4(
                      class = "left-h4",
                      tagList(
                        "Life Insurance Contract Infographics",
                        icon("section",class = "pull-left")
                      ),
                      class = "heading-with-icon"
                    ),
                    tabsetPanel(
                      tabPanel(
                        'Whole Life Insurance',
                        withSpinner(plotOutput('output_wholeInsurance_plot'))
                      ),
                      tabPanel(
                        'Term Life Insurance',
                        withSpinner(plotOutput('output_termInsurance_plot'))
                      ),
                      tabPanel(
                        'Endowment Insurance',
                        withSpinner(plotOutput('output_endowmentInsurance_plot'))
                      )
                    ),
                    div(class = "white-space"),
                    h4(
                      class = "left-h4",
                      tagList(
                        "Life Insurance Contract Values",
                        icon("section",class = "pull-left")
                      ),
                      class = "heading-with-icon"
                    ),
                    fluidRow(
                      width=10,
                      withSpinner(uiOutput('insuranceProducts'))
                    )
                    )
                )
              ),
              #---------------------------------------------life-contingent products tab: annuity contracts---------------------------------------------
              tabPanel(
                '§2: annuity contracts',
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    withMathJax(),
                    sliderInput('x8', 'life age (\\(x\\))', 0, 130, 65),
                    sliderInput('t8', 'range of future periods (\\(t_{min}\\) , \\(t_{max}\\))', 0, 130, c(35,95)),
                    sliderInput('m8', 'compounding frequency per year (\\(m\\))', 1, 365, 1),
                    sliderInput('i8', 'interest rate (\\(i\\))', 0, 1, 0.05),
                    sliderInput('n8', 'length of contract period (\\(n\\))', 1, 130, 5),
                    sliderInput('u8', 'length of deferral period (\\(u\\))', 0, 130, 0),
                    selectInput("a_s8","annuity schedule",choices = c("Due","Immediate"),selected = 'Due'),
                    selectInput("mort_law8","underlying mortality law",choices = c("Makeham", "Gompertz", "Moivre"),selected = 'Makeham'),
                    conditionalPanel(condition = "input.mort_law8 == 'Gompertz'",
                                     numericInput(inputId = 'B18',label = '\\(B\\)',value = 0.00008),
                                     numericInput(inputId = 'C18',label = '\\(C\\)',value = 1.07)),
                    conditionalPanel(condition = "input.mort_law8 == 'Makeham'",
                                     numericInput(inputId = 'A8',label = '\\(A\\)',value = 0.00022),
                                     numericInput(inputId = 'B28',label = '\\(B\\)',value = 2.7 * 10^(-6)),
                                     numericInput(inputId = 'C28',label = '\\(C\\)',value = 1.124)),
                    conditionalPanel(condition = "input.mort_law8 == 'Moivre'",
                                     sliderInput('alpha8','\\(\\alpha\\)',0,0.99,0.01)),
                    actionButton(inputId='button8',label='plot'),
                    htmlOutput("validation_message8")
                  ),
                  mainPanel(
                    div(class = "white-space"),
                    h4(
                      class = "left-h4",
                      tagList(
                        "Annuity Contract Infographics",
                        icon("section",class = "pull-left")
                      ),
                      class = "heading-with-icon"
                    ),
                    tabsetPanel(
                      tabPanel(
                        'Whole Life Annuity',
                        plotOutput('output_wholeAnnuity_plot')
                      ),
                      tabPanel(
                        'Term Life Annuity',
                        plotOutput('output_termAnnuity_plot')
                      )
                    ),
                    div(class = "white-space"),
                    h4(
                      class = "left-h4",
                      tagList(
                        "Annuity Contract Values",
                        icon("section",class = "pull-left")
                      ),
                      class = "heading-with-icon"
                    ),
                    fluidRow(
                      width=10,
                      uiOutput('annuityProducts')
                    )
                  )
                )
              )
            )
    ),
  #---------------------------------------------policy values tab---------------------------------------------
    tabItem(tabName = "profit",
            h2(
              class = "centralized-h2",
              tagList(
                "Policy Values",
                icon("money-bill-trend-up",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
  #---------------------------------------------notes tab---------------------------------------------
    tabItem(tabName = "notes",
            h2(
              class = "centralized-h2",
              tagList(
                "Notes",
                icon("note-sticky",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    )
  )
)

#---------------------------------------------ui definition---------------------------------------------
ui <- dashboardPage(
    header,
    sidebar,
    body
)

#---------------------------------------------server definition---------------------------------------------
server <- function(input, output, session) { 
  #----------------------------------------------------t_max update----------------------------------------------------
  observeEvent(input$m, {
    # Dynamically update the range of the second sliderInput
    updateSliderInput(session, inputId = "t", min = 0, max = input$m*130,value = c((input$m*130/2)-(input$m*25),(input$m*130/2)+(input$m*25)))
  })
  
  observeEvent(input$m2, {
    # Dynamically update the range of the second sliderInput
    updateSliderInput(session, inputId = "t2", min = 0, max = input$m2*130,value = c((input$m2*130/2)-(input$m2*25),(input$m2*130/2)+(input$m2*25)))
  })
  
  observeEvent(input$m4, {
    # Dynamically update the range of the second sliderInput
    updateSliderInput(session, inputId = "t4", min = 0, max = input$m4*130,value = c((input$m4*130/2)-(input$m4*25),(input$m4*130/2)+(input$m4*25)))
  })
  
  observeEvent(input$m5, {
    # Dynamically update the range of the second sliderInput
    updateSliderInput(session, inputId = "t5", min = 0, max = input$m5*130,value = c((input$m5*130/2)-(input$m5*25),(input$m5*130/2)+(input$m5*25)))
  })
  
  observeEvent(input$m7, {
    # Dynamically update the range of the second sliderInput
    updateSliderInput(session, inputId = "t7", min = 0, max = input$m7*130,value = c((input$m7*130/2)-(input$m7*25),(input$m7*130/2)+(input$m7*25)))
  })
  
  #----------------------------------------------------briefHistory flowchart----------------------------------------------------
  briefHistory_nodes <- data.frame(
    id = 1:7,
    label = c("Ancient Origins","17th Century","18th Century","19th Century","20th Century","21st Century","Today"),
    description = c("• <b>Early Beginnings: The Foundations of Actuarial Science</b> <br>
                    &emsp; - The origins of actuarial science can be traced back to ancient civilizations where early forms of risk management and insurance practices were developed. These practices included pooling resources to mitigate the effects of risks like natural disasters and trade losses. <br>
                    &emsp; - <b>Early Risk Management:</b> Communities and merchants pooled resources to share the burden of losses from natural disasters and trade. <br>
                    &emsp; - <b>Ancient Practices:</b> The Code of Hammurabi and Roman burial societies are early examples of risk management and mutual aid practices.",
                    
                    "• <b>Development of Probability Theory</b> <br>
                    &emsp; - The foundation of actuarial science was significantly influenced by the development of probability theory. Mathematicians like Blaise Pascal and Pierre de Fermat laid the groundwork for understanding risk and uncertainty. <br>
                    &emsp; - <b>Mathematical Foundations:</b> Probability theory allowed for the systematic quantification of risk, essential for developing insurance and financial products. <br>
                    &emsp; - <b>Influential Mathematicians:</b> Blaise Pascal and Pierre de Fermat were pioneers in probability theory, which is crucial for actuarial calculations.",
                    
                    "• <b>Establishment of Life Tables</b> <br>
                    &emsp; - Edmund Halley created the first life table in 1693, providing a systematic way to estimate the probability of death at various ages. This was a crucial development for life insurance. <br>
                    &emsp; - <b>Mortality Analysis:</b> Life tables enabled insurers to estimate life expectancy and set premiums accurately. <br>
                    &emsp; - <b>Halley's Contribution:</b> Edmund Halley's life table was a major milestone, forming the basis for modern actuarial science.",
                    
                    "• <b>Formation of Actuarial Societies & Advancements in Mathematics</b> <br>
                    &emsp; - The Institute of Actuaries was established in London in 1848, and the Actuarial Society of America was founded in 1889. These organizations played a critical role in formalizing actuarial education and professional standards. <br>
                    &emsp; - <b>Professionalization:</b> The formation of actuarial societies formalized the profession, setting standards for practice and education. <br>
                    &emsp; - <b>Mathematical Techniques:</b> Actuaries began using more sophisticated mathematical techniques to model and predict risks, including compound interest theory.",
                    
                    "• <b>Expansion of Actuarial Science & Regulatory Developments</b> <br>
                    &emsp; - The actuarial profession expanded beyond life insurance to include pensions, health insurance, and general insurance. The adoption of computers and software allowed for more complex modeling and data analysis. <br>
                    &emsp; - <b>Broader Applications:</b> Actuaries applied their skills to new areas such as pensions and health insurance. <br>
                    &emsp; - <b>Technological Integration:</b> Computers and software facilitated advanced modeling and data analysis. <br>
                    &emsp; - <b>Regulatory Frameworks:</b> Increased regulation ensured the protection of policyholders and the stability of financial systems.",
                    
                    "• <b>Integration of Technology, Broader Applications, & Professional Development</b> <br>
                    &emsp; - The use of big data, machine learning, and predictive analytics has revolutionized the actuarial profession. Actuaries now use these tools to analyze vast amounts of data, improve risk assessment, and develop new insurance products. <br>
                    &emsp; - <b>Technological Advancements:</b> Big data and machine learning have enhanced risk assessment and product development. <br>
                    &emsp; - <b>Broader Fields:</b> Actuarial science now includes enterprise risk management (ERM) and financial risk management. <br>
                    &emsp; - <b>Continuous Education:</b> Organizations like the Society of Actuaries (SOA) and the Casualty Actuarial Society (CAS) ensure ongoing professional development and high standards through rigorous exams and certification.",
                    
                    "• <b>Current State of the Profession</b> <br>
                    &emsp; - Actuaries are essential in various industries, providing expertise in risk assessment, financial modeling, and strategic planning. They continue to adapt to emerging risks such as cyber threats, climate change, and evolving regulatory landscapes. <br>
                    &emsp; - <b>Modern Challenges:</b> Actuaries address new risks like cyber threats and climate change. <br>
                    &emsp; - <b>Strategic Role:</b> Actuaries play a crucial role in strategic decision-making and advising businesses on risk management."),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f6f0", "f21a", "f238", "f072","f5e7","f253"),
    icon.size = 40,
    icon.color = c("red", "green", "brown", "purple", "orange","blue","black")
  )
  
  briefHistory_edges <- data.frame(from = c(1, 2, 3, 4, 5, 6), to = c(2, 3, 4, 5, 6, 7))
  
  output$briefHistory_flowchart <- renderVisNetwork({
    visNetwork(briefHistory_nodes, briefHistory_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = FALSE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$briefHistory_nodeDescription <- renderText({
    req(input$current_node_id)
    briefHistory_selectedNode <- briefHistory_nodes[briefHistory_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", briefHistory_selectedNode$label, br(), briefHistory_selectedNode$description)
  }) 
  #----------------------------------------------------popups...actionButtons----------------------------------------------------
  observeEvent(input$infoButton, {
    toggle("infoPanel")  # Toggle the visibility of the infoPanel
  })
  
  observeEvent(input$infoButton, {
    toggle("insurablePanel")  # Toggle the visibility of the infoPanel
  })
  
  observeEvent(input$infoButton, {
    toggle("selectionPanel")  # Toggle the visibility of the infoPanel
  })
  #----------------------------------------------------underwriting flowchart----------------------------------------------------
  underwriting_nodes <- data.frame(
    id = 1:4,
    label = c("Early Underwriting",
              "Mid-20th Century",
              "Late 20th Century",
              "21st Century"),
    description = c("• <b> Early Underwriting: The Foundations of Actuarial Science </b> <br> 
                    &emsp; - In the early days, underwriting was a manual and labor-intensive process. Actuarial science was in its nascent stages, primarily focused on the development of mortality tables and basic statistical methods to assess risk. <br>
                    &emsp; - <b>Manual Processes and Personal Judgment:</b> Underwriters relied heavily on personal judgment and experience; thus decisions were made based on limited data, often derived from personal interviews, medical examinations, and historical records. <br>
                    &emsp; - <b>Early Actuarial Tools:</b> Actuaries used rudimentary mortality tables and simple statistical tools to assist in risk assessment; thus the accuracy of these methods was constrained by the limited data available and the manual nature of calculations.", 
                    "• <b> Mid-20th Century: The Rise of Statistical Methods and Early Technology </b> <br> 
                    &emsp; - The mid-20th century saw significant advancements in statistical methods and the initial integration of technology into underwriting processes. <br>
                    &emsp; - <b>Introduction of Statistical Methods:</b> Actuaries began to develop more sophisticated statistical models, incorporating factors such as age, gender, and occupation; thus improving the accuracy of risk assessments but still required significant manual effort.<br>
                    &emsp; - <b>Early Use of Technology:</b>The introduction of computers in the 1950s and 1960s allowed for more efficient data processing; and underwriting processes began to incorporate computerized systems to store and analyze data, albeit in a limited capacity.<br> 
                    &emsp; - <b>SOA Contributions:</b>The SOA played a pivotal role in advancing actuarial education and research, promoting the use of statistical methods and emerging technologies in the insurance industry.", 
                    "• <b> Late 20th Century: Digital Transformation and Predictive Analytics </b> <br> 
                    &emsp; - The late 20th century marked a transformative period for underwriting, driven by the digital revolution and the advent of predictive analytics. <br>
                    &emsp; - <b>Digital Transformation:</b> The widespread adoption of digital technologies revolutionized data collection and processing; allowing Insurers to implemente sophisticated software systems to automate underwriting processes, reducing the reliance on manual work.<br>
                    &emsp; - <b>Emergence of Predictive Analytics:</b>Predictive analytics became a cornerstone of modern underwriting, utilizing large datasets and advanced algorithms to forecast risk; resulting in Actuaries and Data Scientists collaborating to develop models that could predict future claims with greater precision.<br> 
                    &emsp; - <b>Integration of Medical Technology:</b>Advances in medical technology, such as electronic health records and genetic testing, provided underwriters with more comprehensive and accurate health data; thus allowing for more nuanced risk assessments based on an individual's medical history and genetic predispositions.", 
                    "• <b> 21st Century: Modern Underwriting and Continuous Innovation </b> <br> 
                    &emsp; - In the 21st century, underwriting has continued to evolve, embracing continuous innovation in data analytics, automation, and digital technology. <br>
                    &emsp; - <b>Advanced Data Analytics:</b> Insurers now leverage big data and machine learning algorithms to analyze vast amounts of information, from social media activity to real-time health data from wearable devices; enabling underwriters to assess risk with unprecedented accuracy and granularity.<br>
                    &emsp; - <b>Automation and Streamlined Processes:</b>Automated underwriting systems handle routine tasks, allowing underwriters to focus on complex cases that require human judgment, which streamlines the underwriting process, reducing turnaround times and operational costs.<br> 
                    &emsp; - <b>Comprehensive Databases:</b> Insurers maintain extensive databases that integrate data from various sources, including financial records, medical histories, and lifestyle information; to support more informed and holistic risk assessments.<br>
                    &emsp; - <b>Regulatory and Ethical Considerations:</b>The evolution of underwriting has also been shaped by regulatory and ethical considerations, particularly around data privacy and the use of genetic information; to ensure fair and ethical underwriting practices.<br> 
                    &emsp; - <b>SOA's Role in Ongoing Development:</b>The SOA continues to contribute to the field through research, professional development, and the dissemination of best practices; supporting actuaries in staying abreast of technological advancements and evolving regulatory landscapes."),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f52d", "f5ad", "f11c", "f019"),
    icon.size = 40,
    icon.color = c("orange", "blue", "brown", "black")
  )
  
  underwriting_edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  
  output$underwriting_flowchart <- renderVisNetwork({
    visNetwork(underwriting_nodes, underwriting_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 64, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$underwriting_nodeDescription <- renderText({
    req(input$current_node_id)
    underwriting_selectedNode <- underwriting_nodes[underwriting_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", underwriting_selectedNode$label, br(), underwriting_selectedNode$description)
  }) 
  #----------------------------------------------------annuityIntro flowchart----------------------------------------------------
  annuityIntro_nodes <- data.frame(
    id = 1:4,
    label = c("Ancient Rome","17th Century","18th and 19th Centuries","20th Century to Present"),
    description = c("• <b>Ancient Rome</b> <br>
                    &emsp; - The origins of actuarial science can be traced back to ancient civilizations where early forms of risk management and insurance practices were developed. These practices included pooling resources to mitigate the effects of risks like natural disasters and trade losses.",
                    "• <b>17th Century</b> <br>
                    &emsp; - The first modern annuity contracts were sold by governments in Europe to raise funds. These contracts were more structured, often involving formal agreements between the government and individuals or institutions.",
                    "• <b>18th and 19th Centuries</b> <br>
                    &emsp; - Actuarial methods were developed to price annuities more accurately, using life expectancy tables and probabilistic models. Annuity contracts during this period were typically issued by insurance companies, with pricing based on rigorous actuarial calculations to ensure financial viability.",
                    "• <b>20th Century to Present</b> <br>
                    &emsp; - The annuity market has grown, with products such as variable annuities, indexed annuities, and immediate vs. deferred annuities. These contracts are typically acquired through insurance companies, with underwriting processes involving detailed financial and health assessments to determine the appropriate pricing and terms."
                    ),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f752", "f04e", "f04e", "f573"),
    icon.size = 40,
    icon.color = c("red", "green", "brown", "black")
  )
  
  annuityIntro_edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  
  output$annuityIntro_flowchart <- renderVisNetwork({
    visNetwork(annuityIntro_nodes, annuityIntro_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$annuityIntro_nodeDescription <- renderText({
    req(input$current_node_id)
    annuityIntro_selectedNode <- annuityIntro_nodes[annuityIntro_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", annuityIntro_selectedNode$label, br(), annuityIntro_selectedNode$description)
  })
  #----------------------------------------------------pensionIntro flowchart----------------------------------------------------
  pensionIntro_nodes <- data.frame(
    id = 1:3,
    label = c("19th Century","20th Century","21st Century"),
    description = c("• <b>19th Century</b> <br>
                    &emsp; - Employer-sponsored pension plans began to emerge, with companies like American Express and the Pennsylvania Railroad offering pensions. These plans were typically unfunded, with benefits promised based on the employer's financial stability.",
                    "• <b>20th Century</b> <br>
                    &emsp; - The expansion of pension plans included the introduction of Social Security in 1935 and ERISA in 1974. These plans became more formalized, with employers and employees contributing to funded plans designed to provide retirement income based on years of service and salary history.",
                    "• <b>21st Century</b> <br>
                    &emsp; - The shift from defined benefit to defined contribution plans, such as 401(k) plans, has changed the accrual process. Defined contribution plans involve regular contributions from employers and employees into individual accounts, with the retirement benefit depending on the investment performance of these accounts."
    ),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f252", "f253"),
    icon.size = 40,
    icon.color = c("red", "orange", "green")
  )
  
  pensionIntro_edges <- data.frame(from = c(1, 2), to = c(2, 3))
  
  output$pensionIntro_flowchart <- renderVisNetwork({
    visNetwork(pensionIntro_nodes, pensionIntro_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$pensionIntro_nodeDescription <- renderText({
    req(input$current_node_id)
    pensionIntro_selectedNode <- pensionIntro_nodes[pensionIntro_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", pensionIntro_selectedNode$label, br(), pensionIntro_selectedNode$description)
  })
  #----------------------------------------------------survivalDist plot----------------------------------------------------  
  validateInput <- reactive({
    if ((input$x+(input$t[2]/input$m) >= 130)) {
      return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + (t<sub>max</sub> / m) < 130}</b>.</p>'))
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$button, {
    validation_message <- validateInput()
    output$validation_message <- renderUI({ validation_message })
    
    if (is.null(validation_message)) {
      setup_plot_survivalDist <- eventReactive(input$button,{
        x <- input$x
        t <- c(input$t[1]:input$t[2])
        m <- input$m
        frac_asump <- input$frac_assump
        mort_law <- input$mort_law
        if(mort_law == 'Gompertz'){
          other_params <- list(input$B1,input$C1)
        } else if(mort_law == 'Makeham'){
          other_params <- list(input$A,input$B2,input$C2)
        } else {
          other_params <- list(input$alpha)
        }
        
        userInput <- dict(
          x = x,
          t = t,
          m = m,
          frac_asump = frac_asump,
          mort_law = mort_law,
          mort_params = other_params,
          age_range = list(0,130)
        )
        return(plot_survivalDist_info(userInput,'whole'))
      })
      
      output$output_survivalDist_plot <- renderPlot({
        setup_plot_survivalDist()
      })
      
      setup_plot_survivalDist2 <- eventReactive(input$button,{
        x <- input$x
        t <- c(input$t[1]:input$t[2])
        m <- input$m
        frac_asump <- input$frac_assump
        mort_law <- input$mort_law
        if(mort_law == 'Gompertz'){
          other_params <- list(input$B1,input$C1)
        } else if(mort_law == 'Makeham'){
          other_params <- list(input$A,input$B2,input$C2)
        } else {
          other_params <- list(input$alpha)
        }
        
        userInput <- dict(
          x = x,
          t = t,
          m = m,
          frac_asump = frac_asump,
          mort_law = mort_law,
          mort_params = other_params,
          age_range = list(0,130)
        )
        return(plot_survivalDist_info(userInput,'frac'))
      })
      
      output$output_survivalDist_plot2 <- renderPlot({
        setup_plot_survivalDist2()
      })
      
    } else {
      output$output_survivalDist_plot <- renderPlot({
        NULL  # Clear the plot if validation fails
      })
    }
  })
  #----------------------------------------------------FOM plot----------------------------------------------------  
  validateInput2 <- reactive({
    if ((input$x2+(input$t2[2]/input$m2) >= 130)) {
      return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + (t<sub>max</sub> / m) < 130}</b>.</p>'))
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$button2, {
    validation_message2 <- validateInput2()
    output$validation_message2 <- renderUI({ validation_message2 })
    
    if (is.null(validation_message2)) {
      setup_plot_fom <- eventReactive(input$button2,{
        x <- input$x2
        t <- c(input$t2[1]:input$t2[2])
        m <- input$m2
        frac_asump <- input$frac_assump2
        mort_law <- input$mort_law2
        if(mort_law == 'Gompertz'){
          other_params <- list(input$B12,input$C12)
        } else if(mort_law == 'Makeham'){
          other_params <- list(input$A2,input$B22,input$C22)
        } else {
          other_params <- list(input$alpha2)
        }
        
        userInput <- dict(
          x = x,
          t = t,
          m = m,
          frac_asump = frac_asump,
          mort_law = mort_law,
          mort_params = other_params,
          age_range = list(0,130)
        )
        return(plot_fom_info(userInput))
      })
      
      output$output_fom_plot <- renderPlot({
        setup_plot_fom()
      })
      
    } else {
      output$output_fom_plot <- renderPlot({
        NULL  # Clear the plot if validation fails
      })
    }
  })
  #----------------------------------------------------complete-life numbers----------------------------------------------------
  setup_plot_completeLife <- eventReactive(input$button3,{
        x <- input$x3
        mort_law <- input$mort_law3
        if(mort_law == 'Gompertz'){
          other_params <- list(input$B13,input$C13)
        } else if(mort_law == 'Makeham'){
          other_params <- list(input$A3,input$B23,input$C23)
        } else {
          other_params <- list(input$alpha3)
        }
        
        userInput <- dict(
          x = x,
          t = c(20:100),
          m = 1,
          frac_asump = 'UDD',
          mort_law = mort_law,
          mort_params = other_params,
          age_range = list(0,130)
        )
        return(plot_survivalDist_info(userInput,'complete'))
      })
      
      output$output_completeLife_plot <- renderText({
        setup_plot_completeLife()
      })
      
      output$output_completeLife_plot2 <- renderText({
        setup_plot_completeLife()+0.5
      })

  #----------------------------------------------------lifeTable plot----------------------------------------------------      
      validateInput4 <- reactive({
        if ((input$x4+(input$t4[2]/input$m4) >= 130)) {
          return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + (t<sub>max</sub> / m) < 130}</b>.</p>'))
        } else {
          return(NULL)
        }
      })
      
      observeEvent(input$button4, {
        validation_message4 <- validateInput4()
        output$validation_message4 <- renderUI({ validation_message4 })
        
        if (is.null(validation_message4)) {
          setup_plot_lifeTable <- eventReactive(input$button4,{
            x <- input$x4
            t <- c(input$t4[1]:input$t4[2])
            m <- input$m4
            frac_asump <- 'UDD'
            mort_law <- input$mort_law4
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B14,input$C14)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A4,input$B24,input$C24)
            } else {
              other_params <- list(input$alpha4)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(plot_survivalDist_info(userInput,type='life',radix=input$radix))
          })
          
          output$output_lifeTable_plot <- renderPlot({
            setup_plot_lifeTable()
          })
          
        } else {
          output$output_lifeTable_plot <- renderPlot({
            NULL  # Clear the plot if validation fails
          })
        }
      })  
  #----------------------------------------------------selection plot----------------------------------------------------     
      validateInput5 <- reactive({
        if ((input$x5+(input$t5[2]/input$m5) >= 130)) {
          return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + (t<sub>max</sub> / m) < 130}</b>.</p>'))
        } else if ((input$x5+input$d >= 130)) {
          return(HTML('<p>The sum of life age and select period must be less than 130: <b>{x + d < 130}</b>.</p>'))
        }else {
          return(NULL)
        }
      })
      
      observeEvent(input$button5, {
        validation_message5 <- validateInput5()
        output$validation_message5 <- renderUI({ validation_message5 })
        
        if (is.null(validation_message5)) {
          setup_plot_selection <- eventReactive(input$button5,{
            x <- input$x5
            t <- c(input$t5[1]:input$t5[2])
            m <- input$m5
            frac_asump <- 'UDD'
            mort_law <- input$mort_law5
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B15,input$C15)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A5,input$B25,input$C25)
            } else {
              other_params <- list(input$alpha5)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(plot_survivalDist_info(userInput,type='select',select=input$d))
          })
          
          output$output_selection_plot <- renderPlot({
            setup_plot_selection()
          })
          
        } else {
          output$output_selection_plot <- renderPlot({
            NULL  # Clear the plot if validation fails
          })
        }
      }) 
  #----------------------------------------------------ultimate plot----------------------------------------------------     
      validateInput6 <- reactive({
        if ((input$x6[2]+input$d6 >= 130)) {
          return(HTML('<p>The sum of life age and select period must be less than 130: <b>{x + d < 130}</b>.</p>'))
        } else if (((input$x6[1]-input$d6) < 0 )) {
          return(HTML('<p>The difference of the select period from the life age must at least be zero: <b>{x - d >= 0}</b>.</p>'))
        }
        else {
          return(NULL)
        }
      })
      
      observeEvent(input$button6, {
        validation_message6 <- validateInput6()
        output$validation_message6 <- renderUI({ validation_message6 })
        
        if (is.null(validation_message6)) {
          setup_plot_ultimate <- eventReactive(input$button6,{
            x <- input$x6[2]
            t <- c(0:130)
            m <- 1
            frac_asump <- 'UDD'
            mort_law <- input$mort_law6
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B16,input$C16)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A6,input$B26,input$C26)
            } else {
              other_params <- list(input$alpha6)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(plot_survivalDist_info(userInput,type='ultimate',select=input$d6,radix=input$radix6,x_range=input$x6))
          })
          
          output$output_ultimate_plot <- renderDT({
            setup_plot_ultimate()
          })
          
        } else {
          output$output_ultimate_plot <- renderDT({
            NULL  # Clear the plot if validation fails
          })
        }
      }) 
  #---------------------------------------------insurance dynamicValueBox---------------------------------------------
      validateInput7 <- reactive({
        if ((input$x7 + input$t7[2] >= 130)) {
          return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + t<sub>max</sub> < 130}</b>.</p>'))
        } else if (input$x7 + input$t7[2] + input$n7 >= 130 ){
          return(HTML('<p>The sum of life age, contract term, and maximum future period must be less than 130: <b>{x + t<sub>max</sub> + n < 130}</b>.</p>'))
        } else if (input$x7 + input$t7[2] + input$n7 + input$u7 >= 130 ){
          return(HTML('<p>The sum of life age, deferral period, contract term, and maximum future period must be less than 130: <b>{x + t<sub>max</sub> + n + u  < 130}</b>.</p>'))
        }
          else {
          return(NULL)
        }
      })
      
      observeEvent(input$button7, {
        validation_message7 <- validateInput7()
        output$validation_message7 <- renderUI({ validation_message7 })
        
        if (is.null(validation_message7)) {
          setup_plot_insuranceTable <- eventReactive(input$button7,{
            x <- input$x7
            t <- c(input$t7[1]:input$t7[2])
            m <- input$m7
            n <- input$n7
            i <- input$i7
            u <- input$u7
            annuity_schedule <- 'Due'
            frac_asump <- 'UDD'
            mort_law <- input$mort_law7
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B17,input$C17)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A7,input$B27,input$C27)
            } else {
              other_params <- list(input$alpha7)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              n = n,
              i = i,
              u = u,
              annuity_schedule = annuity_schedule,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(plot_product_info(userInput,type='insuranceTable'))
          })
          
          output$insuranceProducts <- renderUI({
            # Code to generate each of the messageItems here, in a list. This assumes
            # that messageData is a data frame with two columns, 'from' and 'message'.
            valueBoxes <- apply(setup_plot_insuranceTable(), 1, function(row) {
              valueBox(
                value = withMathJax(row[["value"]]),
                subtitle = row[["label"]],
                width=4
              )
            })

            # Create a tagList to hold the value boxes
            tagList(valueBoxes)
          })
          
          setup_plot_wholeInsurance <- eventReactive(input$button7,{
            x <- input$x7
            t <- c(input$t7[1]:input$t7[2])
            m <- input$m7
            n <- input$n7
            i <- input$i7
            u <- input$u7
            annuity_schedule <- 'Due'
            frac_asump <- 'UDD'
            mort_law <- input$mort_law7
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B17,input$C17)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A7,input$B27,input$C27)
            } else {
              other_params <- list(input$alpha7)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              n = n,
              i = i,
              u = u,
              annuity_schedule = annuity_schedule,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(plot_product_info(userInput,type='wholeInsurance'))
          })
          
          output$output_wholeInsurance_plot <- renderPlot({
            setup_plot_wholeInsurance()
          })  
          
          setup_plot_termInsurance <- eventReactive(input$button7,{
            x <- input$x7
            t <- c(input$t7[1]:input$t7[2])
            m <- input$m7
            n <- input$n7
            i <- input$i7
            u <- input$u7
            annuity_schedule <- 'Due'
            frac_asump <- 'UDD'
            mort_law <- input$mort_law7
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B17,input$C17)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A7,input$B27,input$C27)
            } else {
              other_params <- list(input$alpha7)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              n = n,
              i = i,
              u = u,
              annuity_schedule = annuity_schedule,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(userInput)
          })
          
          output$output_termInsurance_plot <- renderPlot({
            plot_product_info(setup_plot_termInsurance(),type='termInsurance')
          })
          
          output$output_endowmentInsurance_plot <- renderPlot({
            plot_product_info(setup_plot_termInsurance(),type='endowmentInsurance')
          })
          
        } else {
          
          output$insuranceProducts <- renderUI({
            NULL
          })
          
          output$output_wholeInsurance_plot <- renderPlot({
            NULL
          })  
          
          output$output_termInsurance_plot <- renderPlot({
            NULL
          })
        }
      }) 
  #---------------------------------------------annuity dynamicValueBox-------------------------------------------------------------
      validateInput8 <- reactive({
        if ((input$x8 + input$t8[2] >= 130)) {
          return(HTML('<p>The sum of life age and maximum future period must be less than 130: <b>{x + t<sub>max</sub> < 130}</b>.</p>'))
        } else if (input$x8 + input$t8[2] + input$n8 >= 130 ){
          return(HTML('<p>The sum of life age, contract term, and maximum future period must be less than 130: <b>{x + t<sub>max</sub> + n < 130}</b>.</p>'))
        } else if (input$x8 + input$t8[2] + input$n8 + input$u8 >= 130 ){
          return(HTML('<p>The sum of life age, deferral period, contract term, and maximum future period must be less than 130: <b>{x + t<sub>max</sub> + n + u  < 130}</b>.</p>'))
        }
        else {
          return(NULL)
        }
      })
      
      observeEvent(input$button8, {
        validation_message8 <- validateInput8()
        output$validation_message8 <- renderUI({ validation_message8 })
        
        if (is.null(validation_message8)) {
          setup_plot_annuityUI <- eventReactive(input$button8,{
            x <- input$x8
            t <- c(input$t8[1]:input$t8[2])
            m <- input$m8
            n <- input$n8
            i <- input$i8
            u <- input$u8
            annuity_schedule <- input$a_s8
            frac_asump <- 'UDD'
            mort_law <- input$mort_law8
            if(mort_law == 'Gompertz'){
              other_params <- list(input$B18,input$C18)
            } else if(mort_law == 'Makeham'){
              other_params <- list(input$A8,input$B28,input$C28)
            } else {
              other_params <- list(input$alpha8)
            }
            
            userInput <- dict(
              x = x,
              t = t,
              m = m,
              n = n,
              i = i,
              u = u,
              annuity_schedule = annuity_schedule,
              frac_asump = frac_asump,
              mort_law = mort_law,
              mort_params = other_params,
              age_range = list(0,130)
            )
            return(userInput)
          })
          
          output$annuityProducts <- renderUI({
            # Code to generate each of the messageItems here, in a list. This assumes
            # that messageData is a data frame with two columns, 'from' and 'message'.
            valueBoxes <- apply(plot_product_info(setup_plot_annuityUI(),'annuityTable'), 1, function(row) {
              valueBox(
                value = withMathJax(row[["value"]]),
                subtitle = row[["label"]],
                width=4
              )
            })
            
            # Create a tagList to hold the value boxes
            tagList(valueBoxes)
          })
          
          output$output_wholeAnnuity_plot <- renderPlot({
            plot_product_info(setup_plot_annuityUI(),'wholeAnnuity')
          })  
          
          
          output$output_termAnnuity_plot <- renderPlot({
            plot_product_info(setup_plot_annuityUI(),'termAnnuity')
          })

          
        } else {
          
          output$annuityProducts <- renderUI({
            NULL
          })
          
          output$output_wholeAnnuity_plot <- renderPlot({
            NULL
          })  
          
          output$output_termAnnuity_plot <- renderPlot({
            NULL
          })
        }
      }) 
  
}

#---------------------------------------------launch app---------------------------------------------
shinyApp(ui, server)