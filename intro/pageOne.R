library(dplyr) #df manipulation
library(ggplot2) #plotting 
library(tidyverse) #df manipulation
library(methods) # class structure
library(latex2exp) #latex text
library(tinsel) #decorator functions
library(tidyr) #df manipulation
library(SciViews) #logarithm function
source('~/Library/Mobile Documents/com~apple~CloudDocs/Documents/mini/projects/soaFAM/R/discreteProbs.R') #actuarial functions

pageOne <- setRefClass('pageOne', #underlying computations for the first page of the fam-l dashboard
                       fields=list(x='numeric',
                                   t='numeric'),
                       contains='discreteProbs',
                       methods=list(
                         initialize = function(...) {
                           if (missing(...)) { #set default values if no arguments are provided
                             .self$x<-20 #current age
                             .self$t<-50 #term of interest
                             .self$m<-12 #m-thly periodicity
                             .self$frac_asump<-'UDD' #fractional age assumption
                             .self$mort_law<-'Makeham' #mortality law
                             .self$age_range<-list(0, 130) #age limits
                             .self$other_params<-list(0.00022,2.7 * 10^(-6),1.124) #Makeham parameters
                           }else{ #if arguments are provided, use them to initialize the object
                             argsOne <- list(...) #assign  parameters
                             .self$x<-argsOne$x #current age
                             .self$t<-argsOne$t #term of interest
                             .self <- callSuper(...)
                           }},
                         prob_dists = function(){ #cdf & pdf distribution plots
                           data.frame(id=seq(0,(.self$age_range[[2]]-(.self$x+1)),1/.self$m)) %>% #setup input mortality data
                             mutate(p=sapply(id,.self$p_x_t,x=.self$x))%>% #compute survival probability
                             mutate(f_x_t=sapply(id,.self$f_x_t,x=.self$x)*10) %>% #compute force of mortality
                             mutate(x_t=.self$x+id) %>%
                             ggplot(aes(id)) +
                             geom_point(aes(y=p,color='CDF')) +
                             geom_point(aes(y=f_x_t,color='PDF')) +
                             labs( #customize plot
                               title='Discrete Future Life-time Distribitions',
                               x = TeX(r'(Number of Discrete Future Periods $\left{\sum_{k=1}^{\omega-(x+1)}\right}$)'),
                               subtitle = paste('Based on: ',.self$mort_law,"'s Law of Mortality", sep = ''),
                               color='Distribution'
                             ) +
                             theme_dark() +
                             theme(plot.title = element_text(hjust = 0.5)) +
                             theme(plot.subtitle = element_text(hjust = 0.5)) +
                             scale_y_continuous(name = TeX(r'($S_{x}(t)$)'), sec.axis = sec_axis(~., name = TeX(r'($f_{x}(t)$ * 10)')))
                         },
                         discrete_cl = function(){ #discrete complete-life distribution
                           data.frame(id=seq(0,(.self$age_range[[2]]-(.self$x+1)),1/.self$m)) %>% #setup input mortality data
                             mutate(p=sapply(id,.self$p_x_t,x=.self$x)) %>% #compute survival probability
                             mutate(cum_p=cumsum(p)/.self$m) %>% #compute cumulative survival 
                             mutate(x_t=.self$x+id) %>%
                             ggplot() +
                             geom_point(aes(id,cum_p)) + #plot cumulative survival
                             #customize plot
                             labs(title=TeX(r'(Complete Curate Future Life-time $\left(e_{\x}\right)$)'),
                                  x=TeX(r'(Number of Discrete Future Periods $\left{\sum_{k=1}^{\omega-(x+1)}\right}$)'),
                                  y=TeX(r'(Cumulative Survival Function $\left{\sum_{k=1}^{\omega-(x+1)}{S_{x}(t)}\right}$)'),
                                  subtitle = paste('Based on: ',.self$mort_law,"'s Law of Mortality", sep = '')) +
                             theme_dark()+
                             theme(plot.title = element_text(hjust = 0.5)) +
                             theme(plot.subtitle = element_text(hjust = 0.5))
                         },
                         life_table = function(){ #life-table distribution
                           
                           
                         }))
