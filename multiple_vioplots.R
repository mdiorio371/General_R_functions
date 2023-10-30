## a function that creates one violin plot for each column of a dataset

library(tidyverse)
library(data.table)
library(gridExtra)

multiple_vioplot <- 
  function(data_table, colnum){
    
    x = "test"
    dt_melted <- 
      pivot_longer(
        data_table,
        cols = everything(), 
        names_to = "variable", 
        values_to = "value"
      )
    variables <- 
      colnames(data_table)
    #variable_type
    
    #lapply(variables, class) 
    v_plots <- 
      lapply(
        variables,
        function(v){
          #v = variables[1]
          variable_type <- 
            pull(data_table, v) %>% 
            class
          
          if(!is.numeric(pull(data_table, v))){
            ggplot(
              data = 
                dt_melted %>% filter(variable==v) %>% 
                group_by(variable) %>% count(value) %>%
                mutate(value = as.factor(value)),
              aes(x = value, y = n)
            )+
              geom_bar(stat="identity") +
              theme_classic() +
              ylab("") +
              xlab(v) +
              theme(
                axis.title.x = element_text(size=16),
                axis.text =  element_text(size=16)
              )
          } else{
            ggplot(
              data = dt_melted %>% filter(variable==v),
              aes(x = variable, y = value)
            )+ 
              geom_violin() + theme_classic()+ geom_boxplot(width=0.1) +
              theme(
                axis.title = element_blank(),
                axis.text.x = element_text(size = 16),
                axis.text.y =  element_text(size=16)
              )
          }
          
        }
      )  
    do.call(
      "grid.arrange", 
      c(
        v_plots,
        ncol=colnum
      )
    )
  }


### Example
data("mtcars")

data_table <- as_tibble(mtcars)

multiple_vioplot(
  data_table = data_table,
  colnum = 4
)
