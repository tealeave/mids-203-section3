

library(datasauRus)
library(dplyr)
library(ggplot2)
library(tibble)
library(gifski)

#view(datasaurus_dozen_wide)



if(requireNamespace("dplyr")){
  suppressPackageStartupMessages(library(dplyr))
  datasaurus_dozen %>% 
    #filter(dataset == "dino") %>%
    group_by(dataset) %>% 
    summarize(
      mean_x    = mean(x),
      mean_y    = mean(y),
      std_dev_x = sd(x),
      std_dev_y = sd(y),
      corr_x_y  = cor(x, y)
    )
}
summary_stats <- .Last.value

#images
if(requireNamespace("ggplot2")){
  library(ggplot2)
  ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
    geom_point()+
    theme_void()+
    theme(legend.position = "none")+
    facet_wrap(~dataset, ncol = 3)
}

#datasaurus_dozen

library(gganimate)

p <- ggplot( data = datasaurus_dozen,
       aes(x=x,y=y)) + geom_point(); p

animl <- p + transition_states(states = dataset,
                               transition_length = 2,
                               state_length = 1,
                               wrap= FALSE)

#animl

p2 <- ggplot( data = datasaurus_dozen,
             aes(x=x,y=y, color=dataset)) + geom_point(); p2


animl2 <- p2 + transition_states(states = dataset,
                               transition_length = 2,
                               state_length = 1,
                               wrap= FALSE)

animl2


anim3 <- datasaurus_dozen %>%
         ggplot(aes(x=x,y=y, color=dataset,
                  group = 1)) + geom_point(size = 2) +
         theme_bw() + 
         theme(legend.position = 'none') +
         transition_states(states = dataset,
                    transition_length = 2,
                    state_length = 1,
                    wrap= FALSE)

anim3 + ggtitle('Now showing dataset: {closest_state}')


