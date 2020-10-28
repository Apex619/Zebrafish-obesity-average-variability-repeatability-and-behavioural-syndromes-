filter_etho <- function(Personality, x, y){
  
  
  Personality %>% 
    filter(Video %in% c(x)) %>%
    filter(Time == y)
}

#Example of the function
#Personality %>% filter(Video %in% c("A","C","D","E","G","M")) -> Personality_Social1

#Personality_Social1 %>% filter(Time == "6-9") -> Personality_Social1_OnlyPhase1Data

#Personality_Social1_OnlyPhase1Data