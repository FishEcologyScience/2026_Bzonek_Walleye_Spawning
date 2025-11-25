#bug_fix_singularity

no_variance_groups <- m_data.train %>%
  group_by(animal_id) %>%
  summarize(across(where(is.numeric), var)) %>%
  filter(across(everything(), ~ .x == 0)) %>%
  pull(group)


### All animal IDs reach singularity.
#build a loop to find source of singularity
j <- unique(m_data$animal_id) #list each animal ID

for(i in 1:(length(j)-1)){
 print(paste("testing:", j[i], "and", j[i+1]))
 
 loopdata <- filter(m_data, animal_id %in% c(j[i], j[i+1]))
 
 loopmodel <- glmer(spawn ~ (1| animal_id),
           data = loopdata,
           family = binomial)

}

