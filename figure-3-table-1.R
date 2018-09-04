######     
######     Results by ICD 10 groups
######     Code for Table 1 & Figure 3
######
source("init.R")
load("rdas/pr-icd.rda")

hurricane_date <- hurricane_dates$PR[3]

counts <- pr_icd %>% 
            filter(Year <= 2017 & yday(Date)>=yday(hurricane_date)) %>%
            mutate(hurricane = ifelse(Year < 2017, "before", "after"),
                   hurricane = factor(hurricane, levels = c("before", "after"))) %>%
            group_by(ICD, hurricane) %>%
            summarize(Deaths = sum(Deaths, na.rm = TRUE)) %>%
            ungroup()
            
person_years <- pr_population %>%
                  mutate(Year = year(Date)) %>%
                  filter(Year <= 2017 & yday(Date)>=yday(hurricane_date)) %>%
                  mutate(hurricane = ifelse(Year < 2017, "before", "after"),
                         hurricane = factor(hurricane, levels = c("before", "after"))) %>%
                  group_by(hurricane) %>%
                  summarize(person_years = sum(Population/365)) %>%
                  spread(hurricane, person_years) %>%
                  ungroup()
  
res <- counts %>% 
        spread(hurricane, Deaths) %>%
        mutate(log_ir = log( (after / person_years$after) / (before / person_years$before)),
               se = sqrt(1/after + 1/before)) %>%
        left_join(icd_map, by = "ICD") 

## now odd the terms
tmp <- map_df(cause_of_death_terms, function(term){
  filter(pr_icd, pr_icd[,term]) %>%
    filter(Year <= 2017 & yday(Date)>=yday(hurricane_date)) %>%
    mutate(hurricane = ifelse(Year < 2017, "before", "after"),
           hurricane = factor(hurricane, levels = c("before", "after"))) %>%
    group_by(hurricane) %>%
    summarize(Deaths = sum(Deaths, na.rm = TRUE)) %>%
    spread(hurricane, Deaths) %>%
    mutate(log_ir = log( (after / person_years$after) / (before / person_years$before)),
           se = sqrt(1/after + 1/before)) %>%
    mutate(Description = str_to_title(term), ICD = "") %>%
    ungroup()
})

res <- res %>%
  bind_rows(tmp)

### figure 3-1
p<-res %>% filter(!is.na(log_ir)) %>%
  mutate(Description = str_remove_all(Description, "(Diseases of the )|(,\ .*)|(\ and\ certain\ .*)"))  %>%
  mutate(Description = replace_na(str_to_title(Description),"NA")) %>%
  mutate(Description = reorder(Description, log_ir)) %>%
  ggplot(aes(Description, log_ir, ymin = log_ir - qnorm(0.05/30/2)*se, ymax = log_ir + qnorm(0.05/30/2)*se)) + ##bonferroni corrected
  geom_linerange() + 
  geom_point() +
  geom_hline(yintercept=0,lty=2,col="red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Log death rate ratio") + # need to check this
  xlab("") + 
  scale_y_continuous(limits = c(-8, 8)) +
  scale_x_discrete(labels=c("Skin And Subcutaneous Tissue" = "Subcutaneous Tissue",
                            "Ill-Defined And Unknown Causes Of Mortality"="Unknown Causes",
                            "Musculoskeletal System And Connective Tissue"="Musculoskeletal System",
                            "Mental And Behavioural Disorders"="Mental & Beh. Disorders",
                            "Genitourinary System: Urinary System"="Urogenital System",
                            "External Causes Of Morbidity And Mortality"="External Causes",
                            "Infections Caused By Fungi" = "Fungi Infections",
                            "Certain Conditions Originating In The Perinatal Period" = "Perinatal Period",
                            "Congenital Malformations And Deformations" = "Malformations & Deformations",
                            "Diseases of the circulatory system"="Circulatory System",
                            "Endocrine diseases"="Endocrine Diseases",
                            "Diseases of the nervous system"="Nervous System",
                            "Diseases of the respiratory system"="Respiratory System",
                            "Metabolomic diseases","Metabolomic Diseases",
                            "Diseases of the digestive"="Digestive System",
                            "Diseases of the musculosk"="Musculoskeletal System",
                            "Bacterial infections, and other intestinal infectious diseases, and STDs"="Bacterial & Infectious",
                            "suicide"="Suicide",
                            "Nutritional diseases"="Nutritional Diseases")) + 
  coord_flip()+
  theme(axis.text.x=element_text(angle=0, hjust=1, face="bold", color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        strip.text = element_text(size = 10,face="bold",color="black"), 
        axis.title = element_text(face="bold", color="black"))

### Saving figure figure-3-2
ggsave(paste("figs/figure-3A.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)

### for table 1 and supp table
tab <- res %>% 
  mutate(observed = after,
         expected = before/person_years$before * person_years$after,
         increase = (exp(log_ir)-1)*100,
         excess = observed - expected,
         lower = exp(log_ir - 1.96*se)*expected,
         upper = exp(log_ir + 1.96*se)*expected,
         p_value = 2*(1 - pnorm( log_ir / se ))) %>%
  select(ICD, Description, observed, expected, excess, increase, lower, upper, p_value) %>%
  arrange(desc(excess))
           
## table 1 is bonferroni corrected
library(htmlTable)
options(digits=3)
tt <- tab %>% filter(p_value < 0.05/30) %>%
  mutate(expected = as.integer(round(expected)),
         excess = as.integer(round(excess)),
         increase = as.integer(round(increase)),
         lower = as.integer(round(lower)),
         upper = as.integer(round(upper)),
         p_value = round(p_value, 4),
         CI = paste0("[ ",lower,"-",upper,"]")) %>%
  select(Description, ICD, observed, expected, excess, CI) %>%
  setNames(c("Description", "ICD", "Observed", "Expected", "Excess", "CI")) %>%
  htmlTable(rnames = FALSE,
            align = c("l", "l", "r", "r", "r", "r", "r"))
 
## supp table
sink("figs/table-1.html")
print(tt,type="html",useViewer=FALSE)
sink()

### Code for figure
tmp <- tab %>% filter(p_value < 0.05/30)
a<-ggdotchart(tmp, x = "Description", y = "excess",
              color = "#023858",                                # Color by groups
              #palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
              sorting = "descending",                       # Sort value in descending order
              add = "segments",                             # Add segments from y = 0 to dots
              rotate = TRUE,                                # Rotate vertically
              #group = "cyl",                                # Order by groups
              dot.size = 8,                                 # Large dot size
              label = round(tmp$excess),                        # Add mpg values as dot labels
              font.label = list(color = "white", size = 9, 
                                vjust = 0.5,face="bold"),               # Adjust label parameters
              ggtheme = theme_minimal()                        # ggplot2 theme
)

p<-a+scale_x_discrete(labels=c("Diseases of the circulatory system"="Circulatory System",
                            "Endocrine diseases"="Endocrine Diseases",
                            "Diseases of the nervous system"="Nervous System",
                            "Diseases of the respiratory system"="Respiratory System",
                            "Diseases of the genitourinary system: urinary system"="Urogenital System",
                            "Metabolomic diseases","Metabolomic Diseases",
                            "Diseases of the digestive"="Digestive System",
                            "Ill-defined and unknown causes of mortality"="Unknown Causes",
                            "External causes of morbidity and mortality"="External Causes",
                            "Diseases of the skin and subcutaneous tissue"="Subcutaneous Tissue",
                            "Mental and behavioural disorders"="Mental & Beh. Disorders",
                            "Diseases of the musculosk"="Musculoskeletal System",
                            "Bacterial infections, and other intestinal infectious diseases, and STDs"="Bacterial & Infectious",
                            "Diseases of the musculoskeletal system and connective tissue"="Musculoskeletal System",
                            "suicide"="Suicide",
                            "Nutritional diseases"="Nutritional Diseases"))+
  ylab("Excess deaths by Dec 31, 2017") + 
  xlab("") +
  theme(axis.text.x=element_text(face="bold",color="black"),
        axis.text.y=element_text(face="bold",color="black"),
        axis.title = element_text(face="bold"),
        legend.text = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.position = "bottom")

### Saving figure figure-3B
ggsave(paste("figs/figure-3B.pdf"),
       plot = p,
       width = 6, height = 4,
       units = "in",
       dpi = 300)


### cp icd map file to tables
file.copy("data/ICD10_groups.csv", "figs/supp-table-2.csv", overwrite = TRUE)

