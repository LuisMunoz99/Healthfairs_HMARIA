# Poster plots and descriptive table for health fairs
# Authors:     LM
# Maintainers: LM
# Date: 9-OCT-23
# ===========================================

# Prep -------------------------------------------------------------------------
if(!require(pacman))install.packages("pacman")
p_load(argparse,
       here,
       openxlsx,
       dplyr, 
       table1,
       arrow,
       stringr,
       tidyr,
       ggplot2,
       gridExtra,
       papaja)

# Useful functions
wrap_labels <- function(labels) {
  str_wrap(labels, width = 10)
}

# args {{{
args <- list(input1 = here("Poster_plots/input/vieques1.xlsx"),
             input2 = here("Poster_plots/input/vieques2.xlsx"),
             output1 = here("Poster_plots /output/side_by_side_vieques.png"),
             output2 = here("Poster_plots/output/desc_table.docx"))

# Importing data 
vieques1 <- read.xlsx(args$input1)
vieques2 <- read.xlsx(args$input2)

vieques1 <- vieques1 %>% 
  mutate(across(everything(), tolower)) %>% 
  rename_all(tolower) %>% 
  select(c("patient.name", "gender", "age", starts_with("specialist"))) %>% 
  mutate(fair = "First Health Fair")

vieques2 <- vieques2 %>% mutate(across(everything(), tolower)) %>% 
  rename_all(tolower) %>% 
  select(c("patient.name", "gender", "age", starts_with("specialist"))) %>% 
  mutate(specialist6 = NA,
         fair = "Second Health Fair")


all_pts <- rbind(vieques1,vieques2) # by number of healthfair

rm(vieques1,vieques2)




# Cleaning -------------------------------------------------------------------------

# Keep only specialties (fair stations) columns
specialties_pts <- all_pts %>% select(c("patient.name", starts_with("specialist"), "fair"))


# cleaning values

specialties_pts <- specialties_pts %>%
  mutate_all(
    ~str_replace_all(.,c(
        "\\brheuma\\w*" = "rheumatology",
        "\\bcardio\\w*" = "cardiology",
        "\\bintern\\w*" = "internal medicine",
        "\\bpneumo\\w*" = "pneumology",
        "\\bpsy\\w*" = "psychology",
        "\\bchiro\\w*" = "chiropractic",
        "\\bonco\\w*" = "oncology",
        "\\bobg\\w*" = "OBGYN",
        "\\bped\\w*" = "pediatrics"))) %>%
  mutate_at(vars(starts_with("specialist")), ~str_trim(.))

# Get specialties into a single column making the data into "visits level" data

specialties_pts <- specialties_pts %>%
  pivot_longer(cols = starts_with("specialist"), names_to = NULL, values_to = "specialties") %>%
  drop_na() %>% arrange(specialties)

# Renaming specialties (Make this easy editable) 
specialties_pts <- specialties_pts %>%
  mutate(specialties = str_to_title(specialties)) %>%
  mutate(specialties = case_when(
    specialties == "Internal Medicine" ~ "\nInternal\nMed",
    specialties == "Funcion Pulmonar" ~ "Pneumo",
    specialties == "Pneumology" ~ "Pneumo",
    specialties == "Rheumatology" ~ "Rheuma",
    specialties == "Cardiology" ~ "Cardio",
    specialties == "Pediatrics" ~ "Peds",
    specialties == "Chiropractic" ~ "Chiro",
    specialties == "Family Medicine" ~ "\nInternal\nMed",
    specialties == "Obgyn" ~ "OBGYN",
    specialties == "Cirujano" ~ "Surgery",
    specialties == "Vitales" ~ "Checkup",
    specialties == "Ent" ~ "ENT",
    specialties == "Psychology" ~ "Psych",
    specialties == "Hematologo" ~ "Onco",
    specialties == "Oncology" ~ "Onco",
    
    TRUE ~ specialties  
  ))



# Plots ------------------------------------------------------------------------
visits_pts <- specialties_pts %>% count(fair,specialties)  

# Add specific vjust numbers to accomodate levels appropriate
visits_pts1 <- visits_pts %>% filter(fair == "First Health Fair") %>% 
  mutate(vjust.text.x = ifelse(specialties == "\nInternal\nMed", 0.2, 0.6)) 


visits_pts2 <- visits_pts %>% filter(fair == "Second Health Fair") %>% 
  mutate(vjust.text.x = ifelse(specialties == "\nInternal\nMed", 0.3, 0.5)) %>% 
  filter(specialties != "Brigada") # Exclude brigades visits


# First health fair plot 
plot_health_fair_1 <- ggplot(visits_pts1, aes(x = reorder(specialties, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#096192") +
  labs(
    title = "First Health Fair",
    x = "Stations",
    y = "Number of Visits") +
  geom_text(aes(label = wrap_labels(n)), size = 10, color = "white", fontface = "bold", vjust = 1.5) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), expand = c(0, 0)) +
  scale_x_discrete() +
  theme_minimal() +
  theme(
    plot.margin = margin(l = 40, r = 0, b = 0, t = 0),
    plot.title = element_text(size = 30, margin = margin(10, 0, 20, 0), face = "bold"),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 20, color = "black", hjust = 0.5, vjust = 0.5),  # Middle align y-axis text
    axis.text.x = element_text(size = 15, 
                               color = "black", 
                               angle = 90,
                               vjust = visits_pts2$vjust.text.x,
                               hjust = .5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 23, color = "black", vjust = 4.2, hjust = .6)
  )


plot_health_fair_1

# Second health fair plot 
plot_health_fair_2 <- ggplot(visits_pts2, aes(x = reorder(specialties, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#0E8C7f") +
  labs(
    title = "Second Health Fair",
    x = "Stations",
    y = "Number of Visits"
  ) +
  geom_text(data = visits_pts2[visits_pts2$n > 4, ],aes(label = wrap_labels(n)), size = 10, color = "white", fontface = "bold", vjust = 1.5) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60), expand = c(0, 0)) +
  scale_x_discrete() +
  theme_minimal() +
  theme(
    plot.margin = margin(l = 40, r = 0, b = 0, t = 0),
    plot.title = element_text(size = 30, margin = margin(10, 0, 20, 0), face = "bold"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15,
                               color = "black",
                               angle = 90,
                               vjust = visits_pts2$vjust.text.x,                                
                               hjust = 0.5),  # Center x-axis labels
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
)

plot_health_fair_2

# Create a multi-plot layout
side_by_side <- grid.arrange(plot_health_fair_1, plot_health_fair_2, ncol = 2)

print(side_by_side)


# Descriptive table ------------------------------------------------------------
desc <- all_pts %>% 
  mutate(Gender = stringr::str_to_title(gender),
         Age = as.numeric(age)) %>% 
  mutate(Gender = case_when(
    Gender == "F" ~ "Female",
    Gender == "F " ~ "Female",
    Gender == "M" ~ "Male",
    TRUE ~ Gender))

desc$Gender <- as.factor(desc$Gender) 

desc_table <- table1(~ Age + Gender | fair, data=desc, render.missing = NULL)

desc_table

# Export -----------------------------------------------------------------------
ggsave(args$output1, side_by_side, width = 12, height = 8)

