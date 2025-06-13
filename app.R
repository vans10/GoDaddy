#R Shiny Dashboard - Female Entrepreneurs in the US


#Load the required libraries
library(shiny)
library(janitor)
library(ggplot2)
library(forcats)
library(shinythemes)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr) 


# Set working directory 
setwd("C:/Users/Vanshika/Desktop/Field Project/Final Survey Data/new")
#====================================================================

# Define the survey data files we'll be analyzing
# These are the Excel files containing raw survey responses
files <- c("VF_US_National_FEB23_RAWDATA.xlsx",
           "VF_US_National_Aug2023_RAWDATA.xlsx",
           "VF_US_National_FEB24_RawData.xlsx")

# Label each file with its corresponding survey period
# This helps us track when each survey was conducted
survey_years <- c(
  "VF_US_National_FEB23_RAWDATA.xlsx" = "Feb 2023",
  "VF_US_National_Aug2023_RAWDATA.xlsx" = "Aug 2023",
  "VF_US_National_FEB24_RawData.xlsx" = "Feb 2024"
)

# Define where to find each survey question in the Excel files
# This maps question codes (e.g., q72) to their location in the Excel sheets
# Format is "cell range" where the question values and text are stored
question_ranges <- list(
  "VF_US_National_FEB23_RAWDATA.xlsx" = list(
    q72  = "B1491:C1501", # First investment source
    q3a  = "B57:C65",     # Total employees
    q25b = "B1502:C1509", # First sale location
    d1   = "B1777:C1780", # Gender
    d3   = "B1781:C1786", # Race
    d4   = "B1787:C1789", # Hispanic Origin
    d5a  = "B1790:C1795", # Marital Status
    d6   = "B1796:C1802", # Education Level
    d10  = "B1816:C1823", # Household Income
    q15  = "B686:C692",   # Businesses Started
    q25  = "B1510:C1518", # Startup Capital
    q12  = "B1677:C1685", # Hours Worked
    d7   = "B1803:C1810"  # Age Range (for Feb 2023)
  ),
  # Similar mappings for other survey years...
  "VF_US_National_Aug2023_RAWDATA.xlsx" = list(
    q72  = "B1448:C1460", q3a  = "B52:C60",  q25b = "B1474:C1482",
    d1   = "B1903:C1906", d3   = "B1907:C1912", d4   = "B1913:C1915",
    d5a  = "B1916:C1921", d6 = "B1922:C1928", d10  = "B1948:C1955",
    q15  = "B686:C692", q25 = "B1483:C1492", q12 = "B1674:C1682",
    q75  = "B1754:C1762"  # Stress level
    # Note: No cell range for d12 (birth year) as it's a direct input
    # Note: No cell range for s3 (zip code) as it's a direct input
  ),
  "VF_US_National_FEB24_RawData.xlsx" = list(
    q72  = "B1476:C1488", q3a  = "B57:C65",  q25b = "B1502:C1510",
    d1   = "B2036:C2039", d3   = "B2040:C2045", d4   = "B2046:C2048",
    d5a  = "B2049:C2054", d6 = "B2055:C2061", d10  = "B2069:C2077",
    q15  = "B742:C748", q25 = "B1511:C1520", q12 = "B1574:C1582"
    # Note: No cell range for d12 (birth year) as it's a direct input
  )
)

# Create a helper function to clean up survey responses
# This removes the numeric codes that appear in parentheses at the start of responses
# For example, "(1) Yes" becomes just "Yes"
remove_survey_numbers <- function(x) {
  gsub("^\\(\\d+\\)\\s*", "", x)
}

# Function to process each survey dataset file
# This reads the Excel file, extracts the relevant questions, and cleans the data
process_file <- function(file) {
  # Determine which sheet in the Excel file contains the data
  available_sheets <- excel_sheets(file)
  sheet_name <- if ("Data" %in% available_sheets) "Data" else "data"
  
  # Different approach for each file due to different question formats and availability
  if (file == "VF_US_National_FEB23_RAWDATA.xlsx") {
    df <- read_excel(file, sheet = sheet_name) %>%
      select(q20, q72, q3a, q25b, d1, d3, d4, d5a, d6, d10, q15, q25, q12, d7) %>%
      mutate(across(everything(), as.character)) %>%
      # Add empty zip code column for consistency with other datasets
      mutate(Zip_Code = NA_character_, q75 = NA_character_,
             q76_top1_1 = NA_character_, q76_1 = NA_character_,
             Financial_Stress_Top1 = "No value")
  } else if (file == "VF_US_National_Aug2023_RAWDATA.xlsx") {
    df <- read_excel(file, sheet = sheet_name) %>%
      select(q20, q72, q3a, q25b, d1, d3, d4, d5a, d6, d10, q15, q25, q12, q75, q76_top1_1, q76_1, d12, s3) %>%
      mutate(across(everything(), as.character)) %>%
      # Rename the zip code column directly
      rename(Zip_Code = s3)
    
    # Read financial stress values for Aug 2023
    values1 <- read_excel(file, sheet = "Values", range = "B1763:C1767", col_names = c("q76_top1_1", "q76_top1_1_text"))
    values2 <- read_excel(file, sheet = "Values", range = "B1833:C1842", col_names = c("q76_1", "q76_1_text"))
    df <- df %>%
      left_join(mutate(values1, q76_top1_1 = as.character(q76_top1_1)), by = "q76_top1_1") %>%
      left_join(mutate(values2, q76_1 = as.character(q76_1)), by = "q76_1") %>%
      mutate(Financial_Stress_Top1 = coalesce(q76_1_text, q76_top1_1_text))
  } else if (file == "VF_US_National_FEB24_RawData.xlsx") {
    df <- read_excel(file, sheet = sheet_name) %>%
      select(q20, q72, q3a, q25b, d1, d3, d4, d5a, d6, d10, q15, q25, q12, q76_top1_1, q76_1, d12) %>%
      mutate(across(everything(), as.character)) %>%
      # Add empty zip code column for consistency with other datasets
      mutate(Zip_Code = NA_character_, q75 = NA_character_)
    
    # Read financial stress values for Feb 2024
    values1 <- read_excel(file, sheet = "Values", range = "B1686:C1690", col_names = c("q76_top1_1", "q76_top1_1_text"))
    values2 <- read_excel(file, sheet = "Values", range = "B1786:C1788", col_names = c("q76_1", "q76_1_text"))
    df <- df %>%
      left_join(mutate(values1, q76_top1_1 = as.character(q76_top1_1)), by = "q76_top1_1") %>%
      left_join(mutate(values2, q76_1 = as.character(q76_1)), by = "q76_1") %>%
      mutate(Financial_Stress_Top1 = coalesce(q76_1_text, q76_top1_1_text))
  }
  
  # Process each question by matching numeric codes to their text descriptions
  # For example, if q72 has "1", look up what value "1" represents for that question
  for (col in names(question_ranges[[file]])) {
    if (col != "q20" && col %in% names(df)) {  # Skip q20 and check if column exists
      range <- question_ranges[[file]][[col]]
      values <- read_excel(file, sheet = "Values", range = range, col_names = c(col, paste0(col, "_text")))
      if (nrow(values) > 0) {
        values[[col]] <- as.character(values[[col]])
        df[[col]] <- as.character(df[[col]])
        df <- left_join(df, values, by = col)
      }
    }
  }
  
  # Add the survey year column to track which survey this data came from
  df$Survey_Year <- survey_years[[file]]
  
  # Process age data differently based on the file
  if (file == "VF_US_National_FEB23_RAWDATA.xlsx") {
    # For Feb 2023, we already have age ranges in d7_text
    if ("d7_text" %in% names(df)) {
      df <- df %>% 
        rename(Owner_Age_Range = d7_text) %>%
        select(-d7)  # Remove the numeric code column
    }
  } else {
    # For Aug 2023 and Feb 2024, calculate age from birth year
    if ("d12" %in% names(df)) {
      survey_year <- ifelse(file == "VF_US_National_Aug2023_RAWDATA.xlsx", 2023, 2024)
      
      df <- df %>%
        mutate(
          Birth_Year = suppressWarnings(as.numeric(d12)),
          Owner_Age = case_when(
            Birth_Year > 0 & Birth_Year < 2023 ~ survey_year - Birth_Year,
            TRUE ~ NA_real_
          ),
          Owner_Age_Range = case_when(
            Owner_Age < 25 ~ "Under 25",
            Owner_Age >= 25 & Owner_Age < 35 ~ "25-34",
            Owner_Age >= 35 & Owner_Age < 45 ~ "35-44",
            Owner_Age >= 45 & Owner_Age < 55 ~ "45-54",
            Owner_Age >= 55 & Owner_Age < 65 ~ "55-64",
            Owner_Age >= 65 ~ "65 or older",
            TRUE ~ "Unknown"
          )
        ) %>%
        select(-d12, -Birth_Year, -Owner_Age)  # Remove intermediate columns
    }
  }
  
  # Clean up the data by removing numeric codes and using only the text descriptions
  cols_to_remove <- intersect(names(df), setdiff(names(question_ranges[[file]]), "q20"))
  df <- df %>% select(-all_of(cols_to_remove))  
  df <- df %>% mutate(across(everything(), remove_survey_numbers))  
  return(df)
}

# Process all three survey files and combine them into one dataset
merged_data <- bind_rows(lapply(files, process_file))

# Rename the columns to more descriptive names for easier analysis
# Define all possible column names that could exist in our dataset
all_possible_columns <- c(
  "q20", "q72_text", "q3a_text", "q25b_text", 
  "d1_text", "d3_text", "d4_text", "d5a_text", "d6_text", 
  "d10_text", "q15_text", "q25_text", "q12_text", "q75_text",
  "Survey_Year", "Owner_Age_Range", "Zip_Code",
  "q76_top1_1_text", "q76_1_text", "Financial_Stress_Top1"
)

# Create a mapping from source column names to desired column names
column_name_mapping <- c(
  "q20" = "Business_Launch_Year",
  "q72_text" = "First_Investment_Source",
  "q3a_text" = "Total_Employees",
  "q25b_text" = "First_Sale_Location",
  "d1_text" = "Gender",
  "d3_text" = "Race",
  "d4_text" = "Hispanic_Origin",
  "d5a_text" = "Marital_Status",
  "d6_text" = "Education_Level",
  "d10_text" = "Household_Income",
  "q15_text" = "Businesses_Started",
  "q25_text" = "Startup_Capital",
  "q12_text" = "Hours_Worked",
  "q75_text" = "Stress_Level",
  "Survey_Year" = "Survey_Year",
  "Owner_Age_Range" = "Owner_Age_Range",
  "Zip_Code" = "Zip_Code",
  "q76_top1_1_text" = "Top_Financial_Stress_Text",
  "q76_1_text" = "Ranked_Financial_Stress_Text",
  "Financial_Stress_Top1" = "Financial_Stress"
)

# Apply the column renaming only for columns that exist in the dataset
for (col in names(merged_data)) {
  if (col %in% names(column_name_mapping)) {
    merged_data <- merged_data %>%
      rename(!!column_name_mapping[col] := col)
  }
}

# Replace all remaining NA values with "Unknown" for clarity
merged_data <- merged_data %>%
  mutate(across(where(is.character), ~replace_na(.x, "Unknown")))

# Convert categorical columns to factors
# This ensures they'll be treated as categories in our analysis
merged_data <- merged_data %>%
  mutate(
    Total_Employees = as.factor(Total_Employees),
    First_Investment_Source = as.factor(First_Investment_Source),
    First_Sale_Location = as.factor(First_Sale_Location)
  )

# Convert Business_Launch_Year to numeric values
# suppressWarnings prevents errors if some values can't be converted
merged_data <- merged_data %>%
  mutate(Business_Launch_Year = suppressWarnings(as.numeric(Business_Launch_Year)))

# Replace special codes in Business_Launch_Year with meaningful labels
# In the survey, -9 means "Prefer not to answer" and -8 means "Don't know"
merged_data <- merged_data %>%
  mutate(Business_Launch_Year = case_when(
    Business_Launch_Year == -9 ~ "Prefer not to answer",
    Business_Launch_Year == -8 ~ "Don't know",
    TRUE ~ as.character(Business_Launch_Year)
  ))

# Convert the survey year to a numeric value for calculations
# This helps us calculate business age (current year - launch year)
merged_data <- merged_data %>%
  mutate(Survey_Year_Numeric = case_when(
    Survey_Year == "Feb 2023" ~ 2023,
    Survey_Year == "Aug 2023" ~ 2023,
    Survey_Year == "Feb 2024" ~ 2024,
    TRUE ~ NA_real_
  ))

# Calculate how old each business is based on launch year and survey year
# For example, if a business launched in 2020 and the survey was in 2023, age is 3 years
merged_data <- merged_data %>%
  mutate(Business_Age = ifelse(
    grepl("^[0-9]+$", Business_Launch_Year) & !is.na(Survey_Year_Numeric),
    as.numeric(Survey_Year_Numeric) - as.numeric(Business_Launch_Year),
    "Unknown"
  ))

# Clean and format zip codes (taking first 5 digits only)
merged_data <- merged_data %>%
  mutate(
    # Extract first 5 digits of zip code if available
    Zip_Code = case_when(
      !is.na(Zip_Code) & grepl("^\\d{5}", Zip_Code) ~ substr(Zip_Code, 1, 5),
      TRUE ~ Zip_Code
    )
  )

# Create a function to map zip codes to states
get_state_from_zip <- function(zip) {
  # First check if zip is valid
  if (is.na(zip) || zip == "Unknown" || zip == "00000" || nchar(zip) < 5) {
    return("Unknown")
  }
  
  # Extract first 3 digits of zip code for general region matching
  zip_prefix <- as.numeric(substr(zip, 1, 3))
  
  # Map zip code ranges to states
  if (zip_prefix >= 0 && zip_prefix <= 9) {
    return("Puerto Rico/Virgin Islands")
  } else if (zip_prefix >= 10 && zip_prefix <= 14) {
    return("New York")
  } else if (zip_prefix >= 15 && zip_prefix <= 19) {
    return("Pennsylvania")
  } else if (zip_prefix >= 20 && zip_prefix <= 21) {
    return("District of Columbia/Maryland")
  } else if (zip_prefix == 22) {
    return("Virginia")
  } else if (zip_prefix == 23) {
    return("Virginia")
  } else if (zip_prefix == 24) {
    return("West Virginia")
  } else if (zip_prefix >= 25 && zip_prefix <= 26) {
    return("West Virginia")
  } else if (zip_prefix >= 27 && zip_prefix <= 28) {
    return("North Carolina")
  } else if (zip_prefix == 29) {
    return("South Carolina")
  } else if (zip_prefix >= 30 && zip_prefix <= 31) {
    return("Georgia")
  } else if (zip_prefix == 32) {
    return("Florida")
  } else if (zip_prefix == 33) {
    return("Florida")
  } else if (zip_prefix == 34) {
    return("Florida")
  } else if (zip_prefix >= 35 && zip_prefix <= 36) {
    return("Alabama")
  } else if (zip_prefix == 37) {
    return("Tennessee")
  } else if (zip_prefix == 38) {
    return("Tennessee/Mississippi")
  } else if (zip_prefix == 39) {
    return("Mississippi")
  } else if (zip_prefix >= 40 && zip_prefix <= 42) {
    return("Kentucky")
  } else if (zip_prefix >= 43 && zip_prefix <= 45) {
    return("Ohio")
  } else if (zip_prefix >= 46 && zip_prefix <= 47) {
    return("Indiana")
  } else if (zip_prefix == 48) {
    return("Michigan")
  } else if (zip_prefix == 49) {
    return("Michigan")
  } else if (zip_prefix >= 50 && zip_prefix <= 52) {
    return("Iowa")
  } else if (zip_prefix == 53) {
    return("Wisconsin")
  } else if (zip_prefix == 54) {
    return("Wisconsin")
  } else if (zip_prefix == 55) {
    return("Minnesota")
  } else if (zip_prefix == 56) {
    return("Minnesota")
  } else if (zip_prefix == 57) {
    return("South Dakota")
  } else if (zip_prefix == 58) {
    return("North Dakota")
  } else if (zip_prefix == 59) {
    return("Montana")
  } else if (zip_prefix == 60) {
    return("Illinois")
  } else if (zip_prefix == 61) {
    return("Illinois")
  } else if (zip_prefix == 62) {
    return("Illinois")
  } else if (zip_prefix == 63) {
    return("Missouri")
  } else if (zip_prefix == 64) {
    return("Missouri")
  } else if (zip_prefix == 65) {
    return("Missouri")
  } else if (zip_prefix == 66) {
    return("Kansas")
  } else if (zip_prefix == 67) {
    return("Kansas")
  } else if (zip_prefix == 68) {
    return("Nebraska")
  } else if (zip_prefix == 69) {
    return("Nebraska")
  } else if (zip_prefix == 70) {
    return("Louisiana")
  } else if (zip_prefix == 71) {
    return("Louisiana")
  } else if (zip_prefix == 72) {
    return("Arkansas")
  } else if (zip_prefix == 73) {
    return("Oklahoma")
  } else if (zip_prefix == 74) {
    return("Oklahoma")
  } else if (zip_prefix == 75) {
    return("Texas")
  } else if (zip_prefix == 76) {
    return("Texas")
  } else if (zip_prefix == 77) {
    return("Texas")
  } else if (zip_prefix == 78) {
    return("Texas")
  } else if (zip_prefix == 79) {
    return("Texas")
  } else if (zip_prefix == 80) {
    return("Colorado")
  } else if (zip_prefix == 81) {
    return("Colorado")
  } else if (zip_prefix == 82) {
    return("Wyoming")
  } else if (zip_prefix == 83) {
    return("Idaho")
  } else if (zip_prefix == 84) {
    return("Utah")
  } else if (zip_prefix == 85) {
    return("Arizona")
  } else if (zip_prefix == 86) {
    return("Arizona")
  } else if (zip_prefix == 87) {
    return("New Mexico")
  } else if (zip_prefix == 88) {
    return("New Mexico")
  } else if (zip_prefix == 89) {
    return("Nevada")
  } else if (zip_prefix == 90) {
    return("California")
  } else if (zip_prefix == 91) {
    return("California")
  } else if (zip_prefix == 92) {
    return("California")
  } else if (zip_prefix == 93) {
    return("California")
  } else if (zip_prefix == 94) {
    return("California")
  } else if (zip_prefix == 95) {
    return("California")
  } else if (zip_prefix == 96) {
    return("California")
  } else if (zip_prefix == 97) {
    return("Oregon")
  } else if (zip_prefix == 98) {
    return("Washington")
  } else if (zip_prefix == 99) {
    return("Washington/Alaska")
  } else {
    return("Unknown")
  }
}

# Add state column based on zip code
merged_data <- merged_data %>%
  mutate(State = sapply(Zip_Code, get_state_from_zip))

# Handle missing values by replacing NA with "Unknown" for clarity
merged_data <- merged_data %>%
  mutate(across(where(is.character), ~replace_na(.x, "Unknown"))) %>%
  mutate(across(where(is.factor), ~replace_na(.x, "Unknown")))

# Ensure Owner_Age_Range is ordered appropriately if it exists
if ("Owner_Age_Range" %in% names(merged_data)) {
  age_levels <- c("Under 25", "25-34", "35-44", "45-54", "55-64", "65 or older", "Unknown", "Prefer not to answer")
  merged_data <- merged_data %>%
    mutate(Owner_Age_Range = factor(Owner_Age_Range, levels = age_levels))
}

# Create a new column that categorizes businesses as either "Solo Entrepreneur" or "With Employees"
merged_data <- merged_data %>%
  mutate(
    Business_Type = case_when(
      Total_Employees == "1 (just myself)" ~ "Solo Entrepreneur",
      Total_Employees %in% c("2-4", "5-9", "10 or more") ~ "With Employees",
      TRUE ~ NA_character_  # All others are marked as NA (not applicable)
    )
  )

#====================================================================
# Data Preprocessing Section
#====================================================================

merged_data <- merged_data %>%
  mutate(
    Stress_Level_Clean = suppressWarnings(as.numeric(gsub("[^0-9]", "", Stress_Level))),
    Hours_Worked_Num = case_when(
      Hours_Worked == "0 hours" ~ 0,
      Hours_Worked == "1-10 hours" ~ 5,
      Hours_Worked == "11-20 hours" ~ 15,
      Hours_Worked == "21-30 hours" ~ 25,
      Hours_Worked == "31-40 hours" ~ 35,
      Hours_Worked == "41-50 hours" ~ 45,
      Hours_Worked == "51 hours or more" ~ 55,
      TRUE ~ NA_real_
    ),
    Business_Age = suppressWarnings(as.numeric(Business_Age)),
    Business_Age_Group = case_when(
      Business_Age <= 1 ~ "0–1 years",
      Business_Age <= 3 ~ "2–3 years",
      Business_Age <= 5 ~ "4–5 years",
      Business_Age <= 10 ~ "6–10 years",
      Business_Age > 10 ~ "10+ years",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(
    Gender == "Female",
    !is.na(Stress_Level_Clean),
    !is.na(Hours_Worked),
    !is.na(Business_Type),
    !Hours_Worked %in% c("Unknown", "Prefer not to answer", "Selected"),
    !is.na(Startup_Capital),
    Business_Age_Group != "Unknown"
  )

# ================================
# Shiny App
# ================================

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Female Entrepreneurs Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("stress_input", 
                  "Select Stress Level (1 = Low, 7 = High):",
                  min = 1, max = 7, value = 4, step = 1),
      selectInput("edu_input", 
                  "Select Education Level:",
                  choices = unique(merged_data$Education_Level),
                  selected = unique(merged_data$Education_Level)[1]),
      selectInput("race_input", "Select Race:",
                  choices = sort(unique(merged_data$Race)),
                  selected = unique(merged_data$Race)[1]),
      selectInput("owner_age_input", "Select Owner Age Range:",
                  choices = unique(merged_data$Owner_Age_Range),
                  selected = "25-34")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", htmlOutput("Overview")),
        tabPanel("Working Hours % by Stress", plotOutput("stressBarPlot")),
        tabPanel("Stress by Education & Capital", plotOutput("stressByEducationCapital")),
        tabPanel("Heatmap: Marital Status x Hours Worked", plotOutput("heatmapPlot")),
        tabPanel("Count by Business Age", plotOutput("bizCountByAgePlot"))
      )
    )
  )
)
server <- function(input, output) {
  output$Overview <- renderUI({
    HTML("
    <div style='font-size:16px; line-height:1.6; color:#333; padding:10px;'>
      <h4 style='color:#4B3F72;'>Overview</h4>
      <p>This dashboard explores trends and stress factors among <strong>female entrepreneurs</strong> in the U.S. using data from the <strong>GoDaddy Venture Forward Survey</strong>, collected in <strong>Feb 2023, Aug 2023, and Feb 2024</strong>.</p>
      
      <h5 style='margin-top:20px;'>Data Snapshot</h5>
      <ul>
        <li><strong>Total Respondents:</strong> 5011 (including respondants with 'Don't Know/Prefer not to answer'
        as response)</li>
        <li><strong>Top Education Level:</strong> Bachelor’s Degree</li>
        <li><strong>Most Common Business Type:</strong> Solo Entrepreneurs</li>
        <li><strong>Top Startup Capital Range:</strong> $1001 - $5,000</li>
        <li><strong>Top Hours Worked Range:</strong> 1-10 hours/week</li>
        <li><strong>Average Female Stress Level:</strong> 4.28 (on a scale of 1–7)</li>
        <li><strong>Average Female Business Age:</strong> 7.82 years</li>
        <li><strong>Top Marital Status:</strong> Married </li>
      </ul>

      <h5 style='margin-top:20px;'><strong>Key Variables Used</h5>
      <p><strong>The visualizations explore:</p>
      <ul>
        <li><strong>Stress Level</strong> (1–7 scale)</li>
        <li><strong>Startup Capital</strong> (categorical ranges)</li>
        <li><strong>Hours Worked</strong> (grouped weekly ranges)</li>
        <li><strong>Business Age</strong> (derived from launch year)</li>
        <li><strong>Education, Marital Status, Race, Age Range</strong></li>
      </ul>

      <p style='margin-top:20px; font-style:italic;'>Use the interactive filters to analyze trends and uncover insights based on your selection.</p>
    </div>
  ")
  })
  
 #Graph 1 
 output$stressBarPlot <- renderPlot({
  plot_data <- merged_data %>%
    filter(
      Stress_Level_Clean == input$stress_input,
      Business_Type %in% c("Solo Entrepreneur", "With Employees"),
      !is.na(Hours_Worked),
      Hours_Worked %in% c(
        "0 hours", "1-10 hours", "11-20 hours", "21-30 hours",
        "31-40 hours", "41-50 hours", "51 hours or more"
      )
    ) %>%
    mutate(Hours_Worked = factor(Hours_Worked, levels = c(
      "0 hours", "1-10 hours", "11-20 hours", "21-30 hours", 
      "31-40 hours", "41-50 hours", "51 hours or more"
    )))
  print(paste("Sample size for Working Hours % by Stress:", nrow(plot_data)))
  
  
  if (nrow(plot_data) == 0) {
    ggplot() +
      annotate("text", x = 1, y = 1, label = "No data available for this stress level.", size = 6) +
      theme_void()
  } else {
    percent_data <- plot_data %>%
      group_by(Hours_Worked, Business_Type) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Hours_Worked) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1))
    
    ggplot(percent_data, aes(x = Hours_Worked, y = Percentage, fill = Business_Type)) +
      geom_col(position = "stack") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5),
                size = 3.5, fontface = "bold", color = "white") +
      scale_fill_manual(values = c(
        "Solo Entrepreneur" = "#58A4B0",
        "With Employees" = "#644E7E"
      )) +
      labs(
        title = paste("Breakdown of Business Types by Hours Worked at Stress Level", input$stress_input),
        subtitle = "Percentage of Female Entrepreneurs by Business Type within Each Hour Category",
        x = "Hours Worked per Week",
        y = "Percentage of Respondents",
        fill = "Business Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12)
      )
  }
})
  
  
  #Graph 2
  output$stressByEducationCapital <- renderPlot({
    plot_data <- merged_data %>%
      filter(
        Gender == "Female",
        Education_Level == input$edu_input,
        !is.na(Startup_Capital),
        !is.na(Stress_Level_Clean)
      )
    
    ggplot(plot_data, aes(x = Startup_Capital, y = Stress_Level_Clean, fill = Startup_Capital)) +
      geom_boxplot() +
      scale_fill_manual(
        values = rep(c("#4B3F72", "#E6DBD0", "#58A4B0", "#F9E0D9", "#644E7E"), 
                     length.out = length(unique(plot_data$Startup_Capital)))
      ) +
      labs(
        title = paste("Stress Level by Startup Capital"),
        subtitle = paste("Education Level:", input$edu_input),
        x = "Startup Capital Category",
        y = "Stress Level (1 = Low, 7 = High)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  
  #Graph 3
  output$heatmapPlot <- renderPlot({
    hours_levels <- c(
      "0 hours", "1-10 hours", "11-20 hours", "21-30 hours",
      "31-40 hours", "41-50 hours", "51 hours or more"
    )
    
    plot_data <- merged_data %>%
      filter(
        Gender == "Female",
        Race == input$race_input,
        !is.na(Marital_Status), !Marital_Status %in% c("Unknown", "Prefer not to answer", "Selected"),
        !is.na(Hours_Worked), !Hours_Worked %in% c("Unknown", "Prefer not to answer", "Selected")
      ) %>%
      mutate(
        Hours_Worked = factor(Hours_Worked, levels = hours_levels),
        Marital_Status = as.factor(Marital_Status)
      )
    
    marital_levels <- levels(droplevels(plot_data$Marital_Status))
    all_combos <- expand.grid(
      Marital_Status = marital_levels,
      Hours_Worked = hours_levels
    )
    
    count_data <- plot_data %>%
      count(Marital_Status, Hours_Worked)
    
    heatmap_data <- all_combos %>%
      left_join(count_data, by = c("Marital_Status", "Hours_Worked")) %>%
      mutate(n = replace_na(n, 0)) %>%
      group_by(Marital_Status) %>%
      mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
      ungroup()
    
    ggplot(heatmap_data, aes(x = Marital_Status, y = Hours_Worked, fill = Percentage)) +
      geom_tile(color = "white") +
      geom_text(aes(label = paste0(Percentage, "%")), size = 4, fontface = "bold") +
      scale_fill_gradient(low = "#E6DBD0", high = "#4B3F72") +
      labs(
        title = "Distribution of Female Entrepreneurs by Marital Status and Hours Worked",
        subtitle = paste("Race:", input$race_input),
        x = "Marital Status", y = "Hours Worked",
        fill = "Percentage"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_blank()
      )
  })
  
  
  # Graph 4
  output$bizCountByAgePlot <- renderPlot({
    plot_data <- merged_data %>%
      filter(
        Gender == "Female",
        Owner_Age_Range == input$owner_age_input,
        !is.na(Business_Age_Group),
        !Business_Age_Group %in% c("Unknown", "Prefer not to answer")
      ) %>%
      count(Business_Age_Group, name = "Count") %>%
      mutate(Business_Age_Group = factor(Business_Age_Group, levels = c(
        "0–1 years", "2–3 years", "4–5 years", "6–10 years", "10+ years"
      )))
    
    ggplot(plot_data, aes(x = Business_Age_Group, y = Count, fill = Business_Age_Group)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = Count), vjust = -0.5, fontface = "bold", size = 4) +
      scale_fill_manual(values = rep(c("#4B3F72", "#E6DBD0", "#58A4B0", "#F9E0D9", "#644E7E"), length.out = 5)) +
      labs(
        title = paste("Number of Businesses by Age"),
        subtitle = paste("Owner Age Range:", input$owner_age_input),
        x = "Business Age Group",
        y = "Number of Businesses"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  })
}
shinyApp(ui = ui, server = server)
