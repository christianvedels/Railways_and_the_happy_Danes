# Functions
#
# Date updated:   2024-04-07
# Auhtor:         Christian Vedel 
# Purpose:        Functions used for this part of the project

# ==== sub_scandi ====
# this substitutes scandinavian letters
sub_scandi = function(x){
  scandi_letters = c("Æ",
                     "æ",
                     "Ø",
                     "ø",
                     "Å",
                     "å")
  
  replacement = c("Ae",
                  "ae",
                  "Oe",
                  "oe",
                  "Aa",
                  "aa")
  
  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }
  
  return(x)
  
}


# Function to parse JSON safely
safe_fromJSON = function(x) {
  require(jsonlite)
  if (is.na(x) || x == "{}" || x == '[NA]') {
    return(NA)
  } else {
    return(fromJSON(x))
  }
}

# Function to clean and parse the data
clean_aps_data = function(df) {
  df = df %>%
    mutate(
      # Replace single quotes with double quotes in the Emotion Scores column
      `Emotion Scores` = str_replace_all(`Emotion Scores`, "'", "\""),
      # Parse Object Location as list of numerics
      `Object Location` = map(`Object Location`, ~safe_fromJSON(paste0("[", .x, "]"))),
      # # Parse Face Location as list of numerics, handling "NA" correctly
      `Face Location` = map(`Face Location`, ~safe_fromJSON(paste0("[", .x, "]"))),
      # Parse Emotion Scores as a list, handling "{}" correctly
      `Emotion Scores` = map(`Emotion Scores`, ~safe_fromJSON(.x))
    ) %>%
    rename(
      emotion_score = `Emotion Scores`,
      object_locaiton = `Object Location`,
      face_location = `Face Location`
    ) %>% 
    # Flatten Emotion Scores into separate columns
    unnest_wider(emotion_score, names_sep = "_")
  
  return(df)
}

read_aps_data = function(x){
  df = read_csv(
    x,
    col_types = cols(
      Image = col_character(),
      Detected = col_character(),
      `Object Location` = col_character(),
      Confidence = col_double(),
      `Face Detected` = col_character(),
      `Face Location` = col_character(),
      `Dominant Emotion` = col_character(),
      `Emotion Scores` = col_character()
    )
  )
  df1 = clean_aps_data(df)
  
  return(df1)
}


# Round0
round0 = function(x, s){
  return(floor(x/s)*s)
}

# ==== demean ====
demean = function(x){
  return(x - mean(x, na.rm = TRUE))
}
