library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(rmarkdown)

# Load dataset directly 
questions <- read_excel("questions_dataset.xlsx")

# Function to randomly select questions based on set criteria
select_questions <- function(questions, Subsection, //počet vybrnaných otázek z každé subsekce a z toho každý Level??~ n_XXX//) {
  data %>%
    filter(Subsection == Subsection) %>%
    group_by(Level) %>%
    summarise(Sample = list(sample(Question, 
                                   size = min(case_when(
                                     Question_type == "1" ~ n_XXX,
                                     Question_type == "2" ~ n_XXX,
                                     Question_type == "3" ~ n_XXX,
                                     TRUE ~ 0), n()), replace = FALSE))) %>%
    ungroup() %>%
    select(-Question_type) %>%
    tidyr::unnest(Sample)}

# Function to create question-answer pair with the subsection
generate_question_answer_pairs <- function(questions, Subsection, //počet vybrnaných otázek z každé subsekce a z toho každý Level??~ n_XXX//) {
  results <- lapply(Subsection, function(sub) {
    selected <- select_questions(questions, Subsection = sub, XXX)
    selected %>%
      mutate(Subsection = sub) # Retain subsection info for further appl.
  })
  do.call(rbind, results)
}

# Extract unique subsections from the dataset
subsections <- unique(questions$Subsection)

# Randomize and select questions
selected_questions <- generate_question_answer_pairs(
  questions, subsections, 1 = 3, 2 = 2, 3 = 1
)

# Split into questions for students and answers for pimps
questions_for_students <- selected_questions %>%
  select(Subsection, Question)

answers_for_correction <- selected_questions %>%
  left_join(questions, by = c("Subsection", "Question")) %>%
  select(Subsection, Question, Answer)

# Save to PDF
generate_pdf <- function(questions, answers, output_file) {
  temp_file <- tempfile(fileext = ".Rmd")
  
  # Create R Markdown content
  writeLines(c(
    "---",
    "title: \"Zkouška ze Zoologie\"",
    "output: pdf_document1",
    "---",
    "",
    "# Otázky pro studenty",
    "",
    knitr::kable(questions, format = "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position")),
    "",
    "# Správné odpovědi typ XXX",
    "",
    knitr::kable(answers, format = "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position"))
  ), temp_file)
  
  # Render to PDF
  rmarkdown::render(temp_file, output_file = output_file)
}

# Generate the PDF
generate_pdf(
  questions = questions_for_students,
  answers = answers_for_correction,
  output_file = "Randomized_Exam_Paper.pdf"
)