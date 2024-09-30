library(gsheet)
library(dplyr)
library(readxl)

#Read in Carnegie data sheets ----
carnegie_val <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Values")
colnames(carnegie_val) <- c("Variable", "Label", "Value", "Value_Label")
carnegie_val <- carnegie_val[!is.na(carnegie_val$Value_Label), ]

carnegie_dat <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Data")
carnegie_dat <- dplyr::select(carnegie_dat,
                              !c(basic2000, basic2005, basic2010,
                                 basic2015, basic2018))

carnegie_var <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Variable")

#Fill in empty rows ----
fill_vals <- function(x) {
  if(is.na(x[1])) {stop("first value must not be NA")}
  for (i in 1:length(x)) {if(is.na(x[i])) {x[i] <- x[i-1]}}
  return(x)
}
carnegie_val$Variable <- fill_vals(carnegie_val$Variable)
carnegie_val$Label <- fill_vals(carnegie_val$Label)

carnegie_val <- dplyr::filter(carnegie_val,
                              !Variable %in% c("Basic2000", "BASIC2005",
                                               "BASIC2010", "BASIC2015",
                                               "BASIC2018"))
#Label R1 and R2 ----
carnegie_val <- dplyr::mutate(
  carnegie_val,
  Value_Label = ifelse(Variable == "BASIC2021",
                       ifelse(Value == 15,
                              paste("R1", Value_Label),
                              ifelse(Value == 16,
                                     paste("R2", Value_Label),
                                     Value_Label)),
                       Value_Label))

carnegie_dat <- as.data.frame(carnegie_dat)
carnegie_val <- as.data.frame(carnegie_val)
carnegie_var <- as.data.frame(carnegie_var)

#Convert numeric codes into meaningful labels ----
for(i in 1:ncol(carnegie_dat)) {
  col <- colnames(carnegie_dat)[i]
  if(any(grepl(pattern = paste0("^", col, "$"), carnegie_val$Variable,
               ignore.case = TRUE))) {
    temp <- carnegie_val[grep(pattern = paste0("^", col, "$"), carnegie_val$Variable,
                              ignore.case = TRUE), ]
    carnegie_dat[, i] <- temp$Value_Label[match(carnegie_dat[, i], temp$Value)]
  }
}

#Update column names to be long-form understandable labels ----
colnames(carnegie_dat) <-
  sapply(colnames(carnegie_dat),
         mytable = carnegie_var$Variable,
         mylabels = carnegie_var$Label,
         function(clnm, mytable, mylabels) {
           mylabels[grep(pattern = paste0("^", clnm, "$"),
                         x = mytable, ignore.case = TRUE)]})

carnegie_dat <- select(carnegie_dat,
                       "Institution name",
                       "State abbreviation",
                       "2021 Basic Classification",
                       everything())

colnames(carnegie_dat)[1] <- "Carnegie Institution Name"
carnegie_dat <- cbind(data.frame("Alias" = carnegie_dat$`Carnegie Institution Name`),
                      carnegie_dat)

#Modify aliases to account for variants ----
#Manually chosen rules include:
# (note that all rules need to be replicated in gsheet equation)

# Remove "The" at beginning
carnegie_dat <- mutate(carnegie_dat,
                       Alias = gsub("^The ", "", Alias))

# Remove " at ", "-", ", ", " - "
#   Carnegie uses " at " or "-" with no spaces,
#   but ecoevo sometimes uses ", " or " " or " - "
carnegie_dat <- mutate(carnegie_dat,
                       Alias = gsub(" at |-", " ", Alias))

# Replace "St." and "St" with "Saint"
carnegie_dat <- mutate(carnegie_dat,
                       Alias = gsub("^St\\.? ", "Saint ", Alias),
                       Alias = gsub(" St\\.? ", " Saint ", Alias))

#Concatenate state at end of name
# the my_state_name values are set by the form options for the
# Submit Job form in the ecoevo spreadsheet
my_state_name <- c(state.name,
                   "Puerto Rico", "Asia (Other)", "District of Columbia",
                   "Asia (Other)", "Asia (Other)", "Asia (Other)",
                   "Asia (Other)", "Asia (Other)", "South America (Other)")
my_state_abb <- c(state.abb,
                  "PR", "AS", "DC", "FM", "MH", "GU", "MP", "PW", "VI")

carnegie_dat <- mutate(
  carnegie_dat,
  Alias = paste(Alias, my_state_name[match(`State abbreviation`, my_state_abb)]),
  'Carnegie Name State' = paste(
    `Carnegie Institution Name`,
    my_state_name[match(`State abbreviation`, my_state_abb)]))
carnegie_dat <- select(carnegie_dat,
                       "Alias", "Carnegie Institution Name",
                       "State abbreviation", "Carnegie Name State",
                       everything())

#Add manually curated aliases ----
aliases <- read.csv("./data-raw/aliases.csv", fileEncoding = "UTF-8",
                    strip.white = TRUE, check.names = FALSE)
aliases <- aliases[order(aliases$checked, aliases$ecoevo_names), ]
aliases <- mutate(
  filter(aliases, !is.na(carnegie_names), include_gsheetdb == "Y"),
  Alias =
    paste(ecoevo_names, my_state_name[match(`State_abbreviation`, my_state_abb)]),
  nonmanual_alias_tomatch =
    paste(carnegie_names, my_state_name[match(`State_abbreviation`, my_state_abb)]))

aliases_toadd <-
  carnegie_dat[
    match(aliases$nonmanual_alias_tomatch, carnegie_dat$`Carnegie Name State`), ]
aliases_toadd$Alias <- aliases$ecoevo_names

carnegie_dat <- rbind(carnegie_dat, aliases_toadd)

##Write ----
write.csv(carnegie_dat, "./data-raw/carnegiedb_withaliases.csv",
          row.names = FALSE)

