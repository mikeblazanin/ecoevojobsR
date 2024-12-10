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

colnames(carnegie_dat)[1] <- "Official Carnegie Institution Name"
carnegie_dat <- cbind(data.frame("Alias" = carnegie_dat$`Official Carnegie Institution Name`),
                      carnegie_dat)



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
  Location = my_state_name[match(`State abbreviation`, my_state_abb)],
  'Carnegie Name State' = paste(`Official Carnegie Institution Name`, Location))
carnegie_dat <- select(carnegie_dat,
                       "Alias", "Official Carnegie Institution Name",
                       "Location", "State abbreviation", "Carnegie Name State",
                       everything())

#Add aliases created using automatic rules ----
#Chosen rules include:
# (note that all rules need to be replicated in gsheet equation)

# Remove "The" at beginning
auto_aliases <- mutate(carnegie_dat,
                     Alias = gsub("^The ", "", Alias))

# Remove " at ", "-", ", ", " - "
#   Carnegie uses " at " or "-" with no spaces,
#   but ecoevo sometimes uses " at" or ", " or " " or " - "
auto_aliases <- mutate(auto_aliases,
                     Alias = gsub(" at |-", " ", Alias))

# Replace "St." and "St" with "Saint"
auto_aliases <- mutate(auto_aliases,
                     Alias = gsub("^St\\.? ", "Saint ", Alias),
                     Alias = gsub(" St\\.? ", " Saint ", Alias))

# Replace "&" with "and"
auto_aliases <- mutate(auto_aliases,
                       Alias = gsub(" ?& ?", " and ", Alias))

auto_aliases <- filter(auto_aliases,
                     Alias != `Official Carnegie Institution Name`)

#Add manually curated aliases ----
man_aliases <- read.csv("./data-raw/aliases.csv", fileEncoding = "UTF-8",
                    strip.white = TRUE, check.names = FALSE)
man_aliases <- man_aliases[order(man_aliases$checked, man_aliases$ecoevo_names), ]
man_aliases <- mutate(
  filter(man_aliases, !is.na(carnegie_names), include_gsheetdb == "Y"),
  Alias = ecoevo_names,
 'Carnegie Name State' =
    paste(carnegie_names, my_state_name[match(`State_abbreviation`, my_state_abb)]))

#(not all manual aliases should be added to db)
man_aliases_toadd <-
  carnegie_dat[
    match(man_aliases$`Carnegie Name State`, carnegie_dat$`Carnegie Name State`), ]
man_aliases_toadd$Alias <- man_aliases$Alias

#Join ----
carnegie_dat <- rbind(carnegie_dat, auto_aliases, man_aliases_toadd)

carnegie_dat <- carnegie_dat[order(carnegie_dat$`Carnegie Name State`), ]

carnegie_dat <- select(carnegie_dat,
                       -"State abbreviation", -"Carnegie Name State")
carnegie_dat <- select(carnegie_dat,
                       "Alias", "Official Carnegie Institution Name",
                       "Location", ,
                       everything())
carnegie_dat <- cbind(data.frame("Key" = ""), carnegie_dat)

##Write ----
write.csv(carnegie_dat, "./data-raw/carnegiedb_withaliases.csv",
          row.names = FALSE)

#Steps
#1. import carnegiedb_withaliases.csv as sheet
#2. rename sheet to "db"
#3. In Key column of db sheet, enter: =CONCATENATE(B2, " ",D2)
#4. expand to all rows
#5. Add column to Faculty/Permanent Jobs Sheet titled "Classification"
#6. In "Classification" column, enter:
#      =IF(ISNUMBER(MATCH(CONCATENATE(TRIM(B3)," ",TRIM(C3)), db!A$2:A$102040, 0)), VLOOKUP(CONCATENATE(TRIM(B3)," ",TRIM(C3)), db!A$2:CT$102040,5,FALSE), VLOOKUP(CONCATENATE(REGEXREPLACE(REGEXREPLACE(REGEXREPLACE(REGEXREPLACE(REGEXREPLACE(TRIM(B3),"^The |^the ",""), " at | - |, |-"," "),"^St\.? ", "Saint "), " St\.? ", " Saint "), " ?& ?", " and ")," ",TRIM(C3)),db!A$2:CT$102040,5,FALSE))
#7. expand to all rows

#TODO:
# Set up code to pull aliases that are added to the gsheet
