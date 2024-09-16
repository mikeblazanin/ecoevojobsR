library(gsheet)
library(dplyr)
library(readxl)

#These are the links for the 2022-2023 sheet
jobs <- list(
  faculty =
    read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1PnS-tHYXNVPaSfbXT5v9qZl0T7QHH4AtwoyIJSRQ5a0/edit?gid=76501376#gid=76501376",
                                format = 'csv'),
             stringsAsFactors = FALSE, strip.white = TRUE),
  postdoc =
    read.csv(text = gsheet2text("https://docs.google.com/spreadsheets/d/1PnS-tHYXNVPaSfbXT5v9qZl0T7QHH4AtwoyIJSRQ5a0/edit?gid=1368888587#gid=1368888587",
                                format = 'csv'),
             stringsAsFactors = FALSE, strip.white = TRUE)
)

#Drop first row, make 2nd row new headers
jobs <- lapply(jobs, function(x){colnames(x) <- x[1, ]; x <- x[-1, ]})
#Drop Mod flag and all following columns
jobs <- lapply(jobs, function(x){return(x[, 1:(which(colnames(x) == "Mod Flag")-1)])})

jobs <- lapply(jobs,
               FUN = function(x) {
                 return(x[!is.na(x$Timestamp) & x$Timestamp != "", ])})

#Read in Carnegie data sheets
carnegie_val <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Values")
colnames(carnegie_val) <- c("Variable", "Label", "Value", "Value_Label")
carnegie_val <- carnegie_val[!is.na(carnegie_val$Value_Label), ]

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
#Label R1 and R2
carnegie_val <- dplyr::mutate(
  carnegie_val,
  Value_Label = ifelse(Variable == "BASIC2021",
                       ifelse(Value == 15,
                              paste("R1", Value_Label),
                              ifelse(Value == 16,
                                     paste("R2", Value_Label),
                                     Value_Label)),
                       Value_Label))


carnegie_dat <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Data")
carnegie_dat <- dplyr::select(carnegie_dat,
                              !c(basic2000, basic2005, basic2010,
                              basic2015, basic2018))

carnegie_var <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Variable")

carnegie_dat <- as.data.frame(carnegie_dat)
carnegie_val <- as.data.frame(carnegie_val)
carnegie_var <- as.data.frame(carnegie_var)

aliases <- read.csv("./data-raw/aliases.csv", fileEncoding = "UTF-8",
                    strip.white = TRUE)
aliases <- aliases[order(aliases$checked, aliases$ecoevo_names), ]
write.csv(aliases, file = "./data-raw/aliases.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

#Convert numeric codes into meaningful labels
for(i in 1:ncol(carnegie_dat)) {
  col <- colnames(carnegie_dat)[i]
  if(any(grepl(pattern = paste0("^", col, "$"), carnegie_val$Variable,
               ignore.case = TRUE))) {
    temp <- carnegie_val[grep(pattern = paste0("^", col, "$"), carnegie_val$Variable,
                              ignore.case = TRUE), ]
    carnegie_dat[, i] <- temp$Value_Label[match(carnegie_dat[, i], temp$Value)]
  }
}

#Update column names to be long-form understandable labels
colnames(carnegie_dat) <-
  sapply(colnames(carnegie_dat),
         mytable = carnegie_var$Variable,
         mylabels = carnegie_var$Label,
         function(clnm, mytable, mylabels) {
           mylabels[grep(pattern = paste0("^", clnm, "$"),
                         x = mytable, ignore.case = TRUE)]})

#Match institution names
uniq_inst <- unique(c(jobs[[1]]$Institution,
                      jobs[[2]]$Institution))
find_matches <- function(jobs, carnegie, aliases) {
  #Jobs should have columns named "Institution" and "Location"
  #Carnegie should have column named "Institution name"
  jobs <- cbind(jobs, "Unmatched", NA, NA)
  colnames(jobs)[(ncol(jobs)-2):ncol(jobs)] <-
    c("Matched status", "Institution name", "State abbreviation")
  for(i in 1:nrow(jobs)) {
    myinst <- jobs$Institution[i]

    #Skip international institutions
    if(!jobs$Location[i] %in% c(state.name, "District of Columbia", "USA")) {
      jobs$`Matched status`[i] <- "International"
      next
    }

    #Do exact matching
    matches <- grep(paste0("^", myinst, "$"), carnegie$`Institution name`)
    if(length(matches) > 1) {
      #Match by state if there are multiple matches
      matches <-
        matches[match(carnegie$`State abbreviation`[matches], state.abb) ==
                  which(jobs$Location[i] == state.name)]
    }
    if(length(matches) == 1) {
      jobs$`Institution name`[i] <- carnegie$`Institution name`[matches]
      jobs$`State abbreviation`[i] <- carnegie$`State abbreviation`[matches]
      jobs$`Matched status`[i] <- "Match, exact"
    }
  }

  #For rows where plain matching failed, do partial matching
  #Manually chosen rules include:
  #   Treat "The" from beginning of Carnegie as optional
  #   Treat " at " anywhere in Carnegie as optional
  #   Carnegie uses "-" with no spaces, but ecoevo sometimes
  #     uses ", " or " " or " - "
  #Change all "St" and "St." to be "Saint"
  jobs_inst_mod <-
    trimws(gsub("^The |, | at |-| - ", " ",
                gsub("St |St\\. ", "Saint ", jobs$Institution)))
  carnegie_inst_mod <-
    trimws(gsub("^The |, | at |-| - ", " ",
                gsub("St |St\\. ", "Saint ", carnegie$`Institution name`)))

  for(i in which(is.na(jobs$`Institution name`))) {
    myinst <- jobs_inst_mod[i]
    matches <- grep(paste0("^", myinst, "$"), carnegie_inst_mod)
    if(length(matches) > 1) {
      #Match by state if there are multiple matches
      matches <-
        matches[match(carnegie$`State abbreviation`[matches], state.abb) ==
                  which(jobs$Location[i] == state.name)]
    }
    if(length(matches) == 1) {
      jobs$`Institution name`[i] <- carnegie$`Institution name`[matches]
      jobs$`State abbreviation`[i] <- carnegie$`State abbreviation`[matches]
      jobs$`Matched status`[i] <- "Match, partial"
    }
  }

  #For rows where plain and partial matching failed, do matching using
  # manually-curated list of aliases
  unmatched_names <- unique(jobs$Institution[jobs$`Matched status` == "Unmatched"])

  if(any(!is.na(aliases$carnegie_names) &
         !aliases$carnegie_names %in% carnegie$`Institution name`)) {
    warning(
      paste0("Not all aliases found in Carnegie. Check alias correctness:\n",
            paste(
              aliases$carnegie_names[
                !is.na(aliases$carnegie_names) &
                !aliases$carnegie_names %in% carnegie$`Institution name`],
                  collapse = "\n")))
  }

  #original aliases.csv file created by running
  # data.frame("ecoevo_names" = unmatched_names,
  #            "carnegie_names" = NA, "checked" = "N"))

  if(any(!unmatched_names %in% aliases$ecoevo_names)) {
    which_msng <- which(!unmatched_names %in% aliases$ecoevo_names)
    aliases <-
      rbind(aliases,
            data.frame("ecoevo_names" = unmatched_names[which_msng],
                       "carnegie_names" = NA, "checked" = "N"))
    aliases <- aliases[order(aliases$checked, aliases$ecoevo_names), ]
    print(paste("There are", length(which_msng), "new aliases which need a match"))
  }

  myrows <- which(jobs$`Matched status` == "Unmatched")
  jobs$`Institution name`[myrows] <-
    aliases$carnegie_names[match(jobs$Institution[myrows], aliases$ecoevo_names)]
  jobs$`State abbreviation`[myrows] <-
    carnegie$`State abbreviation`[
      match(jobs$`Institution name`[myrows], carnegie$`Institution name`)]

  jobs$`Matched status`[myrows][!is.na(jobs$`Institution name`[myrows])] <-
    "Match, by alias"

  #Return results
  return(list("jobs" = jobs, "aliases" = aliases))
}

jobs <- lapply(
  X = jobs, FUN = function(x, c, a) {
    find_matches(jobs = x, carnegie = c, aliases = a)},
  c = carnegie_dat, a = aliases)

for (i in 1:length(jobs)) {
  if(nrow(jobs[[i]]$aliases) > nrow(aliases)) {
    write.csv(x = jobs[[i]]$aliases[jobs[[i]]$aliases$checked == "N", ],
              file = paste0("./data-raw/aliases_new", i, ".csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  } else {file.remove(paste0("./data-raw/aliases_new", i, ".csv"))}
}

##If there are new aliases, they need to be added to aliases.csv
## and have their matching institution name saved there too

#Join carnegie_dat and jobs using matched institution name
jobs <- lapply(X = jobs,
               FUN = function(x, y) {dplyr::left_join(x = x$jobs, y = y)},
               y = carnegie_dat)

jobs <- lapply(X = jobs,
               FUN = function(x) {
                 rename(x, "Matched institution name" = "Institution name")})

mynames <- c("faculty", "postdoc")
for (i in 1:length(jobs)) {
  write.csv(jobs[[i]], paste0("./data-raw/ecoevojobsR_", mynames[i], ".csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
}

#Write date-time to file
writeLines(format(Sys.time(), usetz = TRUE), "./data-raw/lastrun.txt")
