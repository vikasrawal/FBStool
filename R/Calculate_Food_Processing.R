
Calculate_Food_Processing <- function(input,output,session){

 

dbg_print <- function(x) {
  print(paste0("NEWBAL (", COUNTRY, "): ", x))
}


files = dir("SUA-FBS Balancing/R",full.names = TRUE)

for(i in files){
  source(i, local = TRUE)
}

# t=as.character(input$endyear)


t=as.numeric(as.numeric(2010) : as.numeric(input$endyear))

basedir <-getwd()

start_time <- Sys.time()


sapply(dir("SUA-FBS Balancing/R", full.names = TRUE), source)
COUNTRY <- as.character(unique(countryData$CountryM49))


dbg_print("parameters")


#BALANCING_METHOD <- swsContext.computationParams$balancing_method
BALANCING_METHOD <- "proportional"

#THRESHOLD_METHOD <- swsContext.computationParams$threshold_method
THRESHOLD_METHOD <- 'share'

# FIX_OUTLIERS <- TRUE

FIX_OUTLIERS <- FALSE

#FILL_EXTRACTION_RATES<-as.logical(swsContext.computationParams$fill_extraction_rates)
FILL_EXTRACTION_RATES <- FALSE

YEARS <- as.character(t)


p <- defaultStandardizationParameters()

p$itemVar <- "measuredItemSuaFbs"
p$mergeKey[p$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
p$elementVar <- "measuredElementSuaFbs"
p$childVar <- "measuredItemChildCPC"
p$parentVar <- "measuredItemParentCPC"
p$createIntermetiateFile <- "TRUE"
p$protected <- "Protected"
p$official <- "Official"

# shareDownUp_file <-
#   file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_", COUNTRY, ".csv"))

TourismNoIndustrial = read.csv("SUA-FBS Balancing/Data/TourismNoIndustrial.csv")



# Always source files in R/ (useful for local runs).
# Your WD should be in faoswsStandardization/


# Flags set by this plugin. These are temporary: once the testing phase is over,
# most of these should be changed to I,e, E,e, I,i. See the email sent.

dbg_print("define functions")

######### FUNCTIONS: at some point, they will be moved out of this file. ####


# Replacement for merge(x, y, by = VARS, all.x = TRUE) that do not set keys
# By default it behaves as dplyr::left_join(). If nomatch = 0, non-matching
# rows will not be returned



dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]
  
  setcolorder(res, c(names(x), setdiff(names(y), names(x))))
  
  res
}

dt_full_join <- function(x, y, by = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }
  
  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }
  
  res <- merge(x, y, by = by, all = TRUE)
  
  # merge sets the key to `by`
  setkey(res, NULL)
  
  res
}









# This function will recalculate opening stocks from the first
# observation. Always.


update_opening_stocks <- function(x) {
  x <- x[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]
  
  groups <- unique(x[, .(geographicAreaM49, measuredItemSuaFbs)])
  
  res <- list()
  
  for (i in seq_len(nrow(groups))) {
    z <- x[groups[i], on = c("geographicAreaM49", "measuredItemSuaFbs")]
    if (nrow(z) > 1) {
      for (j in seq_len(nrow(z))[-1]) {
        # negative delta cannot be more than opening
        if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
          z$delta[j-1] <- - z$new_opening[j-1]
        }
        z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
      }
      # negative delta cannot be more than opening
      if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
        z$delta[j] <- - z$new_opening[j]
      }
    }
    res[[i]] <- z
  }
  rbindlist(res)
}


# Fill NAs by LOCF/FOCB/interpolation if more than two
# non-missing observations are available, otherwhise just
# replicate the only non-missing observation
na.fill_ <- function(x) {
  if(sum(!is.na(x)) > 1) {
    zoo::na.fill(x, "extend")
  } else {
    rep(x[!is.na(x)], length(x))
  }
}



`%!in%` <- Negate(`%in%`)


# RemainingToProcessedParent() and RemainingProdChildToAssign() will
# be used in the derivation of shareDownUp

RemainingToProcessedParent<-function(data){
  data[, 
       parent_already_processed:=ifelse(is.na(parent_qty_processed),parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  data[,
       only_child_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                 is.na(processed_to_child) &
                                 !is.na(production_of_child) &
                                 !is.na(parent_qty_processed) & 
                                 production_of_child>0,TRUE,FALSE),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[only_child_left==TRUE,processed_to_child:=remaining_processed_parent*extractionRate]
  
  data[,
       parent_already_processed:=ifelse(is.na(parent_qty_processed), parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  
  return(data)
}


RemainingProdChildToAssign<-function(data){
  
  data[,
       available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
  ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  
  data[,
       only_parent_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                  is.na(processed_to_child) &
                                  !is.na(parent_qty_processed) & 
                                  parent_qty_processed>=0,TRUE,FALSE)
  ]
  
  data[only_parent_left==TRUE,processed_to_child:=0]
  
  data[,available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
  ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  return(data)
  
}


# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)
  
  if (plusor) {
    found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))
    
    if (found == 0) {
      return(max(share, na.rm = TRUE))
    } else if (found == 1) {
      return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
    } else { # should be 2
      return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
    }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}

# Function that calculates imbalance as supply - utilizations (both calculated
# inside the function, dropped if keep_(supply|utilizations) set to FALSE).
calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
  
  stopifnot(is.data.table(data))
  
  data[,
       `:=`(
         supply =
           sum(Value[measuredElementSuaFbs %chin% supply_add],
               - Value[measuredElementSuaFbs %chin% supply_subtract],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
               na.rm = TRUE)
       ),
       by = bygroup
  ][,
    imbalance := supply - utilizations
  ]
  
  if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}

outside <- function(x, lower = NA, upper = NA) {
  x < lower | x > upper
}




################### optim stuff ###########################

my_fun <- function(s) {
  
  zero_if_na <- function(x) {
    ifelse(length(x) == 0, 0, ifelse(is.na(x), 0, x))
  }
  
  prod_i <- x[, measuredElementSuaFbs == 'production']
  impo_i <- x[, measuredElementSuaFbs == 'imports']
  expo_i <- x[, measuredElementSuaFbs == 'exports']
  stoc_i <- x[, measuredElementSuaFbs == 'stockChange']
  food_i <- x[, measuredElementSuaFbs == 'food']
  feed_i <- x[, measuredElementSuaFbs == 'feed']
  seed_i <- x[, measuredElementSuaFbs == 'seed']
  proc_i <- x[, measuredElementSuaFbs == 'foodManufacturing']
  indu_i <- x[, measuredElementSuaFbs == 'industrial']
  loss_i <- x[, measuredElementSuaFbs == 'loss']
  tour_i <- x[, measuredElementSuaFbs == 'tourist']
  resi_i <- x[, measuredElementSuaFbs == 'residual']
  
  prod_v <- zero_if_na(x$Value[prod_i])
  impo_v <- zero_if_na(x$Value[impo_i])
  expo_v <- zero_if_na(x$Value[expo_i])
  stoc_v <- zero_if_na(x$Value[stoc_i])
  food_v <- zero_if_na(x$Value[food_i])
  feed_v <- zero_if_na(x$Value[feed_i])
  seed_v <- zero_if_na(x$Value[seed_i])
  proc_v <- zero_if_na(x$Value[proc_i])
  indu_v <- zero_if_na(x$Value[indu_i])
  loss_v <- zero_if_na(x$Value[loss_i])
  tour_v <- zero_if_na(x$Value[tour_i])
  resi_v <- zero_if_na(x$Value[resi_i])
  
  #  prod_p <- x$Protected[prod_i] ; prod_p <- ifelse(length(prod_p) == 0, 0, ifelse(is.na(prod_p), 0, prod_p))
  #  impo_p <- x$Protected[impo_i] ; impo_p <- ifelse(length(impo_p) == 0, 0, ifelse(is.na(impo_p), 0, impo_p))
  #  expo_p <- x$Protected[expo_i] ; expo_p <- ifelse(length(expo_p) == 0, 0, ifelse(is.na(expo_p), 0, expo_p))
  #  stoc_p <- x$Protected[stoc_i] ; stoc_p <- ifelse(length(stoc_p) == 0, 0, ifelse(is.na(stoc_p), 0, stoc_p))
  #  food_p <- x$Protected[food_i] ; food_p <- ifelse(length(food_p) == 0, 0, ifelse(is.na(food_p), 0, food_p))
  #  feed_p <- x$Protected[feed_i] ; feed_p <- ifelse(length(feed_p) == 0, 0, ifelse(is.na(feed_p), 0, feed_p))
  #  seed_p <- x$Protected[seed_i] ; seed_p <- ifelse(length(seed_p) == 0, 0, ifelse(is.na(seed_p), 0, seed_p))
  #  proc_p <- x$Protected[proc_i] ; proc_p <- ifelse(length(proc_p) == 0, 0, ifelse(is.na(proc_p), 0, proc_p))
  #  indu_p <- x$Protected[indu_i] ; indu_p <- ifelse(length(indu_p) == 0, 0, ifelse(is.na(indu_p), 0, indu_p))
  #  loss_p <- x$Protected[loss_i] ; loss_p <- ifelse(length(loss_p) == 0, 0, ifelse(is.na(loss_p), 0, loss_p))
  #  tour_p <- x$Protected[tour_i] ; tour_p <- ifelse(length(tour_p) == 0, 0, ifelse(is.na(tour_p), 0, tour_p))
  #  resi_p <- x$Protected[resi_i] ; resi_p <- ifelse(length(resi_p) == 0, 0, ifelse(is.na(resi_p), 0, resi_p))
  
  supply <- prod_v + impo_v - expo_v - stoc_v
  
  #  utilizations <-
  #    (food_v * !food_p) * s[1] +
  #    (feed_v * !feed_p) * s[2] +
  #    (seed_v * !seed_p) * s[3] +
  #    (proc_v * !proc_p) * s[4] +
  #    (indu_v * !indu_p) * s[5] +
  #    (loss_v * !loss_p) * s[6] +
  #    (tour_v * !tour_p) * s[7] +
  #    (resi_v * !resi_p) * s[8]
  
  utilizations <-
    food_v * s[1] +
    feed_v * s[2] +
    seed_v * s[3] +
    proc_v * s[4] +
    indu_v * s[5] +
    loss_v * s[6] +
    tour_v * s[7] +
    resi_v * s[8]
  
  abs(supply - utilizations)
}


do_optim <- function(d) {
  
  elem_names <- c('food', 'feed', 'seed', 'foodManufacturing',
                  'industrial', 'loss', 'tourist', 'residual')
  
  my_lower <- d$min_adj
  my_upper <- d$max_adj
  
  my_protected <- d$Protected
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- d$measuredElementSuaFbs
  
  my_lower <- my_lower[elem_names]
  my_upper <- my_upper[elem_names]
  my_protected <- my_protected[elem_names]
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- elem_names
  
  my_lower[is.na(my_lower) | my_protected == TRUE] <- 0.999999
  my_upper[is.na(my_upper) | my_protected == TRUE] <- 1.000001
  
  # XXX this used to be &, but we want now all 1s so it chan change
  initial <- (my_lower | my_upper ) * 1.0
  
  #initial[initial < 1] <- 0.00000001
  
  opt <- optim(initial, my_fun, method = "L-BFGS-B",
               lower = my_lower, upper = my_upper,
               control = list(pgtol = 0.0001))
  
  opt$par <- opt$par * !dplyr::near(opt$par, 0, 0.0000001)
  
  res <- reshape2::melt(as.list(opt$par))
  
  setDT(res)
  
  setnames(res, c("value", "L1"), c("adj", "measuredElementSuaFbs"))
  
  # Round to keep things actually different from 1.
  res[, adj := ifelse(dplyr::near(adj, 1, tol = 0.0001), 1, adj)]
  
  return(res)
}


balance_optimization <- function(d) {
  myres_optim <- do_optim(d)
  
  res <- myres_optim[d[, list(measuredElementSuaFbs, Value)],
                     on = c('measuredElementSuaFbs')]
  
  res[, adj := adj * Value]
  
  res[dplyr::near(adj, 0) | dplyr::near(adj, 1), adj := NA_real_]
  
  return(res$adj)
}

################### / optim stuff ###########################

balance_proportional <- function(data) {
  
  x <- copy(data)
  
  x <-
    x[
      can_balance == TRUE,
      c("measuredElementSuaFbs", "Value",
        "mov_share", "imbalance", "min_threshold",
        "max_threshold", "can_balance"),
      with = FALSE
    ]
  
  mov_share_sum <- sum(x$mov_share, na.rm = TRUE)
  
  if (mov_share_sum > 0) {
    # Recalculate mov_share, as we are excluding protected values
    x[, mov_share := mov_share / mov_share_sum]
  } else {
    # It means that there are no back shares, so use current values
    x[!is.na(Value) & can_balance == TRUE, mov_share := Value / sum(Value, na.rm = TRUE)]
  }
  
  x[is.na(Value), Value := 0]
  
  x[, adjusted_value := 0]
  
  x[Value + mov_share * imbalance >= 0, adjusted_value := Value + mov_share * imbalance]
  
  x[adjusted_value > Value & adjusted_value > max_threshold, adjusted_value := max_threshold]
  
  x[adjusted_value < Value & adjusted_value < min_threshold, adjusted_value := min_threshold]
  
  x <-  x[, c("measuredElementSuaFbs", "adjusted_value"), with = FALSE][data, on = "measuredElementSuaFbs"]
  
  return(as.numeric(x$adjusted_value))
}

# rollavg() is a rolling average function that uses computed averages
# to generate new values if there are missing values (and FOCB/LOCF).
# I.e.:
# vec <- c(NA, 2, 3, 2.5, 4, 3, NA, NA, NA)
#
#> RcppRoll::roll_mean(myvec, 3, fill = 'extend', align = 'right')
#[1]       NA       NA       NA 2.500000 3.166667 3.166667       NA       NA       NA 
#
#> rollavg(myvec)
#[1] 2.000000 2.000000 3.000000 2.500000 4.000000 3.000000 3.166667 3.388889 3.185185

rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  
  non_missing <- sum(!is.na(x))
  
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
    
    x <- zoo::na.fill(x, 'extend')
  }
  
  return(x)
}


newBalancing <- function(data, Utilization_Table) {
  
  # Contains a variable that indicates whether stocks changed
  data[, change_stocks := NA_integer_]
  
  # XXX define "food residual" those items for which the only utilization
  # is food. Food processing can also be another possible utilization and
  # if there is that does not change its food-residualness, this is why
  # the check is done before assigning food processing.
  # NW: I have commented the conditions out
  # Now it only checks to make sure that food is the only utilization
  # We noticed that some of the food items were missing from the utilization table
  # This is still different from the previous approach of assigning all of the imbalance to 
  # food when "none of the other utilizations are activable"
  data[,
       food_resid :=
         # It's a food item & ...
         (measuredItemSuaFbs %chin% Utilization_Table[food_item == 'X', cpc_code] |
            # food exists & ...
            # !is.na(Value[measuredElementSuaFbs == 'food']) &
            Food_Median > 0 & !is.na(Food_Median)) &
         # ... is the only utilization
         all(is.na(Value[!(measuredElementSuaFbs %chin%
                             c('loss', 'food', 'production', 'imports',
                               'exports', 'stockChange','foodManufacturing', 'tourist'))])),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  # Checking if the commodity has past value before assigning the residual
  # imbalance at the end of the balancing procees
  data[,
       `:=`(
         feed_resid =
           # It's a feed item or have past value & ...
           #(measuredItemSuaFbs %in% Utilization_Table[feed == 'X', cpc_code] |
           (Feed_Median > 0 & !is.na(Feed_Median)) &
           #feed is the only utilization....
           all(is.na(Value[!(measuredElementSuaFbs %chin%
                               c('feed', 'production', 'imports', 'exports',
                                 'stockChange','foodManufacturing'))])),
         # It's a industrial item or have past value & ...
         industrial_resid = Industrial_Median > 0 & !is.na(Industrial_Median)),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ] 
  
  data[,
       supply :=
         sum(
           Value[measuredElementSuaFbs %chin% c('production', 'imports')],
           - Value[measuredElementSuaFbs %chin% c('exports')],
           na.rm = TRUE
         ),
       by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  # When production needs to be created
  data[
    Protected == FALSE &
      # Only primary
      measuredItemSuaFbs %chin% Utilization_Table[primary_item == "X"]$cpc_code &
      measuredElementSuaFbs == 'production' &
      supply < 0 &
      stockable == FALSE,
    `:=`(
      Value = ifelse(is.na(Value), 0, Value) - supply,
      flagObservationStatus = "E"
      # flagMethod = "c"
    )
  ]
  
  calculateImbalance(data)
  
  # Try to assign the maximum of imbalance to stocks
  # NOTE: in the conditions below, 2 was 0.2, indicating that no more than
  # 20% should go to stocks. Now, the condition was relaxed a lot (200%)
  data <-
    dt_left_join(
      data,
      all_opening_stocks[,
                         .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
                           timePointYears, opening_stocks = Value)
      ],
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    )
  
  data[,
       Value_0 := ifelse(is.na(Value), 0, Value)
  ][
    Protected == FALSE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "stockChange" &
      stockable == TRUE,
    change_stocks :=
      # The numbers indicate the case. Assignmnet (value and flags) will be done below
      case_when(
        # case 1: we don't want stocks to change sign.
        sign(Value_0) * sign(Value_0 + imbalance) == -1                                                  ~ 1L,
        # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
        Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks           ~ 2L,
        # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
        Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks            ~ 3L,
        # case 4: if value + imbalance send LESS than 200% of supply, send all
        Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 2) ~ 4L,
        # case 5: if value + imbalance send MORE than 200% of supply, send 200% of supply
        Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 2)  ~ 5L
      )
  ]
  
  data[change_stocks == 1L, Value := 0]
  data[change_stocks == 2L, Value := Value_0 + imbalance]
  data[change_stocks == 3L, Value := - opening_stocks]
  data[change_stocks == 4L, Value := Value_0 + imbalance]
  # Only case for which grouping is required
  data[
    change_stocks == 5L,
    Value := max(supply * 2 - opening_stocks, 0),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ]
  
  data[
    change_stocks %in% 1L:5L,
    `:=`(flagObservationStatus = "E")
  ]
  
  data[, Value_0 := NULL]
  
  data[, opening_stocks := NULL]
  
  # Recalculate imbalance
  calculateImbalance(data)
  
  # Assign imbalance to food if food "only" (not "residual") item
  data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
      flagObservationStatus = "E"
      # flagMethod = "h"
    )
  ]
  
  for (j in 1:10) {
    
    # Recalculate imbalance
    calculateImbalance(data)
    
    data[, can_balance := FALSE]
    
    data[
      !is.na(Value) &
        Protected == FALSE &
        !(data.table::between(Value, min_threshold, max_threshold, incbounds = FALSE) %in% FALSE) &
        !(measuredElementSuaFbs %chin%
            c("production", "imports", "exports", "stockChange", "foodManufacturing")),
      can_balance := TRUE
    ]
    
    data[,
         elements_balance := any(can_balance),
         by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    ]
    
    
    
    if (nrow(data[dplyr::near(imbalance, 0) == FALSE & elements_balance == TRUE]) == 0) {
      break()
    } else {
      data[
        dplyr::near(imbalance, 0) == FALSE &
          elements_balance == TRUE,
        adjusted_value := balance_proportional(.SD),
        by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
      ]
      
      
      
      data[
        !is.na(adjusted_value) & !dplyr::near(adjusted_value, Value),
        `:=`(
          Value = adjusted_value,
          flagObservationStatus = "E"
          # flagMethod = "-"
        )
      ]
      
      data[, adjusted_value := NULL]
    }
    
  }
  
  # At this point the imbalance (in the best case scenario) should be zero,
  # the following re-calculation is useful only for debugging
  
  calculateImbalance(data)
  
  # Assign imbalance to food if food "only" (not "residual") item
  data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, 0)
        ),
      flagObservationStatus = "E"
      # flagMethod = "h"
    )
  ]
  
  calculateImbalance(data)
  
  # Assign the residual imbalance to industrial if the conditions are met
  data[
    Protected == FALSE &
      industrial_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "industrial",
    `:=`(
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, Value)
        ),
      flagObservationStatus = "E"
      # flagMethod = "b"
    )
  ]
  
  if (COUNTRY %in% TourismNoIndustrial) {
    
    adj_tour_ind <-
      data.table::dcast(
        data[
          measuredItemSuaFbs %chin% Utilization_Table[food_item == "X"]$cpc_code &
            measuredElementSuaFbs %chin% c("industrial", "tourist")
        ],
        geographicAreaM49 + timePointYears + measuredItemSuaFbs ~ measuredElementSuaFbs,
        value.var = "Value"
      )
    
    adj_tour_ind <- adj_tour_ind[industrial > 0]
    
    adj_tour_ind[, new_tourist := industrial]
    adj_tour_ind[!is.na(tourist), new_tourist := tourist + industrial]
    
    adj_tour_ind[, c("industrial", "tourist") := NULL]
    
    by_vars <- c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    
    data <- dt_left_join(data, adj_tour_ind, by = by_vars)
    
    data[
      measuredElementSuaFbs %in% c("industrial", "tourist"),
      any_protected := any(Protected == TRUE),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    ]
    
    data[
      !is.na(new_tourist) & measuredElementSuaFbs == "tourist" & any_protected == FALSE,
      `:=`(
        Value = new_tourist,
        flagObservationStatus = "E"
        # flagMethod = "e"
      )
    ]
    
    data[
      !is.na(new_tourist) & measuredElementSuaFbs == "industrial" & any_protected == FALSE,
      `:=`(
        Value = 0,
        flagObservationStatus = "E"
        # flagMethod = "e"
      )
    ]
    
    data[, c("any_protected", "new_tourist") := NULL]
  }
  
  calculateImbalance(data)
  
  # Assign the residual imbalance to feed if the conditions are met
  data[
    Protected == FALSE &
      feed_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "feed",
    `:=`(
      # XXX: this creates a warning when no assignment is done:
      # Coerced 'logical' RHS to 'double'
      Value =
        ifelse(
          is.na(Value) & imbalance > 0,
          imbalance,
          ifelse(Value + imbalance >= 0, Value + imbalance, Value)
        ),
      flagObservationStatus = "E"
      # flagMethod = "b"
    )
  ]
  
  calculateImbalance(data)
  
  data[, c("supply", "utilizations", "imbalance", "mov_share_rebased") := NULL]
  
  return(data)
}



############################## / FUNCTIONS ##################################


dbg_print("end functions")






#####################################  TREE #################################

dbg_print("download tree")


# tree <- getCommodityTreeNewMethod(COUNTRY, YEARS)
# 
# stopifnot(nrow(tree) > 0)
# 
# tree <- tree[geographicAreaM49 %chin% COUNTRY]
# 
# 
# 
# # The `tree_exceptions` will npo be checked by validateTree()
# 
# # Exception: high share conmfirmed by official data
# tree_exceptions <- tree[geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01"]
# 
# if (nrow(tree_exceptions) > 0) {
#   tree <- tree[!(geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01")]
# }
# 
# validateTree(tree)
# 
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/Data/tree.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/Data/tree.csv"))
#   write.csv(tree,"SUA-FBS Balancing/Data/tree.csv", row.names =FALSE)
# }
# 
# 
# 
# stopifnot(nrow(tree) > 0)
#  
# tree <- tree[geographicAreaM49 %chin% COUNTRY]
# # 
# # 
# # # The `tree_exceptions` will npo be checked by validateTree()
# # 
# # # Exception: high share conmfirmed by official data
# tree_exceptions <- tree[geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01"]
# # 
# if (nrow(tree_exceptions) > 0) {
#   tree <- tree[!(geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01")]
# }
# # 
# validateTree(tree)
# # 
# if (nrow(tree_exceptions) > 0) {
#   tree <- rbind(tree, tree_exceptions)
#   rm(tree_exceptions)
# }




tree <- fread("SUA-FBS Balancing/Data/tree.csv")

tree <- subset(tree, timePointYears %in% c(2010:input$endyear) )  #fill extraction rate is deactivated.

#when pulling data to tool, fill extraction rate function is ran. so no need to rerun again. 

if (COUNTRY == "835"){
  
  tree[, geographicAreaM49 := as.character(835)]
  
}




# 
# ## NA ExtractionRates are recorded in the sws dataset as 0
# ## for the standardization, we nee them to be treated as NA
# ## therefore here we are re-changing it
# 
tree[Value == 0, Value := NA]


# 
# #proc_level_exceptions <- ReadDatatable("processing_level_exceptions")
# #
# #if (nrow(proc_level_exceptions) > 0) {
# #  setnames(proc_level_exceptions, c("m49_code", "parent", "child"),
# #           c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC"))
# #
# #  tree <-
# #    tree[!proc_level_exceptions[is.na(level)],
# #         on = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")]
# #
# #  proc_level_exceptions <- proc_level_exceptions[!is.na(level)]
# #}
# 
# tree_to_send <- tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]
# 
if (FILL_EXTRACTION_RATES == TRUE) {
  
  expanded_tree <-
    merge(
      data.table(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = sort(unique(tree$timePointYears))
      ),
      unique(tree[, .(geographicAreaM49, measuredElementSuaFbs,
                      measuredItemParentCPC, measuredItemChildCPC)]),
      by = "geographicAreaM49",
      all = TRUE,
      allow.cartesian = TRUE
    )
  
  tree <- tree[expanded_tree, on = colnames(expanded_tree)]
  
  # flags for carry forward/backward
  tree[is.na(Value), c("flagObservationStatus", "flagMethod") := list("E", "t")]
  
  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = -Inf
    ]
  
  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = Inf
    ]
  
  # keep orig flags
  tree[, flagObservationStatus := i.i.flagObservationStatus]
  tree[, flagMethod := i.i.flagMethod]
  
  tree[, names(tree)[grep("^i\\.", names(tree))] := NULL]
}

# 
# # saveRDS(
# #   tree[
# #     !is.na(Value) & measuredElementSuaFbs == "extractionRate",
# #     -grepl("measuredElementSuaFbs", names(tree)),
# #     with = FALSE
# #     ],
# #   file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "tree.rds")
# # )
# 
# tree_to_send <-
#   tree_to_send %>% 
#   dplyr::anti_join(tree[is.na(Value) & measuredElementSuaFbs == "extractionRate"], by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod")) %>%
#   dplyr::select(-Value) %>%
#   dplyr::left_join(tree, by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "flagObservationStatus", "flagMethod")) %>%
#   setDT()
# 
# tree_to_send <-
#   tree_to_send[,
#                .(geographicAreaM49, measuredElementSuaFbs, measuredItemParentCPC,
#                  measuredItemChildCPC, timePointYears, Value, flagObservationStatus, flagMethod)]
# 
# setnames(
#   tree_to_send,
#   c("measuredItemParentCPC", "measuredItemChildCPC"),
#   c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
# )
# 
# tree_to_send <-
#   nameData("suafbs", "ess_fbs_commodity_tree2", tree_to_send, except = c('measuredElementSuaFbs', 'timePointYears'))
# 
# tree_to_send[,
#              `:=`(
#                measuredItemParentCPC_tree = paste0("'", measuredItemParentCPC_tree),
#                measuredItemChildCPC_tree = paste0("'", measuredItemChildCPC_tree))
#              ]
# 
# tmp_file_name_extr <- tempfile(pattern = paste0("FILLED_ER_", COUNTRY, "_"), fileext = '.csv')
# 
# # write.csv(tree_to_send, tmp_file_name_extr)
# 
# # XXX remove NAs
tree <- tree[!is.na(Value)]




# if (unique(tree$geographicAreaM49))


### Update tree by setting some edges to "F"
#FPCommodities <- c("01499.06", "01921.01")
#
## These commodities are forwards processed instead of backwards processed:
##        code             description type
## 3: 01499.06      Kapokseed in shell CRNP
## 4: 01921.01   Seed cotton, unginned CRPR
#
#tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities, "F", "B")]

uniqueLevels <- unique(tree[, list(geographicAreaM49, timePointYears)])

levels <- list()

treeLevels <- list()

for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  setnames(levels, "temp", "measuredItemParentCPC")
  
  treeLevels[[i]] <- merge(treeCurrent, levels, by = c("measuredItemParentCPC"), all.x = TRUE)
}

tree <- rbindlist(treeLevels)




# XXX there are no different process levels, but check it
tree[,
     processingLevel := max(processingLevel, na.rm = TRUE),
     by = c("geographicAreaM49", "timePointYears",
            "measuredElementSuaFbs", "measuredItemChildCPC")
]



# XXX Check if this one is still good or it can be obtained within the dataset
processed_item_datatable <- fread("SUA-FBS Balancing/Data/processed_item_datatable.csv")
processedCPC <- processed_item_datatable[, measured_item_cpc]


# XXX what is this for?
itemMap <- fread("SUA-FBS Balancing/Data/itemMap.csv")


itemMap <- itemMap[, list(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################


coproduct_table <- fread("SUA-FBS Balancing/Data/zeroweight_coproducts.csv")



elemKeys <- c("5510", "5610", "5071", "5113", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")

Utilization_Table <- fread("SUA-FBS Balancing/Data/utilization_table_2018.csv")



stockable_items <- Utilization_Table[stock == 'X', cpc_code]

zeroWeight <- fread("SUA-FBS Balancing/Data/zeroWeight.csv")$x



# flagValidTable <- ReadDatatable("valid_flags")


flagValidTable=fread("SUA-FBS Balancing/Data/flagValidTable.csv")

flagValidTable[, flagObservationStatus := as.factor(flagObservationStatus)]



dbg_print("download data")


data_2010 <- get(load("Data/countrySUA.RData"))

data_2010[!is.na(Value) & is.na(Flag), Flag := ""]

# if (update  == TRUE){
#   
#   data_2010 <- copy(df_sua_unbalanced$data_sua_unbalanced)
#   
#   
#   data_2010 <- long_format(data_2010)
#   
# }

#commented for Zanzibar
data_2000 <- get(load("Data/countrySUA_2000_2009.RData"))



# data <- rbind(data_2010)

#commented for zanzibar
data <- rbind(data_2010,data_2000)



# LOAD
# data <- readRDS(paste0('c:/Users/mongeau.FAODOMAIN/tmp/new_balancing/data_', COUNTRY, '.rds'))

data[,c("Country","Commodity","Element") := NULL]

setnames(data,c("CountryM49","ElementCode","CPCCode","Year","Value","Flag"),
         c("geographicAreaM49","measuredElementSuaFbs","measuredItemFbsSua","timePointYears","Value","flagObservationStatus")
)


# data <- subset(data, timePointYears %in% t)

#################### FODDER CROPS ##########################################

# Some of these items may be missing in reference files,
# thus we carry forward the last observation.
fodder_crops_items <-
  tibble::tribble(
    ~description, ~code,
    "Maize for forage", "01911",
    "Alfalfa for forage", "01912",
    "Sorghum for forage", "01919.01",
    "Rye grass for forage", "01919.02",
    "Other grasses for forage", "01919.91",
    "Clover for forage", "01919.03",
    "Other oilseeds for forage", "01919.94",
    "Other legumes for  forage", "01919.92",
    "Cabbage for fodder", "01919.04",
    "Mixed grass and legumes for forage", "01919.93",
    "Turnips for fodder", "01919.05",
    "Beets for fodder", "01919.06",
    "Carrots for fodder", "01919.07",
    "Swedes for fodder", "01919.08",
    "Other forage products, nes", "01919.96",
    "Other forage crops, nes", "01919.95",
    "Hay for forage, from legumes", "01919.10",
    "Hay for forage, from grasses", "01919.09",
    "Hay for forage, from other crops nes", "01919.11"
  )


fodder_crops_availab <-
  data[
    measuredItemFbsSua %in% fodder_crops_items$code &
      measuredElementSuaFbs == "5510"
  ]

if (nrow(fodder_crops_availab) > 0) {
  
  fodder_crops_complete <-
    CJ(
      geographicAreaM49 = unique(fodder_crops_availab$geographicAreaM49),
      measuredElementSuaFbs = "5510",
      timePointYears = unique(data$timePointYears),
      measuredItemFbsSua = unique(fodder_crops_availab$measuredItemFbsSua)
    )
  
  fodder_crops_complete <-
    fodder_crops_complete[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]
  
  fodder_crops <-
    merge(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears"),
      all.x = TRUE
    )
  
  fodder_crops[,
               Value := zoo::na.locf(Value),
               by = c("geographicAreaM49", "measuredItemFbsSua")
  ]
  
  fodder_crops_new <-
    fodder_crops[
      !fodder_crops_availab,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
    ]
  
  
  if (nrow(fodder_crops_new) > 0) {
    
    # fodder_crops_new[, `:=`(flagObservationStatus = "E", flagMethod = "t")]
    
    fodder_crops_new[, `:=`(flagObservationStatus = "E")]
    
    data <- rbind(data, fodder_crops_new)
    
  }
}

#################### / FODDER CROPS ########################################


#Giulia


# opening_stocks_2014 <-
#   ReadDatatable(
#     "opening_stocks_2014",
#     where = paste0("m49_code IN (",
#                    paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
#   )
# 
# stopifnot(nrow(opening_stocks_2014) > 0)
# 
# 
# if(file.exists(paste0(basedir,"/SUA-FBS Balancing/Data/opening_stocks_2014.csv"))){
#   file.remove(paste0(basedir,"/SUA-FBS Balancing/Data/opening_stocks_2014.csv"))
#   write.csv(opening_stocks_2014,"SUA-FBS Balancing/Data/opening_stocks_2014.csv",row.names = FALSE)
# }
# 
# 
# 
# 
# non_null_prev_deltas <-
#   unique(
#     data[
#       measuredElementSuaFbs == "5071" & timePointYears %in% 2009:2013
#       ][,
#         .SD[sum(!dplyr::near(Value, 0)) > 0],
#         by = c("geographicAreaM49", "measuredItemFbsSua")
#         ][,
#           .(geographicAreaM49, measuredItemFbsSua)
#           ]
#   )
# 
# opening_stocks_2014[opening_stocks < 0, opening_stocks := 0]
# 
# opening_stocks_2014 <-
#   opening_stocks_2014[
#     m49_code %in% COUNTRY,
#     .(
#       geographicAreaM49 = m49_code,
#       measuredItemFbsSua = cpc_code,
#       timePointYears = "2014",
#       Value_cumulated = opening_stocks
#     )
#     ]
# 
# # Keep only those for which recent variations are available
# opening_stocks_2014 <-
#   opening_stocks_2014[
#     non_null_prev_deltas,
#     on = c("geographicAreaM49", "measuredItemFbsSua")
#     ]

#giulia


original_opening_stocks <- data[measuredElementSuaFbs == "5113"]

original_opening_stocks[, flagMethod := NULL]

original_opening_stocks <-
  flagValidTable[
    original_opening_stocks,
    on = c("flagObservationStatus")
  ][,
    Valid := NULL
  ]


# Remove protected in 2014 from cumulated


#Giulia


# opening_stocks_2014 <-
#   opening_stocks_2014[
#     !original_opening_stocks[timePointYears == "2014" & Protected == TRUE, .(geographicAreaM49, measuredItemFbsSua)],
#     on = c("geographicAreaM49", "measuredItemFbsSua")
#     ]
# 
# 
# all_opening_stocks <-
#   merge(
#     original_opening_stocks,
#     opening_stocks_2014,
#     by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
#     all = TRUE
#   )
# 
# 
# all_opening_stocks[
#   !Protected %in% TRUE & is.na(Value) & !is.na(Value_cumulated),
#   `:=`(
#     Value = Value_cumulated,
#     flagObservationStatus = "I",
#     # flagMethod = "-",
#     # We protect these, in any case, because they should not
#     # be overwritten, even if not (semi) official or expert
#     Protected = TRUE,
#     measuredElementSuaFbs = "5113",
#     timePointYears = "2014"
#   )
#   ][,
#     Value_cumulated := NULL
#     ]
# 

# Now, for all remaining stockable items, we create opening
# stocks in 2014 as 20% of supply in 2013

# remaining_opening_stocks <-
#   data[
#     timePointYears == "2013" &
#       measuredItemFbsSua %chin%
#       setdiff(
#         stockable_items,
#         all_opening_stocks$measuredItemFbsSua
#       )
#     ][,
#       .(
#         opening_20 =
#           sum(
#             Value[measuredElementSuaFbs %chin% c("5510", "5610")],
#             - Value[measuredElementSuaFbs == "5910"],
#             na.rm = TRUE
#           ) * 0.2,
#         timePointYears = "2014"
#       ),
#       by = c("geographicAreaM49", "measuredItemFbsSua")
#       ]
# 
# remaining_opening_stocks[opening_20 < 0, opening_20 := 0]
# 
# all_opening_stocks <-
#   merge(
#     all_opening_stocks,
#     remaining_opening_stocks,
#     by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
#     all = TRUE
#   )


all_opening_stocks<- copy(original_opening_stocks)



all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]

# all_opening_stocks[
#   !Protected %in% TRUE & is.na(Value) & !is.na(opening_20),
#   `:=`(
#     Value = opening_20,
#     flagObservationStatus = "I",
#     # flagMethod = "i",
#     # We protect these, in any case, because they should not
#     # be overwritten, even if not (semi) official or expert
#     Protected = TRUE
#   )
#   ][,
#     opening_20 := NULL
#     ]



##################################################################################################

# complete_all_opening <-
#   CJ(
#     geographicAreaM49 = unique(all_opening_stocks$geographicAreaM49),
#     timePointYears = as.character(min(all_opening_stocks$timePointYears):2017),
#     measuredItemFbsSua = unique(all_opening_stocks$measuredItemFbsSua)
#   )


# all_opening_stocks <-
#   merge(
#     complete_all_opening,
#     all_opening_stocks,
#     by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
#     all = TRUE
#   )
# 
# all_opening_stocks[, orig_val := Value]

# all_opening_stocks <-
#   all_opening_stocks[!is.na(Value)][
#     all_opening_stocks,
#     on = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
#     roll = Inf]
# 
# all_opening_stocks[
#   is.na(orig_val) & !is.na(Value),
#   `:=`(
#     Protected = FALSE,
#     flagObservationStatus = "E"
#     
#   )
#   ]

# all_opening_stocks[, measuredElementSuaFbs := "5113"]

# all_opening_stocks[, orig_val := NULL]

# all_opening_stocks[, names(all_opening_stocks)[grep("^i\\.", names(all_opening_stocks))] := NULL]
# 
# all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]

#Giulia




# all_opening_stocks[, new_opening := NULL]

# / Recalculate opening stocks



data <- merge(data, flagValidTable, by = c("flagObservationStatus"), all.x = TRUE)

data[flagObservationStatus %in% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]

data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]

dbg_print("elementToCodeNames")

# XXX FIXME: the elementCodesToNames below is
# not working proberply, see issue #38
codes <- tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodmanufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stock_change"
)

data <- merge(data, codes, by = "measuredElementSuaFbs", all.x = TRUE)

data[, measuredElementSuaFbs := name]

data[, name := NULL]

#data <-
#  elementCodesToNames(
#    data,
#    itemCol = "measuredItemFbsSua",
#    elementCol = "measuredElementSuaFbs"
#  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")

dbg_print("convert sugar")

# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)

## XXX: check for max processing level
#level <- findProcessingLevel(tree, from = p$parentVar,
#                            to = p$childVar, aupusParam = p)


#primaryEl <- level[processingLevel == 0, get(p$itemVar)]

## XXX what is this?
#data[
#  !(get(p$protected) == TRUE | (flagObservationStatus == "I" & flagMethod %in% c("i", "e"))) &
#    get(p$elementVar) == p$productionCode &
#    !(get(p$itemVar) %chin% primaryEl),
#  Value_xxx := NA
#]


#data[,
#  availability :=
#    sum(
#      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
#      - Value[get(p$elementVar) %in% p$exportCode],
#      na.rm = TRUE
#    ),
#  by = c(p$mergeKey)
#]




########## Remove feed if new element and negative imbalance is huge

# Feed requires country-specific reference files, which are being updated.
# Until these get reviewed, the feed module will generate feed items in
# some countries, where it is not required/needed/appropriate. Below, we
# remove the NEW feed item if the NEGATIVE imbalance obtained by including
# it is more than 50% of supply (e.g., -72%).

#Giulia





########## / Remove feed if new element and negative imbalance is huge


treeRestricted <- tree[, .(measuredItemParentCPC, measuredItemChildCPC, processingLevel)]
treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])

primaryInvolved <- getPrimary(processedCPC, treeRestricted, p)

dbg_print("primary involved descendents")

# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )


# stocks need to be generated for those items for
# which "opening stocks" are available

items_to_generate_stocks <-
  unique(all_opening_stocks$measuredItemFbsSua)

#giulia
# stock <-
#   CJ(
#     measuredItemSuaFbs    = items_to_generate_stocks,
#     measuredElementSuaFbs = 'stock_change',
#     geographicAreaM49     = unique(data$geographicAreaM49),
#     timePointYears        = unique(data$timePointYears)
#   )
# 
# # rbind with anti_join
# data <-
#   rbind(
#     data,
#     stock[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
#                         'geographicAreaM49', 'timePointYears')],
#     fill = TRUE
#   )

# XXX what is primaryInvolvedDescendents ?????????
#deriv <- CJ(measuredItemSuaFbs = primaryInvolvedDescendents, measuredElementSuaFbs = 'production', geographicAreaM49 = unique(data$geographicAreaM49), timePointYears = unique(data$timePointYears))
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )



data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]



data[, stockable := measuredItemSuaFbs %chin% stockable_items]

#giulia



# stockable items for which a historical series of at least
# 5 non-missing/non-null data points exist
historical_avail_stocks <-
  data[
    measuredElementSuaFbs == "stock_change" &
      timePointYears <= 2013 &
      !is.na(Value) &
      stockable == TRUE,
    .(n = sum(!dplyr::near(Value, 0))),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ][
    n >= 5
  ][,
    n := NULL
  ]


# Keep processingShare and shareDownUp
computed_shares <- list()
computed_shares_send <- list()
# Keep negative availability
negative_availability <- list()

#keep shareuPdOWN
updated_shareUpDOwn<-list()

fixed_proc_shares <- list()

original_stock_variation <-
  data[
    measuredElementSuaFbs == "stock_change" & timePointYears >= 2014 & !is.na(Value),
    .(
      geographicAreaM49, measuredItemSuaFbs, timePointYears, Value, flagObservationStatus
    )
  ]





########################  


dbg_print("Calculate Food Processing ")



treeProc <-
  tree[
    !is.na(Value) &
      # processingLevel == lev &
      measuredElementSuaFbs == 'extractionRate',
    list(
      measuredItemParentCPC,
      geographicAreaM49,
      measuredItemChildCPC,
      timePointYears,
      extractionRate = Value
    )
  ]
#     
#     #tree containing children of all parent of current level
tree_parent_Level<-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate' &
      measuredItemParentCPC %in% treeProc[, get(p$parentVar)],
    list(
      measuredItemParentCPC,
      geographicAreaM49,
      measuredItemChildCPC,
      timePointYears,
      extractionRate = Value
      # processingLevel
    )
  ]


dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stock_change')]

data_proc <- copy(dataMergeTree)

setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")

dataMergeTree[, geographicAreaM49 := as.character(geographicAreaM49)]
tree_parent_Level[, geographicAreaM49 := as.character(geographicAreaM49)]
tree_parent_Level[, timePointYears := as.character(timePointYears)]



dataMergeTree <-
  merge(
    dataMergeTree,
    tree_parent_Level,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )



dataMergeTree[,
              availability :=
                sum(
                  Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
                  # XXX p$stockCode is "stockChange", not "stock_change"
                  - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
                  na.rm = TRUE
                ),
              by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
]
#     
#     dbg_print("negative availability")
#     
# negative_availability[[as.character(lev)]] <-
#   unique(
#     dataMergeTree[
#       availability < -100,
#       list(
#         country            = geographicAreaM49,
#         year               = timePointYears,
#         measuredItemFbsSua = measuredItemParentCPC,
#         element            = measuredElementSuaFbs,
#         Value,
#         flagObservationStatus,
#         # flagMethod,
#         availability
#       )
#       ]
#   )
#     
#     dbg_print("zero out negative availability")
#     
#     # XXX: Replace negative availability, so we get zero production, instead of negative. This, , however, should be fixed in advance, somehow.
dataMergeTree[availability < 0, availability := 0]


dataMergeTree[,
              availabilitieChildEquivalent := availability * extractionRate #,
              #by = c(params$geoVar, params$yearVar, params$parentVar, params$childVar, "measuredElementSuaFbs")
]


dataMergeTree[,
              sumAvail := sum(unique(na.omit(availabilitieChildEquivalent))),
              by = c(p$childVar, p$yearVar, p$geoVar)  #, "measuredElementSuaFbs"
]

dataMergeTree[, shareDownUp := availabilitieChildEquivalent / sumAvail]

dataMergeTree[shareDownUp > 1, shareDownUp := 1]
dataMergeTree[shareDownUp < 0, shareDownUp := 0]
dataMergeTree[is.nan(shareDownUp), shareDownUp := 0]


setkey(dataMergeTree, NULL)

dataMergeTree <-
  unique(
    dataMergeTree[,
                  list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                       measuredItemChildCPC, extractionRate, availability, shareDownUp)
    ]
  )


tree_with_shareDWN <- copy(dataMergeTree)

#data_proc includes data for the current level

setnames(data_proc, p$itemVar, p$childVar)


tree_with_shareDWN = merge(tree_with_shareDWN, data_proc, by = c(p$geoVar,p$yearVar, p$childVar), all.x = TRUE)

tree_with_shareDWN[, weight := ifelse(get(p$childVar) %in% zeroWeight,0,1)]

# dataMergeTree[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
tree_with_shareDWN[, foodProcElement:= ((Value/extractionRate)*shareDownUp)*weight]

tree_with_shareDWN <- tree_with_shareDWN[measuredElementSuaFbs == "production"]


food_proc_table=tree_with_shareDWN[,list(measuredItemSuaFbs=get(p$parentVar),geographicAreaM49 = get(p$geoVar) ,timePointYears = get(p$yearVar)
                                         ,foodProcElement)]





food_proc_table <-  food_proc_table[, 
                                    list(Value = sum(foodProcElement, na.rm = TRUE)), 
                                    by = list(geographicAreaM49,measuredItemSuaFbs, timePointYears)]


food_proc_table <- food_proc_table[Value > 0]

food_proc_table <- food_proc_table[!is.na(Value)]


if (dim(food_proc_table)[1] != 0){
  
  proc_to_bind <- food_proc_table[, list(geographicAreaM49,measuredItemSuaFbs,timePointYears,Value,
                                         measuredElementSuaFbs = p$foodProcCode, flagObservationStatus ="E", Valid = TRUE
                                         , Protected = TRUE, Official = FALSE)]
  
  proc_to_bind[, stockable := measuredItemSuaFbs %chin% stockable_items]
  
  proc_to_bind <- unique(proc_to_bind)
  
  data <- rbind(data, proc_to_bind)
  
  
  
}

dbg_print("end Food Processing")

food_processing <- data[measuredElementSuaFbs == "foodManufacturing"]

food_processing <- food_processing[, c("measuredItemSuaFbs","timePointYears", "Value", "flagObservationStatus")]

food_processing[,ElementCode := "5023"]

setnames(food_processing,c("measuredItemSuaFbs","timePointYears","flagObservationStatus"),
         c("CPCCode","Year","Flag"))

food_processing <- merge(food_processing,all_cpc, by= c("CPCCode"),all.x = T)
food_processing <- merge(food_processing,all_elements, by= c("ElementCode"),all.x = T)

setcolorder(food_processing,c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))

food_processing <-subset(food_processing, Year %in% t)



return(food_processing)

}