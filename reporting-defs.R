library("reshape")

sets.to.formula <- function(cols, rows) {
  as.formula(paste( ifelse(length(cols) == 0, ".", do.call(paste, as.list(c(cols, sep="+")))) ,
                    ifelse(length(rows) == 0, ".", do.call(paste, as.list(c(rows, sep="+")))), 
                    sep="~"))  
  
}

combo.to.formula <- function (combo) {
  
  #as.formula(paste( do.call(paste, as.list(c(combo, sep="+"))) ,".", sep="~"))
  sets.to.formula(combo, c())
}


combos.list <- function (fixed=c("SCHOOL_YEAR"), choices=c("SCHOOL_ID",
                                                           "GRADE_ENROLLED",
                                                           "SUBJECT_CODE")) {
  combos <- lapply(sort(1:length(choices),decreasing=TRUE), function(i) {
    combo.mat <- combn(choices, i)
    lapply(1:dim(combo.mat)[[2]], function (j) combo.mat[,j])
  })
  
  c(lapply(do.call(c, combos[1:length(choices)]), function (x) c(fixed,x)), list(fixed))
  
}


#aggregation.combos <- combos.list()


produce.aggregates.scoped <- function (df, 
                                       ids.fixed= list(SCHOOL = c("SCHOOL_YEAR", "SCHOOL_ID"),
                                                      STATE = c("SCHOOL_YEAR")),
                                       id.choices = c(GRADE_ENROLLED="ALL", SUBJECT_CODE="ALL"),
                                       orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                        GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                       col.vars = list(SUBJECT_CODE=c("RE","MA","SC")),
                                       obs="ACCOUNTABILITY_PERF_LEVEL",
                                       value.label = NA,
                                       aggregator=function (x) {
                                         c(PERCENT_PROFICIENT =round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),
                                           N_TESTS=length(x),
                                           N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0)))
                                         },
                                       filter = NULL
)
{
  school.aggregates <- produce.aggregates(cbind(SCOPE = rep("SCHOOL", nrow(df)), df),
                                          c("SCOPE", ids.fixed[[1]]),
                                          id.choices,
                                          orderings,
                                          col.vars,
                                          obs,
                                          value.label,
                                          aggregator)
  
  find.grade.span <- function (x) {
    c(LOW_GRADE=min(as.character(x)),
      HIGH_GRADE=max(as.character(x)))
  }
  
  school.grade.spans <- cast(with(school.aggregates$tab, school.aggregates$tab[as.character(GRADE_ENROLLED) != 'ALL',c("SCHOOL_YEAR", "SCHOOL_ID", "GRADE_ENROLLED")]),
                             SCHOOL_ID + SCHOOL_YEAR~ ., fun.aggregate=find.grade.span)
  
  
  spans <- unique(school.grade.spans[, c("LOW_GRADE", "HIGH_GRADE")])

  aggregate.state.spanned <-function (i) {
    span <- spans[i,]
    low <- span[,"LOW_GRADE"]
    high <- span[,"HIGH_GRADE"]
    df.spanned <- df[df$GRADE_ENROLLED >= low & df$GRADE_ENROLLED <= high,]
    produce.aggregates(cbind(SCOPE = rep("STATE", nrow(df.spanned)),
                             LOW_GRADE = rep(low, nrow(df.spanned)),
                             HIGH_GRADE = rep(high, nrow(df.spanned)),
                             df.spanned),
                                           c("SCOPE", "LOW_GRADE", "HIGH_GRADE", ids.fixed[[2]]),
                                           id.choices,
                                           orderings,
                                           col.vars,
                                           obs,
                                           value.label,
                                           aggregator)
  }

  #hokey way of keeping some records out of the state average...
  if (!is.null(filter))
    df <- with(df, df[eval(filter),])
  
  state.aggregates <- lapply(1:nrow(spans), aggregate.state.spanned)
  
  state.aggregates <- list(do.call(rbind, lapply(state.aggregates, function (pair) if (is.null(pair)) NULL else pair[[1]])),
                 do.call(rbind, lapply(state.aggregates, function (pair) if (is.null(pair)) NULL else pair[[2]])))
  
  names(state.aggregates) <- c("norm", "tab")
  
  
  school.aggregates.state <- lapply(state.aggregates, function (agg) merge(school.grade.spans, agg))
  
  result <- list(norm = rbind(school.aggregates$norm, school.aggregates.state$norm[,names(school.aggregates$norm)]),
                  tab = rbind(school.aggregates$tab, school.aggregates.state$tab[,names(school.aggregates$tab)]))
  
  result$tab <- result$tab[do.call(order, c(lapply(c(ids.fixed[[1]], 
                                                     setdiff(names(id.choices), names(col.vars)), 
                                                     "STATISTIC", "SCOPE"), 
                                                   function (id) result$tab[[id]]))),]
  
  result$tab$ORDER <- 1:nrow(result$tab)
    
  result
}

produce.aggregates <- function (df, 
                                id.fixed=c("SCHOOL_YEAR"),                                 
                                id.choices = c(GRADE_ENROLLED="ALL", SUBJECT_CODE="ALL", SCHOOL_ID='7700000'),
                                orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                 GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),
                                col.vars = list(SUBJECT_CODE=c("RE","MA","SC")), 
                                obs="ACCOUNTABILITY_PERF_LEVEL",
                                value.label = NA,
                                aggregator=function (x) 
                                  c(PERCENT_PROFICIENT =round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),
                                    N_TESTS=length(x),
                                    N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0)))
)
{
  
  ids <- c(id.fixed, names(id.choices))
  df.molten <- melt.data.frame(df, id.vars=ids, measure.vars = obs, na.rm=TRUE)
  lapply(names(col.vars), function (col) {
    df.molten[[col]] <<- factor(df.molten[[col]], levels=col.vars[[col]])    
  })
  
  aggregate.combo <- function (combo) {      
    aggregation <- cast(df.molten[df.molten$variable==obs,], combo.to.formula(combo), aggregator, add.missing=TRUE, fill=NA)      
    ##need to recast the ids as characters, because add.missing turns everything into a factor
    lapply(combo, function (id) {
      aggregation[[id]] <<- as.character(aggregation[[id]])
    })
    aggregation
    #tail(names(aggregation), length(aggregator)) = names(aggregator)
  }
  
  aggregation.combos <- combos.list(id.fixed, names(id.choices))
  
  result <- rbind.fill(lapply(aggregation.combos, aggregate.combo))
    
  
  #if there's one return value things don't get labelled properly?
  if (!is.na(value.label))
    names(result)[length(ids)+1] = value.label
    
  #relabel NAs
  lapply(names(id.choices), function (name) {
    result[[name]] <<- ifelse(is.na(result[[name]]), id.choices[name], result[[name]])
  })
  
  #convert custom ordered columns to ordered factors
  lapply(names(orderings), function (o.name) {
    ordering <- orderings[[o.name]]
    result[[o.name]] <<- factor(result[[o.name]], levels=ordering, ordered=TRUE)    
  })
  
  #reorder so NAs are first (ie, the more aggregated results)
  result <- result[do.call(order, c(lapply(ids, function (id) result[[id]]))),]
  
#   if (!is.null(scope)) {
#     result$SCOPE <- rep(scope, nrow(result))
#     ids <- c(ids, "SCOPE")
#   }
  
  result.norm <- melt(result, ids=ids)
  names(result.norm)[(length(ids) + 1):ncol(result.norm)] = c("STATISTIC","VALUE")
  
  result.tabbed <- cast(result.norm, sets.to.formula(c(setdiff(ids, names(col.vars)), "STATISTIC"), names(col.vars)))  
  
  result.tabbed <- result.tabbed[!is.na(result.tabbed$ALL),] #get rid of empty grade levels
  result.norm <- merge(result.norm, result.tabbed[,c(setdiff(ids, names(col.vars)), "STATISTIC")])[,c(ids, "STATISTIC","VALUE")] #get rid of the corresponding rows in the normalized table  
  
  result.tabbed$ORDER <- 1:nrow(result.tabbed)
  list(norm=result.norm, tab=result.tabbed)
}


propagate.to.paired.schools <- function (df, pairings=school.pairing.lookup) {
  
  
  do.call(rbind, lapply(names(school.pairing.lookup),
                        function (school.year) {
                          lookup <- pairings[[school.year]]
                          do.call(rbind, lapply(1:length(lookup), 
                                                function (i) {
                                                  paired.school.id <- names(lookup)[i]
                                                  parent.school.id <- lookup[[i]]
                                                  
                                                  result <- df[df$SCHOOL_ID == parent.school.id &
                                                                 df$SCHOOL_YEAR == school.year,]
                                                  if (nrow(result) == 0)
                                                    NULL
                                                  else {
                                                    result$SCHOOL_ID <- rep(paired.school.id, nrow(result))
                                                    result
                                                  }
                                                  
                                                }))
                        }))
  
}