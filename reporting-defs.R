library("RODBC")
library("reshape")
require(plyr)
library("data.table")
library(devtools)

load_all('C:\\Users\\jpaul\\plyr', reset=TRUE, export_all=TRUE)

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
                                       filter = NULL,
                                       fill.val=NA,
                                       schools = NULL)
{
  school.aggregates <- produce.aggregates(cbind(SCOPE = rep("SCHOOL", nrow(df)), df),
                                          c("SCOPE", ids.fixed[[1]]),
                                          id.choices,
                                          orderings,
                                          col.vars,
                                          obs,
                                          value.label,
                                          aggregator,
                                          fill.val,
                                          schools)
  
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
                       aggregator,
                       fill.val)
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
                                    N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                fill.val=NA,
                                schools = NULL,
                                paired.schools = school.pairing.lookup[[current.school.year]]
)
{
  
  ids <- c(id.fixed, names(id.choices))
  df.molten <- melt.data.frame(df, id.vars=ids, measure.vars = obs, na.rm=TRUE)
  lapply(names(col.vars), function (col) {
    df.molten[[col]] <<- factor(df.molten[[col]], levels=col.vars[[col]])    
  })
  
  if (!is.null(schools))
    df.molten$SCHOOL_ID <- factor(df.molten$SCHOOL_ID, levels=schools$SCHOOL_ID)
  
  aggregate.combo <- function (combo) {      
    aggregation <- cast(df.molten[df.molten$variable==obs,], combo.to.formula(combo), aggregator, add.missing=TRUE, fill=fill.val)      
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
  if (is.null(schools)) {
    result.tabbed <- result.tabbed[!is.na(result.tabbed$ALL),] #get rid of empty grade levels
  } else {
    #get rid of grade levels not falling within the range of school years associated with each school    
    min.grade <- levels(result.tabbed$GRADE_ENROLLED)[2]
    max.grade <- levels(result.tabbed$GRADE_ENROLLED)[length(levels(result.tabbed$GRADE_ENROLLED))]
    old.names <- names(result.tabbed)
    #determine high and low grades for school, taking into consideration pairing
    low.grade.high.grade <- data.frame(t(apply(schools[c("SCHOOL_ID",  "LOW_GRADE", "HIGH_GRADE")],
                                               c(1),
                                               
                                               function (school) {
                                                 id <- school[["SCHOOL_ID"]]
                                                 low <- school[["LOW_GRADE"]]
                                                 high <- school[["HIGH_GRADE"]]
                                                 
                                                 paired.schools <- names(paired.schools)[which(paired.schools==id)]
                                                 if (length(paired.schools) >= 1) {                                                   
                                                   lows <- schools[schools$SCHOOL_ID %in% paired.schools, "LOW_GRADE"] 
                                                   low<-lows[order(sapply(lows, function (l) if (l %in% c('KG','PK')) '00' else sprintf("%02d",as.integer(l))))][1]
                                                   
                                                 }
                                                 
                                                 c(SCHOOL_ID=id,
                                                   LOW_GRADE=if (low %in% c('KG','PK')) '00' else sprintf("%02d",as.integer(low)),
                                                   HIGH_GRADE=if (high %in% c('KG','PK')) '00' else sprintf("%02d",as.integer(high)))                                                 
                                               })))
    
    result.tabbed.labeled <- merge(result.tabbed, low.grade.high.grade)
    
    result.tabbed <- with(result.tabbed.labeled,
                          result.tabbed.labeled[(as.character(GRADE_ENROLLED) >= LOW_GRADE &
                                                  as.character(GRADE_ENROLLED) <= HIGH_GRADE) | as.character(GRADE_ENROLLED)=='ALL',
                                                old.names])

    
  }
  
  
  result.norm <- merge(result.norm, result.tabbed[,c(setdiff(ids, names(col.vars)), "STATISTIC")])[,c(ids, "STATISTIC","VALUE")] #get rid of the corresponding rows in the normalized table  
  
  result.tabbed$ORDER <- 1:nrow(result.tabbed)
  list(norm=result.norm, tab=result.tabbed)
}


#when we don't need subject code columns (RE, MA, ALL) 
produce.aggregates.norm <- function (df, 
                                     id.fixed=c("SCHOOL_YEAR"),                                 
                                     id.choices = c(GRADE_ENROLLED="ALL", SUBJECT_CODE="ALL", SCHOOL_ID='7700000'),
                                     orderings = list(SUBJECT_CODE = c("ALL", "RE","MA","SC"),                                                 
                                                      GRADE_ENROLLED = c("ALL", "03", "04", "05", "06", "07", "08")),                                
                                     obs=c(ACCOUNTABILITY_PERF_LEVEL="character",WISER_ID="character"),
                                     aggregator=c(
                                       function (x) 
                                         c(PERCENT_PROFICIENT = if (length(x) == 0) 
                                           NA else 
                                             round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),                                      
                                           N_TESTS=length(x),
                                           N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                       N_STUDENTS=function (x) {
                                         length(unique(x))
                                       }),
                                     add.missing = TRUE
)
{
  
  ids <- c(id.fixed, names(id.choices))
  df.molten <- melt.data.frame(df, id.vars=ids, measure.vars = names(obs), na.rm=TRUE)
  
  aggregate.combo <- function (combo, obs, obs.type, aggregator, value.label) {  
    df.obs <- df.molten[df.molten$variable==obs,] #convert to numeric type if observation is numeric
        
    if(obs.type == "numeric")
      df.obs$value <- as.numeric(df.obs$value)
    aggregation <- cast(df.obs, combo.to.formula(combo), aggregator, add.missing=add.missing, fill=NA)      
    ##need to recast the ids as characters, because add.missing turns everything into a factor
    lapply(combo, function (id) {
      aggregation[[id]] <<- as.character(aggregation[[id]])
    })
    
    if (nchar(value.label) > 0) ##lets us rename (all) column in the case of a scalar aggregator
      names(aggregation)[ncol(aggregation)] = value.label
    
    aggregation
    #tail(names(aggregation), length(aggregator)) = names(aggregator)
  }
  
  aggregation.combos <- combos.list(id.fixed, names(id.choices))
  
  #merge results together
  result <- Reduce(merge, lapply(1:length(obs), 
                                 function (i) {
                                   rbind.fill(lapply(aggregation.combos, 
                                                     aggregate.combo, 
                                                     names(obs)[i], 
                                                     obs[i],
                                                     aggregator[[i]],
                                                     names(aggregator)[i]))
                                 }))
  
  
  #if there's one return value things don't get labelled properly?
  
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
  
  result
}




aggregate.by.subgroup <- function (df, 
                                   subgroup, 
                                   relabel="SUBGROUP",
                                   recode=NA,
                                   obs=c(ACCOUNTABILITY_PERF_LEVEL="character",WISER_ID="character"),
                                   FUN=c(
                                     function (x) 
                                       c(PERCENT_PROFICIENT = if (length(x) == 0) 
                                         NA else 
                                           round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),                                      
                                         N_TESTS=length(x),
                                         N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                     N_STUDENTS=function (x) {
                                       length(unique(x))
                                     }),
                                   ids.fixed= c("SCHOOL_YEAR"),
                                   id.choices = c(SCHOOL_ID='All'),
                                   orderings = c()
) {
  
  
  
  df.aggregates <- produce.aggregates.norm(df,
                                           id.fixed=c(ids.fixed, subgroup),
                                           id.choices=c(id.choices, c(GRADE_ENROLLED='All', SUBJECT_CODE="All")),
                                           orderings=c(list(SUBJECT_CODE = c("All", "RE","MA","SC"),                                                 
                                                            GRADE_ENROLLED = c("All", "03", "04", "05", "06", "07", "08")), orderings),                                           
                                           obs=obs,
                                           aggregator=FUN,
                                           add.missing=TRUE)
  
  
  
  
  if (!is.na(recode)) {
    
    df.aggregates[[subgroup]] <- unlist(lapply(df.aggregates[[subgroup]], 
                                               function (x) do.call(switch, as.list(c(x,recode, NA)))))
  }
  
  real.defaults <- unlist(lapply(FUN, function (f) f(c())))
  
  df.aggregates[,names(real.defaults)] <- data.frame(t(apply(df.aggregates[,names(real.defaults)], c(1),                                                                                      
                                                             FUN=function (row) {
                                                               if (all(is.na(row)))
                                                                 real.defaults
                                                               else
                                                                 row                                                          
                                                             })))
  
  if (!is.na(relabel)) 
    names(df.aggregates)[names(df.aggregates) %in% subgroup] <- relabel
  
  
  df.aggregates
}


aggregate.subgroup.combos <- function (df, 
                                       subgroups = list(
                                         STUDENT_LUNCH=c(T='Yes', F='No'),
                                         IDEA_CURRENT_YEAR=c(T='Yes', F='No'),                                                        
                                         CONSOLIDATED_SUBGROUP=c(T='Yes', F='No'),
                                         SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                       ),
                                       obs=c(ACCOUNTABILITY_PERF_LEVEL="character",WISER_ID="character"),
                                       FUN=c(
                                         function (x) 
                                           c(PERCENT_PROFICIENT = if (length(x) == 0) 
                                             NA else 
                                               round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),                                      
                                             N_TESTS=length(x),
                                             N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                         N_STUDENTS=function (x) {
                                           length(unique(x))
                                         }),
                                       ids.fixed= c("SCHOOL_YEAR"),
                                       id.choices = c(SCHOOL_ID='ALL')
)

{
  subgroup.choices <- rep('ALL', length(subgroups))
  names(subgroup.choices) <- names(subgroups)
  id.choices <- c(id.choices, c(GRADE_ENROLLED='ALL', SUBJECT_CODE="ALL"), subgroup.choices)
  
  
  df.aggregates <- produce.aggregates.norm(df,
                                           ids.fixed,
                                           id.choices,
                                           orderings = c(list(GRADE_ENROLLED = c("ALL", "03", "04","05","06","07","08"), 
                                                              SUBJECT_CODE = c("ALL", "RE","MA","SC")), lapply(subgroups, function (val.map) c("ALL", names(val.map)))),
                                           obs=obs,
                                           aggregator=FUN,
                                           add.missing=TRUE)
  
  
  
  real.defaults <- unlist(lapply(FUN, function (f) f(c())))
  
  df.aggregates[,names(real.defaults)] <- data.frame(t(apply(df.aggregates[,names(real.defaults)], c(1),                                                                                      
                                                             FUN=function (row) {
                                                               if (all(is.na(row)))
                                                                 real.defaults
                                                               else
                                                                 row                                                          
                                                             })))
  
  #do recoding
  lapply(seq(1, length(subgroups)),
         function (i) {
           df.aggregates[[names(subgroups)[i]]] <<- unlist(lapply(df.aggregates[[names(subgroups)[i]]], 
                                                                  function (x) do.call(switch, as.list(c(as.character(x),c(subgroups[[i]], ALL="ALL"), NA)))))
         })
  #   if (!is.na(recode)) {
  #     
  #     df.aggregates[[subgroup]] <- unlist(lapply(df.aggregates[[subgroup]], 
  #                                                function (x) do.call(switch, as.list(c(x,recode, NA)))))
  #   }
  
  df.aggregates
}



aggregate.all.subgroups <- function (df, 
                                     subgroups = list(ETHNICITY=NA,
                                                      STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                      IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),
                                                      GENDER=NA,
                                                      ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                      CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                      TESTING_STATUS_CODE=c(T='All')
                                     ),                                     
                                     obs=c(ACCOUNTABILITY_PERF_LEVEL="character",WISER_ID="character"),
                                     FUN=c(
                                       function (x) 
                                         c(PERCENT_PROFICIENT = if (length(x) == 0) 
                                           NA else 
                                             round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),                                      
                                           N_TESTS=length(x),
                                           N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                       N_STUDENTS=function (x) {
                                         length(unique(x))
                                       }),
                                     other.choices = c(),
                                     other.orderings = c()
                                     
) {
  
  scopes <- c(SCHOOL = "SCHOOL_ID", DISTRICT = "DISTRICT_ID")
  
  do.call(rbind, lapply(seq(1:length(scopes)),
                        function (i) {
                          scope <- names(scopes)[i]
                          id.label <- scopes[[i]]
                          id.choices <- c('All')
                          names(id.choices) <- id.label
                          by.scope <- do.call(rbind, lapply(seq(1:length(subgroups)),
                                                            function (i) {
                                                              aggregate.by.subgroup(df, 
                                                                                    subgroup=names(subgroups)[i],
                                                                                    recode=subgroups[[i]],
                                                                                    obs=obs,
                                                                                    FUN=FUN,
                                                                                    id.choices = c(id.choices, other.choices),
                                                                                    orderings=other.orderings
                                                              )
                                                            }))
                          
                          #ugh....
                          id.frame <- if (scope == 'SCHOOL') {
                            data.frame(DISTRICT_ID=ifelse(by.scope$SCHOOL_ID == 'All', 
                                                          'All', 
                                                          paste(substr(by.scope$SCHOOL_ID,1,4),'000',sep='')),
                                       by.scope['SCHOOL_ID'])
                          } else {
                            data.frame(by.scope['DISTRICT_ID'],
                                       SCHOOL_ID = rep('All', nrow(by.scope)))
                            
                          }
                          
                          
                          by.scope <- cbind(SCOPE=ifelse(by.scope[[id.label]] == 'All', 
                                                         paste('STATE',scope, sep="_"), scope), 
                                            by.scope[,c("SCHOOL_YEAR", "GRADE_ENROLLED", "SUBGROUP", "SUBJECT_CODE")],
                                            id.frame,
                                            if (length(other.choices) == 1) by.scope[names(other.choices)] else by.scope[,names(other.choices)],
                                            by.scope[,names(unlist(lapply(FUN, function (f) f(c()))))])  #get the measurement columns
                          
                        }))
}



aggregate.subgroup.combos.product <- function (df, 
                                               subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                               ),
                                               subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                TESTING_STATUS_CODE=c(T='All')
                                               ),
                                               obs=c(ACCOUNTABILITY_PERF_LEVEL="character",WISER_ID="character"),
                                               FUN=c(
                                                 function (x) 
                                                   c(PERCENT_PROFICIENT = if (length(x) == 0) 
                                                     NA else 
                                                       round((sum(ifelse(x %in% c('3','4'), 1, 0))/length(x)) * 100, 1),                                      
                                                     N_TESTS=length(x),
                                                     N_PROFICIENT=sum(ifelse(x %in% c('3','4'), 1, 0))),
                                                 N_STUDENTS=function (x) {
                                                   length(unique(x))
                                                 }),
                                               ids.fixed= c("SCHOOL_YEAR"),
                                               id.choices = c(SCHOOL_ID='All')
)

{
  
  
  df.aggregates <- aggregate.all.subgroups(df,
                                           subgroups.nonintersecting,
                                           obs,
                                           FUN,
                                           other.choices=setNames(rep('All', length(subgroups.intersecting)), names(subgroups.intersecting)),
                                           other.orderings=lapply(subgroups.intersecting, function (val.map) c("All", names(val.map))))
  
  
  
  #do recoding
  lapply(seq(1, length(subgroups.intersecting)),
         function (i) {
           df.aggregates[[names(subgroups.intersecting)[i]]] <<- unlist(lapply(df.aggregates[[names(subgroups.intersecting)[i]]], 
                                                                               function (x) do.call(switch, as.list(c(as.character(x),c(subgroups.intersecting[[i]], All="All"), NA)))))
         })
  #   if (!is.na(recode)) {
  #     
  #     df.aggregates[[subgroup]] <- unlist(lapply(df.aggregates[[subgroup]], 
  #                                                function (x) do.call(switch, as.list(c(x,recode, NA)))))
  #   }
  
  df.aggregates
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


# partition.names <- function (df, cols) {
#   
#   split(names(df), sapply(seq(1, length(names(df))),
#                           function (i, v) {
#                             if (v[i] == 1)
#                               1
#                             else
#                               ifelse(sum(v[1:i]) > 1, 2, 0)
#                             
#                           }, as.numeric(names(result) %in% cols)))
#   
# }
# 
# factor.data.frame <- function (df, cols, id.name='Id', table.names=NULL) {
#     
#   #get the columns we are factoring out
#   df.a <- unique(df[, cols])
#   #order them the way we want
#   df.a <- df.a[do.call(order, lapply(cols, function (x) df.a[[x]])),]
#   #assign a sequence
#   df.a <- data.frame(seq(1, nrow(df.a)), df.a)
#   #and a name
#   names(df.a)[1] <- id.name
#   
#   #partition df's names              
#   name.parts <- partition.names(df, cols)
#   
#   #replace cols with the correct sequence value
#   df.b <- cbind(df[, name.parts[[1]]], 
#                 merge(df.a, df[,name.parts[[2]]])[id.name],
#                 df[, name.parts[[3]]])
#   #return the result tables
#   result <- list(df.a, df.b)
#   #name the result tables
#   if (!is.null(table.names))
#     names(result) <- table.names
#   result
# }



factor.data.frame <- function (df, dim.table.specs, scd=c(), school.year.label, year.id.offset=0) {
  
  
  factor.table <- function (df, cols, id.label, school.year.id=NULL, school.year.id.label=NULL, id.offset=0) {
    
    
    df.a <- unique(df[cols])
    
    #order them according to the order specified in cols    
    df.a <- data.table(df.a[do.call(order, lapply(cols, function (x) df.a[[x]])),])
    
    setnames(df.a, seq(1, ncol(df.a)), cols)
    
    
    #assign a sequence if there are no id columns already defined
    if (length(cols[which(names(cols) == "ID")]) == 0) {
      df.a <- data.table(seq(1, nrow(df.a)) + id.offset, df.a)
      #and a name
      setnames(df.a, 1, id.label)
      setkeyv(df.a, id.label)
    } else {
      
      setkeyv(df.a, cols[which(names(cols) == "ID")])
    }
    
    #add year column if it is present
    if (!is.null(school.year.id)) {      
      #old.key <- key(df.a)
      df.a <- data.table(rep(school.year.id, nrow(df.a)), df.a)
      setnames(df.a, 1, school.year.id.label)      
      #setkeyv(df.a, c(school.year.id.label, old.key))
      df.a      
    }else 
      df.a
  }
  
  #compute the school year dimension table
  school.year.id.label <- paste(school.year.label, "Id", sep="")  
  school.year.table = factor.table(df, school.year.label, school.year.id.label, id.offset=year.id.offset)
  
  
  factor.table.year <-   function (j, cols, id.col.label) {
    factor.table(df[eval(bquote(df[.(school.year.label)] == .(school.year.table[j][[school.year.label]]))),],
                 cols, 
                 id.col.label,
                 school.year.table[j][[school.year.id.label]],
                 school.year.id.label)
    
  }
  
  factor.table.years <- function (i) {
    cols <- dim.table.specs[[i]]
    id.col.label <- paste(names(dim.table.specs)[i], "Id", sep="")
    
    if (i %in% scd) {
      result <- do.call(rbind, lapply(seq(1,nrow(school.year.table)),
                                      factor.table.year, cols, id.col.label
      ))
      setkeyv(result, c(school.year.id.label, cols[which(names(cols) == "ID")]))
    } else {
      factor.table(df, cols, id.col.label)
    }
  }
  
  
  #compute the other dimensions indexed by school year to track slowly change over time
  dim.tables <- lapply(seq(1, length(dim.table.specs)), factor.table.years)
  names(dim.tables) <- names(dim.table.specs)
  
  #now school year is just another dimension
  dim.tables <- c(SchoolYear=list(school.year.table), dim.tables)
  dim.table.specs <- c(SchoolYear=school.year.label, dim.table.specs)
  
  #factor out the dimensions
  obs.vals <- names(df)[which(!(names(df) %in% unlist(dim.table.specs)))]
  
  make.filter.expr <- function (col, row, dim) {
    eval(bquote(dim[[.(col)]] == .(row[[col]])))
  }
  
  make.filter <- function (row, spec, dim) {
    Reduce(`&`, lapply(spec, make.filter.expr, row, dim))
  }
  
  lookup.id <- function (row, dim, spec, id.col.label) {
    dim[make.filter(row, spec, dim)][[id.col.label]]
  }
  
  #function to construct dimension id columns
  get.id.col <- function (i) {
    spec = dim.table.specs[[i]]
    dim = dim.tables[[i]]
    
    ids <- spec[which(names(spec) == "ID")]
    if (length(ids) > 0) {
      df[ids]      
    } else {
      id.col.label <- paste(names(dim.table.specs)[i], "Id", sep="")      
      result <- data.frame(apply(df[spec], c(1), lookup.id, dim, spec, id.col.label))
      names(result) <- id.col.label
      result      
    }
  }
  
  #The fact table consists of the dimension ids plus the observed variables
  obs.table <- do.call(cbind, c(lapply(seq(1, length(dim.table.specs)), 
                                       get.id.col),
                                df[, obs.vals]))
  
  result <- c(dim.tables, Stats=list(obs.table))
  result
}



estimate.varchar.lengths <- function (df) {
  
  estimate.varchar.lengths.aux <- function (x) {
    if (class(df[[x]]) == "character") {
      max.length <-max(nchar(df[[x]][which(!is.na(df[[x]]))]))
      if (max.length == 0) #still create a column with width 1 even if every value is empty
        max.length = 1
      log.max.length <- log(max.length, 2)
      bytes <- floor(log.max.length)
      if (bytes != log.max.length) {
        bytes = bytes + 1        
      } 
      paste("varchar(", 2^bytes, ")", sep="")        
    }
    else {
      NULL
    }
  }
  
  varcharTypes = unlist(lapply(names(df), estimate.varchar.lengths.aux))
  names(varcharTypes) = unlist(lapply(names(df), function (n) if (class(df[[n]])=="character") n else NULL))
  varcharTypes
} 



estimate.mssql.types <- function (df) {
  is.int <- function (v) {    
    all(floor(v) == v)
  }
  
  num.int.digits <- function (v) {
    max(floor(log10(abs(c(1, v[which(v != 0)])))) + 1)
  }
  estimate.mssql.types.aux <- function (x) {
    if ("character" %in% class(df[[x]]) | "factor" %in% class(df[[x]])) {
      col <- as.character(df[[x]])
      max.length <-max(nchar(col[which(!is.na(col))]))
      if (max.length == 0) #still create a column with width 1 even if every value is empty
        max.length = 1
      log.max.length <- log(max.length, 2)
      bytes <- floor(log.max.length)
      if (bytes != log.max.length) {
        bytes = bytes + 1        
      } 
      paste("varchar(", 2^bytes, ")", sep="")        
    }
    else {
      
      if (is.numeric(df[[x]])) {
        col <- df[[x]]
        all.ints <- is.int(col[which(!is.na(col))])
        is.bit <- all.ints & any(col == 0) & any(col == 1) & all(between(col[which(!is.na(col))],0,1)) #kind of a guess: both zeros and ones (and maybe nulls) and that's all
        integral.digits <- num.int.digits(col)
        fractional.digits <- num.int.digits(as.numeric(unlist(lapply(col, function (x) strsplit(as.character(x), ".", fixed=TRUE)[[1]][2]))))
        
        if (is.bit)
          "bit"
        else if (all.ints) 
          "int"
        else
          paste("decimal", "(", integral.digits+fractional.digits, ",", fractional.digits, ")", sep="")
        
        
      } else {
        NULL
      }
    }
  }
  
  types <- unlist(lapply(names(df), estimate.mssql.types.aux))
  names(types) <- names(df)
  types
} 


write.tables <- function (db.dsn, df.list, db.prefix, uid=NULL, pwd=NULL, append=FALSE, time.var="SchoolYearId", other.fact.key=NULL) {
  
  if (is.null(uid))
    conn <- odbcConnect(dsn=db.dsn)
  else
    conn <- odbcConnect(db.dsn, uid, pwd)
  
  make.key.list <- function (dim.tables, other.cols = NULL) {
    do.call(paste, c(as.list(Reduce(union, c(other.cols, sapply(dim.tables, function (t) key(t))))), sep=", "))
    
  }
  
  alter.nullability <- function (dim, table.name, other.cols = NULL) {
    alter.key <- function (pk.col) {
      cols <- sqlColumns(conn, table.name)
      pk.data.type <- cols[cols$COLUMN_NAME==pk.col, "TYPE_NAME"]
      pk.col.size <- if (pk.data.type == "varchar") paste("(", cols[cols$COLUMN_NAME==pk.col, "COLUMN_SIZE"], ")", sep="") else ""
      sqlQuery(channel=conn,             
               query=paste("ALTER TABLE", 
                           table.name, 
                           "ALTER COLUMN", 
                           pk.col, 
                           pk.data.type,
                           pk.col.size,
                           "NOT NULL"))
    }
    
    lapply(key(dim), alter.key)
    sapply(other.cols, alter.key)
    
  }
  
  add.dim.constraints <- function (dim, dim.table.name) {
    alter.nullability(dim, dim.table.name)
    sqlQuery(channel=conn,
             query=paste("ALTER TABLE", 
                         dim.table.name, 
                         "ADD CONSTRAINT", 
                         do.call(paste, c(as.list(strsplit(dim.table.name, ".", TRUE)[[1]]), "PK", sep="_")), 
                         "PRIMARY KEY (", 
                         make.key.list(list(dim)), 
                         ")"))      
  }
  
  add.fact.constraints <- function (n, facts, facts.table.name, other.cols) 
  {
    
    foreign.keys <- function (i) {
      
      dim.table <- df.list[[i]]
      dim.table.name <- names(df.list)[i]
      dim.table.id <- make.key.list(list(dim.table))
      alter.nullability(dim.table, facts.table.name, other.cols)
      sqlQuery(channel=conn,
               query=paste("ALTER TABLE", 
                           facts.table.name, 
                           "ADD CONSTRAINT", 
                           do.call(paste, c(as.list(strsplit(facts.table.name, ".", TRUE)[[1]]), 
                                            as.list(strsplit(dim.table.name, ".", TRUE)[[1]]),
                                            "FK", 
                                            sep="_")), 
                           "FOREIGN KEY (", 
                           dim.table.id, 
                           ") REFERENCES",
                           paste(db.prefix,                                  
                                 dim.table.name, 
                                 sep="."),
                           "(",
                           dim.table.id, 
                           ")"))
      
      
      
    }
    
    lapply(seq(1, n-1), foreign.keys)  
    
    
    sqlQuery(channel=conn,
             query=paste("ALTER TABLE", 
                         facts.table.name, 
                         "ADD CONSTRAINT", 
                         do.call(paste, c(as.list(strsplit(facts.table.name, ".", TRUE)[[1]]), "PK", sep="_")), 
                         "PRIMARY KEY (", make.key.list(df.list[1:(n-1)]) , ")"))
    
  }
  
  write.table <- function (i) {
    
    data <- df.list[[i]]
    table <- paste(db.prefix, 
                   names(df.list)[i], 
                   sep=".")
    
    data <- do.call(data.frame,
                    sapply(names(data),
                           function (n) {
                             
                             if (class(data[[n]]) == 'logical')
                               as.numeric(data[[n]])
                             else
                               data[[n]]
                           }, simplify = FALSE, USE.NAMES = TRUE))
    
    #write all tables when not in append mode
    #write only data for tables that depend on time.var when in append mode
    if(!append | time.var %in% names(data)) {
      sqlSave(channel = conn, 
              dat = data, 
              tablename = table,
              rownames = FALSE,
              safer=FALSE,
              varTypes = estimate.mssql.types(data),
              append=append)
      
      #only write constraints when not in append mode
      if (!append) {
        #fact table is presumed to be the last one
        if (i < length(df.list))
          add.dim.constraints(data, table)
        else
          add.fact.constraints(i, data, table, other.fact.key)  
      }
    }
  }
  
  #initially drop all tables when not in append mode
  if (!append) {
    lapply(rev(names(df.list)), function (table.name) {
      sqlQuery(channel=conn,
               query=paste("DROP TABLE", 
                           paste(db.prefix, 
                                 table.name, 
                                 sep=".")))
      
    })
  }
  
  lapply(seq(1: length(df.list)), write.table)
  
  odbcClose(conn)
}




suppress.groups <- function (suppress.dt, Ncol, col.groups, min.N=10, All.label="All"){
  
  
  suppressed.label <- paste("Suppressed", Ncol, sep=".")
  marked.label <- paste("Marked", Ncol, sep=".")
  
  #suppress all the rows that don't meet min.N on Ncol
  suppress.dt[, c(suppressed.label) := get(Ncol) > 0 & get(Ncol) < min.N]  
  
    
  suppress.group <- function (s, a) {
    
    if (any(s)) 
      sum(a[which(s)]) > 0 & sum(a[which(s)]) < min.N
    else
      FALSE
  }
  
  
  suppress.helper <- function (g, col.name) {
    suppress.dt[get(col.name) %in% g,
                c(suppressed.label) := get(suppressed.label) | (suppress.group(get(suppressed.label),get(Ncol)) & get(Ncol) > 0), 
                by=eval(setdiff(key(suppress.dt), col.name)) ]  #not sure why we have to do eval(setdiff(...
  }
  
  suppress.partition <- function (col.name) {           
    lapply(col.groups[[col.name]], #one iteration per partition
           suppress.helper, col.name
    )
  }
  #apply secondary supressession to partitions with suppressed parts
  lapply(names(col.groups), #one iteration per partitioned column
         suppress.partition 
  )
  
  
  #table(suppress.dt[,get(suppressed.label)])
  
  
  #Now we have to go back through and suppress the rest of those groups that had some of their members suppressed.
  #First we'll mark them for suppression in order to draw a comparison with what is already suppressed, and then suppress them. 
  mark.group <- function (comps,supp, Ns) {
    
    #order the group with 'All' first
    supp.ordered <- supp[c(which(comps == All.label), which(comps != All.label))]
    Ns.ordered <- Ns[c(which(comps == All.label), which(comps != All.label))]
    Ns.suppressed <- sum(Ns.ordered[which(supp.ordered[seq(supp.ordered) > 1] == 1) + 1])
    
    if((!supp.ordered[1] & sum(supp.ordered[seq(supp.ordered) > 1]) == 1) |  #'All' is not suppressed but excatly one subcategory is suppressed, no matter its size
      (!supp.ordered[1] &  0 < Ns.suppressed & Ns.suppressed < min.N) |  #'All' is not suppressed and the sum of the populations of the suppressed partitions is less then the minimum
      (supp.ordered[1] & sum(supp.ordered[seq(supp.ordered) > 1]) == 0))  #'All' is suppressed but no subcategories are suppressed
      supp[which(comps != All.label)] <- TRUE
    
    supp
    
  }
  
  suppress.dt[,c(marked.label) :=FALSE]

  
  mark.helper <- function (g, col.name) {
    suppress.dt[get(col.name) %in% c(All.label,g),
                c(marked.label) := get(marked.label) | (mark.group(get(col.name), get(suppressed.label), get(Ncol)) & get(Ncol) > 0), 
                by=eval(setdiff(key(suppress.dt), col.name)) ]  #not sure why we have to do eval(setdiff(...
  }
  
  mark.partition <- function (col.name) {           
    lapply(col.groups[[col.name]], #one iteration per partition
           mark.helper, col.name
    )
  }

  lapply(names(col.groups), #one iteration per partitioned column
         mark.partition
  )
  #table(suppress.dt[,c(suppressed.label, marked.label), with=FALSE], useNA="ifany")
  
  #Supress the marked ones
  suppress.dt[,c(suppressed.label):= get(suppressed.label) | get(marked.label),]
  suppress.dt
    
}


compute.growth.subgroups <- function (assess.df, growth.df, schools, pairings, min.N.growth.report=10, round.to = 1, growthCuts = c(35, 65), year.id.offset=0) {
  
  paws.agg.sg <- aggregate.subgroup.combos.product(assess.df,
                                                   subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                                   ),
                                                   subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                    IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                    CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                    ADVANCED_SUBGROUP=c(T='ASG', F='NASG'),
                                                                                    ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                                                    ETHNICITY=NA,
                                                                                    GENDER=NA,
                                                                                    TESTING_STATUS_CODE=c(T='All')
                                                   ),
                                                   obs=c(STANDARD_PAWS_PERF_LEVEL="character",WISER_ID="character"))
  
  
  
  
  
  growth.agg.sg <- aggregate.subgroup.combos.product( growth.df[!is.na(growth.df$SGP),],
                                                      subgroups.intersecting = list(SCHOOL_FULL_ACADEMIC_YEAR=c(T='Yes', F="No")
                                                      ),
                                                      subgroups.nonintersecting = list(STUDENT_LUNCH=c(T='FRL', F='NFRL'),
                                                                                       IDEA_CURRENT_YEAR=c(T='IDEA', F='NIDEA'),                                                        
                                                                                       CONSOLIDATED_SUBGROUP=c(T='CSG', F='NCSG'),
                                                                                       ADVANCED_SUBGROUP=c(T='ASG', F='NASG'),
                                                                                       ELL_CURRENT_YEAR=c(T='ELL', F='NELL'),
                                                                                       ETHNICITY=NA,
                                                                                       GENDER=NA,
                                                                                       TESTING_STATUS_CODE=c(T='All')
                                                      ),
                                                      obs=c(SGP="numeric",AGP="numeric",WISER_ID="character"),
                                                      FUN=c(
                                                        function (x) 
                                                          c(NLowGrowth = sum(ifelse(x <= growthCuts[1], 1, 0)),
                                                            NTypicalGrowth = sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0)),
                                                            NHighGrowth = sum(ifelse(growthCuts[2] < x, 1, 0)),
                                                            NGrowth=length(x),
                                                            PLowGrowth = if (length(x) == 0) NA else round((sum(ifelse(x <= growthCuts[1], 1, 0))/length(x))*100,round.to),
                                                            PTypicalGrowth = if (length(x) == 0) NA else round((sum(ifelse(growthCuts[1] < x & x <= growthCuts[2], 1, 0))/length(x))*100,round.to),
                                                            PHighGrowth = if (length(x) == 0) NA else round((sum(ifelse(growthCuts[2] < x, 1, 0))/length(x))*100,round.to),
                                                            MGP = if (length(x) == 0) NA else median(x)),
                                                        MAGP=function (x) {
                                                          if (length(x) == 0) NA else median(x)
                                                        },
                                                        N_STUDENTS=function (x) {
                                                          length(unique(x))
                                                        }))
  
  
  
  
  combo.agg <- merge (paws.agg.sg, growth.agg.sg, by=c("SCOPE","SCHOOL_YEAR", "GRADE_ENROLLED", "SUBGROUP", "SUBJECT_CODE", "DISTRICT_ID", "SCHOOL_ID", "SCHOOL_FULL_ACADEMIC_YEAR"), all.x=TRUE)
  
  combo.agg[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", 
               
               "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP", "N_STUDENTS.y")] <- data.frame(t(apply(combo.agg[,c("NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", 
                                                                                                                                 "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP", "N_STUDENTS.y")], c(1),                                                                                      
                                                                                                                    FUN=function (row) {
                                                                                                                      if (all(is.na(row)))
                                                                                                                        c(0,0,0,0,NA,NA,NA,NA, NA,0)
                                                                                                                      
                                                                                                                      else
                                                                                                                        row                                                          
                                                                                                                    })))
  #rename the two n_students columns
  names(combo.agg)[names(combo.agg) %in% c("N_STUDENTS.x", "N_STUDENTS.y")] <- c("N_STUDENTS_ACHIEVEMENT", "N_STUDENTS_GROWTH")
  
  
  
  
  combo.agg <- with(combo.agg, combo.agg[!(SCOPE %in% c("STATE_DISTRICT")),])
  combo.agg$SCOPE <- ifelse(combo.agg$SCOPE == "STATE_SCHOOL", "STATE", combo.agg$SCOPE)
  
  
  #need to get in district name, school name, subject description
  combo.agg$SUBJECT_DESCRIPTION <- unlist(lapply(combo.agg$SUBJECT_CODE, function (s) { switch(as.character(s), RE="Reading", MA="Mathematics", "Reading & Math") }))
  
  
  
  
  combo.agg$SUBGROUP_DESCRIPTION <- unlist(lapply(combo.agg$SUBGROUP, function (x) switch(x,
                                                                                          
                                                                                          All='All Students', 
                                                                                          ASG="Advanced Sugroup",
                                                                                          CSG="Consolidated Subgroup", 
                                                                                          FRL="Free And Reduced Lunch", 
                                                                                          IDEA="Students with Disabilities",
                                                                                          NCSG="Not Consolidated Subgroup",
                                                                                          NFRL="Not Free and Reduced Lunch",
                                                                                          NIDEA="Students without Disabilities",
                                                                                          ELL="English Language Learner",
                                                                                          NELL="Not English Language Learner",
                                                                                          M="Male",
                                                                                          F="Female",
                                                                                          A="Asian",
                                                                                          B="Black (not Hispanic)",
                                                                                          H="Hispanic",
                                                                                          I="American Indian/Alaska Native",
                                                                                          P="Native Hawaiian/Pacific Islander",
                                                                                          W="White (not Hispanic)",
                                                                                          Z="Two or More Races",
                                                                                          NA)))
  
  
  
  combo.agg <- merge(combo.agg, schools[,c("SCHOOL_YEAR", "SCHOOL_ID", "NAME")], all.x=TRUE)
  combo.agg <- merge(combo.agg, unique(schools[,c("SCHOOL_YEAR", "DISTRICT_ID", "DISTRICT_NAME")]), all.x=TRUE)
  
  combo.agg <- with(combo.agg, combo.agg[(SCOPE == 'SCHOOL' & !is.na(NAME)) | (SCOPE == 'DISTRICT' & !is.na(DISTRICT_NAME)) | SCOPE=='STATE',])  #
  
  
  #assign 'All Schools' and 'All Districts' descriptors
  combo.agg[combo.agg$SCHOOL_ID=='All',]$NAME <- 'All Schools'
  combo.agg[combo.agg$DISTRICT_ID=='All',]$DISTRICT_NAME <- 'All Districts'
  
  
  
  #GRADE_ENROLLED is an ordered factor (should look into this)
  combo.agg$GRADE_DESCRIPTION <- unlist(lapply(combo.agg$GRADE_ENROLLED, function (x) switch(as.character(x),
                                                                                             All="All Grades",
                                                                                             `03`="Third Grade", 
                                                                                             `04`="Fourth Grade", 
                                                                                             `05`="Fifth Grade", 
                                                                                             `06`="Sixth Grade", 
                                                                                             `07`="Seventh Grade",
                                                                                             `08`="Eigth Grade",
                                                                                             NA)))
  
  
  combo.agg$SCHOOL_FAY_DESCRIPTION <- unlist(lapply(combo.agg$SCHOOL_FULL_ACADEMIC_YEAR, function (x) switch(x,
                                                                                                             All="All Students",
                                                                                                             Yes="Full Academic Year Only", 
                                                                                                             No="Not Full Academic Year Only",
                                                                                                             NA)))
  
  result <- combo.agg[,c("SCOPE", "DISTRICT_ID", "DISTRICT_NAME", "SCHOOL_ID", "NAME",
                         "SCHOOL_YEAR", "GRADE_ENROLLED", "GRADE_DESCRIPTION", "SUBGROUP", "SUBGROUP_DESCRIPTION", "SCHOOL_FULL_ACADEMIC_YEAR", "SCHOOL_FAY_DESCRIPTION",
                         "SUBJECT_CODE", "SUBJECT_DESCRIPTION", 
                         "NLowGrowth", "NTypicalGrowth", "NHighGrowth", "NGrowth", "N_STUDENTS_GROWTH",
                         
                         "PLowGrowth", "PTypicalGrowth", "PHighGrowth", "MGP", "MAGP",
                         
                         "PERCENT_PROFICIENT", "N_PROFICIENT", "N_STUDENTS_ACHIEVEMENT", "N_TESTS")]
  
  
  names(result) <- c("DataScope"
                     ,"DistrictId"
                     ,"DistrictName"
                     ,"SchoolId"
                     ,"SchoolName"
                     ,"SchoolYear"
                     ,"GradeEnrolled"
                     ,"GradeDescription"
                     ,"Subgroup"
                     ,"SubgroupDescription"
                     ,"StudentMobility"
                     ,"StudentMobilityDescription"
                     ,"SubjectCode"
                     ,"Subject"
                     ,"NLowGrowth"
                     ,"NTypicalGrowth"
                     ,"NHighGrowth"
                     ,"NGrowthScores"
                     ,"NStudentsGrowth"
                     ,"PLowGrowth"
                     ,"PTypicalGrowth"
                     ,"PHighGrowth"
                     ,"MGP"
                     ,"MAGP"
                     ,"PProficient"
                     ,"NProficient"
                     ,"NStudentsAchievement"
                     ,"NTestsAchievement")
  
  
  
  
  #paired schools obtain the same values as their parent schools
  pairing.helper <- function (s) {
    
    school.name <- schools[schools$SCHOOL_YEAR == current.school.year & schools$SCHOOL_ID==s,"NAME"]
    if (length(school.name) == 0)
      return(NULL)
    
    parent.school <- pairings[[s]]
    parent.recs <-result[result$SchoolId == parent.school,]
    parent.recs$SchoolId <- s
    parent.recs$SchoolName <- school.name
    parent.recs
  }
  
  
  result <- rbind(result[!(result$SchoolId %in% names(pairings)),], do.call(rbind, lapply(names(pairings),
                                                                                          pairing.helper)))
  
  dim.spec <- list(Scope="DataScope", 
                   District=c(ID="DistrictId", "DistrictName"), 
                   School=c(ID="SchoolId", "SchoolName"), 
                   Grade=c(ID="GradeEnrolled", "GradeDescription"), 
                   Subgroup=c(ID="Subgroup", "SubgroupDescription"), 
                   Mobility=c(ID="StudentMobility", "StudentMobilityDescription"),
                   Subject=c(ID="SubjectCode", "Subject"))
  
  factored.result <- factor.data.frame(result,                                      
                                       dim.spec,
                                       c(3),
                                       "SchoolYear",
                                       year.id.offset)
  
  stats.key <- c("SchoolYearId", "ScopeId", "DistrictId", "SchoolId", 
                 "Subgroup", "GradeEnrolled", "StudentMobility", "SubjectCode") 
  
  stats.table <- data.table(factored.result$Stats,
                            key=stats.key)
  
  suppress.groups(stats.table, 
                  "NStudentsGrowth", 
                  list(Subgroup=list(c('A','B','H','I','P','W','Z'),
                                     c('ASG', 'NASG'),
                                     c('CSG', 'NCSG'),
                                     c('ELL', 'NELL'),
                                     c('M','F'),
                                     c('IDEA', 'NIDEA'),
                                     c('FRL', 'NFRL')),
                       StudentMobility=list(c('No','Yes')), 
                       GradeEnrolled=list(c("03", "04", "05", "06", "07", "08"))))
  
  suppress.groups(stats.table, 
                  "NStudentsAchievement", 
                  list(Subgroup=list(c('A','B','H','I','P','W','Z'),
                                     c('ASG', 'NASG'),
                                     c('CSG', 'NCSG'),
                                     c('ELL', 'NELL'),
                                     c('M','F'),
                                     c('IDEA', 'NIDEA'),
                                     c('FRL', 'NFRL')),
                       StudentMobility=list(c('No','Yes')), 
                       GradeEnrolled=list(c("03","04", "05", "06", "07", "08"))))  
  
  stats.table[,Suppressed := Suppressed.NStudentsGrowth | Suppressed.NStudentsAchievement]
  #convert back to data.frame to apply capping
  stats.table <- data.frame(stats.table)[,names(stats.table)[grep('^Marked', names(stats.table), invert=TRUE)]]
    
  stats.table$NStudentsGrowthCap <- sapply(stats.table$NStudentsGrowth,
                                           function (a) {
                                             
                                             if (a == 0)
                                               return("0")
                                             
                                             low <- if (a < 5) 1 else (a%/%5)*5
                                             high <- ((a+5)%/%5)*5  - 1
                                             
                                             paste(low,high, sep="-")                                                                                                                              
                                           })
  
  stats.table$NStudentsAchievementCap <- sapply(stats.table$NStudentsAchievement,
                                                function (a) {
                                                  
                                                  if (a == 0)
                                                    return("0")
                                                  
                                                  low <- if (a < 5) 1 else (a%/%5)*5
                                                  high <- ((a+5)%/%5)*5  - 1
                                                  
                                                  paste(low,high, sep="-")                                                                                                                              
                                                })
  
  
  stats.table[c("PProficientCap","PProficientCapOperator")] <- t(apply(stats.table[c("NStudentsAchievementCap",
                                                                                     "PProficient",
                                                                                     "Suppressed")],
                                                                       c(1),
                                                                       function (row) {
                                                                         par <- as.numeric(row[["PProficient"]])
                                                                         aac <- row[["NStudentsAchievementCap"]]
                                                                         suppressd <- as.numeric(row[["Suppressed"]])
                                                                         
                                                                         if (is.na(par))
                                                                           return(c(NA, NA))
                                                                         
                                                                         cap <- switch(aac, `1-4`=100, `5-9`=20, `10-14`=10, `15-19`=6.7, 5)
                                                                         
                                                                         if (par <= cap)
                                                                           c(cap, "<=")
                                                                         else {
                                                                           
                                                                           if (par >= 100-cap)
                                                                             c(100-cap, ">=")
                                                                           else
                                                                             c(par, "==")
                                                                           
                                                                         }                                                                               
                                                                       }))
  
  stats.table$PProficientCap <- as.numeric(stats.table$PProficientCap)
  
  
  
  stats.table$Suppressed <- as.numeric(stats.table$Suppressed)
  
  
  
  
  
  
  factored.result$Stats <- stats.table
  
  list(result=result, factored.result=factored.result)
}
