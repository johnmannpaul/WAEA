library("RODBC")
library("reshape")
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
        integral.digits <- num.int.digits(col)
        fractional.digits <- num.int.digits(as.numeric(unlist(lapply(col, function (x) strsplit(as.character(x), ".", fixed=TRUE)[[1]][2]))))
        
        if (all.ints) 
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


write.tables <- function (db.dsn, df.list, db.prefix, uid=NULL, pwd=NULL) {
  
  if (is.null(uid))
    conn <- odbcConnect(dsn=db.dsn)
  else
    conn <- odbcConnect(db.dsn, uid, pwd)
  
  make.key.list <- function (dim.tables) {
    do.call(paste, c(as.list(Reduce(union, sapply(dim.tables, function (t) key(t)))), sep=", "))
    
  }
  
  alter.nullability <- function (dim, table.name) {
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
  
  add.fact.constraints <- function (n, facts, facts.table.name) 
  {
    
    foreign.keys <- function (i) {
      
      dim.table <- df.list[[i]]
      dim.table.name <- names(df.list)[i]
      dim.table.id <- make.key.list(list(dim.table))
      alter.nullability(dim.table, facts.table.name)
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
    
    sqlSave(channel = conn, 
            dat = data, 
            tablename = table,
            rownames = FALSE,
            safer=FALSE,
            varTypes = estimate.mssql.types(data))
    
    #fact table is presumed to be the last one
    if (i < length(df.list))
      add.dim.constraints(data, table)
    else
      add.fact.constraints(i, data, table)  
  }
  
  lapply(rev(names(df.list)), function (table.name) {
    sqlQuery(channel=conn,
             query=paste("DROP TABLE", 
                         paste(db.prefix, 
                               table.name, 
                               sep=".")))
    
  })
  
  lapply(seq(1: length(df.list)), write.table)

  odbcClose(conn)
}

