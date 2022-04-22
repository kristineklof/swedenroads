#=================================================================#
#                 Descriptove statistics functions
#=================================================================#

DescriptiveStats <- function(var){
    dstats <- data.frame(Mean = round(mean(var, na.rm=TRUE), digits = 0),
                SD = round(sd(var, na.rm=TRUE), digits = 0),
               Min = round(min(var, na.rm=TRUE), digits = 0),
               Q1 = round(quantile(var, probs = 0.25, na.rm=TRUE), digits=0),
               Median = round(median(var, na.rm=TRUE), digits = 0),
               Q3 = round(quantile(var, probs = 0.75, na.rm=TRUE), digits=0),
               Max = round(max(var, na.rm=TRUE), digits = 0))

    return(dstats)
}

QualitativeStatsSingleGroup <- function(df, grp.var, uniq.var){
    classlengtht <- df %>%
              group_by(!!grp.var) %>%
              summarise(grouplen = sum(!!uniq.var)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))

    return(classlengtht)
}

QualitativeStatsDoubleGroup <- function(df, grp.var, grp.var2, uniq.var){
    classlengtht <- df %>%
              group_by(!!grp.var, !!grp.var2) %>%
              summarise(grouplen = sum(!!uniq.var)/1000) %>%
              mutate(prop = grouplen/sum(grouplen))

    return(classlengtht)
}

QualitativeStatsTripleGroup <- function(df, grp.var, grp.var2, grp.var3, uniq.var){
    classlengtht <- df %>%
        group_by(!!grp.var, !!grp.var2, !!grp.var2) %>%
        summarise(grouplen = sum(!!uniq.var)/1000) %>%
        mutate(prop = grouplen/sum(grouplen))
    
    return(classlengtht)
}

QualitativeStatsSingleGroupTA <- function(df, grp.var, uniq.var, traf.var){
    classlengtht <- df %>%
              group_by(!!grp.var) %>%
              summarise(grouplen = sum(Length/1000),
                        grouplenta = sum(!!uniq.var * !!traf.var/1000)) %>%
              mutate(prop = grouplen/sum(grouplen)) %>%
              mutate(propta = grouplenta/sum(grouplenta))

    return(classlengtht)
}