#########################################################################################
# Copyright (c) 2016. All rights reserved.  See the file LICENSE for
# license terms.
#########################################################################################
# File: LE2_ScoringModel.R
# Proj: R Workshop
# Desc: R - Beyond the Basics,
#       Lecture Part 2 (Project)
# Auth: Andrea Bublitz and Claudia Wenzel
# Date: 2017/02/06
#########################################################################################

#' RFMfunction

#' Calculate a weighted RMF score, this is a Description
#
#Arguments
#' @param data - a data table
#' @param weight_recency - weight of recency
#' @param weiht_frequency - weight of frequency
#
#
#' @details mmmmmmmmmmm
#' @return Returns a data.table containing ... xy ...
#' score this is the second line. thank you for smoking.
#' @export

#Set working directory ####
#setwd("~/Dropbox/R - Beyond the basics/Lectures/data")
#Make sure to set your working directory correctly!

#Install (if necessary) and load the following packages. ####


# 5. The RFM function ####
RFMfunction <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){

   library(data.table)
  library(lubridate)
  library(Hmisc)

  # Ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  # RFM measures
  max.Date <- max(data$TransDate)
  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)/.N),
    by="Customer"
    ]

  # RFM scores
  temp <- temp[,list(Customer,
    recency = as.numeric(cut2(-recency, g=3)),
    frequency = as.numeric(cut2(frequency, g=3)),
    monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
