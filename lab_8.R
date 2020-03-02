# 2 March 2020—Lab 8: Capture Models ####
library("Rcapture")
library("FSA")
# Ornate Lizard in Chiricahua Mountains ####
# import the data
ornate_lizard <- read.csv("treelizardcapturehistories.csv", 
													header = TRUE, colClasses = c("ch" = "character"))
head(ornate_lizard)

# subset data
ornate_lizard_u1 <- ornate_lizard[ornate_lizard$hab == "u" &
																		ornate_lizard$site == "1",]
head(ornate_lizard_u1)

# convert data (we are using columns 2 and 3, which correspond to
# id and capture history).
ornate_lizard_u1_ind <- capHistConvert(ornate_lizard_u1[,2:3],
																			 id = "id", in.type = "RMark",
																			 out.type = "individual")

head(ornate_lizard_u1_ind)

# explore heterogeniety in the data (using only columns 2-14)
desc <- descriptive(ornate_lizard_u1_ind[,2:14])
desc
plot(desc)

# Model fit ####
models <- closedp.t(ornate_lizard_u1_ind[,2:14])
models

darroch_estimates <- closedpCI.t(ornate_lizard_u1_ind[,2:14],
																 m = "Mth", h = "Darroch")
plotCI(darroch_estimates)
# Bayesian parameterization ####


# Assignment 1 ####
# Assignment 2 ####

