# PCR Multiplex Analysis R Shiny App

This repository contains the R Shiny app for PCR Multiplex Analysis. The app is designed to analyze PCR data and identify specific genetic targets within samples. Given a set of known targets, the app determines which samples contain one or more of these targets.

## Current Features:
The app allows users to input known target data along with PCR results for each sample.
It identifies the presence of targets by analyzing upper and lower bounds and detecting outliers within groups of samples from the same patient.
Currently, the app is limited to handling samples that can have up to three possible targets.

## Planned Improvements:
Expanded Data Handling – Support for multiple sets of target data and PCR results, enabling users to analyze multiple PCR graphs simultaneously.
Increased Target Capacity – Extending the app’s capability to handle more than three targets per sample.
Enhanced Prediction Accuracy – Improving the target prediction algorithm to work effectively with real-world (less-than-perfect) data, as the current model was developed using simulated data.

This app aims to help the biology department in their research endeavors. Feel free to clone and improve the app!