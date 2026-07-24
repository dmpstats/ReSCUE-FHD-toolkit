# ReSCUE-FHD Toolkit User Guide

**Last Updated:** July 24, 2026  
**Author:** C. Clarke, B. Caneco

---

## Table of Contents

1. [ReSCUE Project Background](#rescue-project-background)
2. [ReSCUEApp Overview](#rescueapp-overview)
3. [Selecting your Flight Height Distribution](#selecting-your-flight-height-distribution)
4. [Analysing your Flight Height Distribution](#analysing-your-flight-height-distribution)
5. [Exporting & Downloading your Results](#exporting--downloading-your-results)

---

## ReSCUE Project Background

The Reducing Seabird Collisions Using Evidence (ReSCUE) project is a multi-year, multi-partner initiative led by Natural England and funded by The Crown Estate's Offshore Wind Evidence and Change (OWEC) Programme. The project addresses a critical knowledge gap: the lack of accurate, high-quality data on seabird flight heights, which is essential for robust assessments of collision risk with offshore wind turbines.

ReSCUE tackles this challenge through three main objectives:

1. **Validating flight height estimation methods**, particularly combined camera-LiDAR (Light Detection and Ranging) Digital Aerial Surveys (DAS), along with laser rangefinders and telemetry approaches
2. **Collecting new and assembling existing flight height data** that is fit-for-purpose for use in offshore wind impact assessments
3. **Creating publicly available datasets and analytical tools** to inform guidance, recommendations, and evidence-led consenting decisions

A major component of ReSCUE is a 12-month digital aerial survey campaign using advanced LiDAR technology, launched in September 2025 and continuing through August 2026. These surveys cover key seabird sites across four regions of UK waters: the northern North Sea, Irish Sea, southern North Sea, and Celtic Sea. High-resolution flight height measurements are collected through integration of digital aerial imagery with LiDAR data, validated through rigorous quality assurance processes. All resulting data will be made publicly available under Open Government Licence.

By combining rigorous validation, cutting-edge technology, and robust analytical approaches, ReSCUE strengthens the evidence base needed to balance seabird conservation with the UK's low-carbon energy ambitions, delivering practical tools and knowledge that protect marine life while enabling sustainable offshore wind development.

### What is a Flight Height Distribution?

A **flight-height distribution**, or FHD, is a statistical estimate of the range of heights at which a bird flies above sea level. 

In simple terms, it describes how likely a bird is to be flying at different heights. For example, a flight height distribution might show that a particular seabird species is most likely to fly between 10 and 30 meters above sea level, with fewer birds flying at lower or higher altitudes.

Flight-height distributions are important for assessing the risk of seabird collisions with offshore wind turbines. By understanding the typical flight heights of different seabird species, developers and regulators can better predict the likelihood of birds colliding with turbine blades, and implement effective mitigation measures to reduce this risk.

<span class="alert alert-info" role="alert">
  <strong><h5>Note:</h5></strong>
  Add an image of a FHD here later.
  </span>

---

## ReSCUEApp Overview

### Purpose

**ReSCUEAPP** is a user-friendly Shiny application making the ReSCUE Project's research accessible to practitioners. The app serves as an access point for seabird flight height data, distributions, and analytical tools.

The toolkit is intended to streamline the use of seabird flight height data in collision risk assessments, simplifying the process of selecting and comparing flight height distributions, conducting air-gap analyses, and exporting results for use in collision risk modelling.

The outputs of ReSCUEApp are designed to be directly compatible with the Stochastic Collision Risk Model (sCRM) and other collision risk assessment tools, enabling users to incorporate the latest evidence into their assessments.

### Key Features

The app provides comprehensive functionality for working with seabird flight height data, including:

- **Interactive data selection** with geographic mapping of UK sea areas and access to recommended flight height distributions for multiple species
- **Flexible comparison tools** allowing users to select and compare multiple flight height distributions, including recommended defaults, existing published data, and user-uploaded distributions
- **Air-gap analysis** functionality to model collision risk under different mitigation scenarios through adjustable risk height envelopes
- **Comprehensive visualization outputs** including distribution plots with confidence intervals, summary statistics, and air-gap comparisons
- **Data export capabilities** allowing users to download analysis summaries, plots, tables, and flight height distribution data in formats compatible with collision risk modelling tools such as the Stochastic Collision Risk Model (sCRM)

---

## Selecting your Flight Height Distribution

ReSCUEApp provides three options for selecting the most suitable flight height distribution data for your use-case:

<div class="card border-primary mb-3">
  <div class="card-body">
  <ol>
    <li><strong>Default Data Selection</strong> - Select a species and region, and the recommended regulator-approved flight height distribution will be automatically selected for you.</li>
  <li><strong>Manual Data Selection</strong> - Select a species and region, and choose from a list of available flight height distributions, including recommended defaults and existing published data.</li>
  <li><strong>User Upload</strong> - Upload your own ReSCUETools-generated flight height distribution data in CSV format for analysis and comparison with existing distributions.</li>
  </ol>
  </div>
</div>

In the below sections, we'll talk you through each of these options in more detail.

### Default Data Selection

This is the simplest option for users who want to quickly access the recommended flight-height distribution for your species and region of interest. 

Simply select a region (either by clicking on the map or selecting from the drop-down) and a species from the list. The app will automatically select the recommended distribution for you. Then click "Next" to proceed to the analysis section.

### Manual Data Selection

For cases in which the recommended flight-height distribution is not suitable, you can manually select from the available distributions. 

Some distributions also contain additional covariates, such as wind speed, which are not included in the recommended distributions. If you wish to use these covariates, the appropriate distribution can be selected from the map. The analysis page will allow you to explore the impact of any included covariates on the flight height distribution.

Once you have selected your species and region, click "Next" to proceed to the analysis section.

<div class="alert alert-warning" role="alert">
  <strong><h4>A warning about covariates!</h3></strong>
  Most distributions containing covariates are considered unsuitable for implementation in SCRM, which is not designed to handle additional covariates. These distributions are provided for reference and investigation, rather than use in SCRM.
</div>

### User Upload

In some rare cases, you may wish to use your own flight height distribution. This can be uploaded as an object exported from the ReSCUETools R package. 

This may be suitable in cases where there are no suitable distributions available for your species and/or region of interest, or if you have additional data that you wish to include in your analysis.

Simply select the "Upload your own distribution" option, provide the required information, and upload your CSV file. The app will then allow you to explore your uploaded distribution alongside the available distributions.

Don't worry about the security of your data - the app does not store any uploaded data, and it will be deleted when you close the app.

Once you have uploaded your distribution, click "Next" to proceed to the analysis section.

<div class="card border-success mb-3">
  <div class="card-body">
  <strong><h4>Mixing and Matching!</h4></strong>
    You can use a mix of the above data-selection options. For example, you may wish to compare the recommended distribution for your use-case to a distribution that you have uploaded yourself. Just use both methods separately to add the selected distributions to the selection table!
  </div>
</div>

---

## Analysing your Flight Height Distribution

---

## Exporting & Downloading your Results

---
