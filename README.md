# AC-SINS Analysis R Shiny Application
R Shiny application that I developed at Eli Lilly &amp; Co. 

# Background on AC-SINS
AC-SINS stands for Affinity-Capture Self-Interaction Nanoparticle Spectroscopy. 

# AC-SINS Analysis & LOESS

# Features of the Application

# Workflow / How to Use the Application
The workflow of AC-SINS analysis is as follows. To begin, the user prepares the raw AC-SINS data output from the experiment, as well as a sample name file, which contains sample information in a 384-well format. These two files are then uploaded to the AC-SINS Analysis R Shiny application, and the application initiates the generation of all tables and visualizations that support the analysis process. The user may then view the resulting plasmon wavelength (as well as additional results) per sample quadruplicate, as determined by the LOESS method. Simply stated, LOESS is a smoothing method that removes noise from the data, isolating a peak value from the newly described data. Upon consideration of the results, plots, and data quality, the user can then optimize and download any visualization, and download conditionally formatted reports from the application to use for his or her presentation purposes.

**Step 1:** Uploading Data Files
There is anonymyzed example data available in this repository, in the "data" folder. The file named "raw.xlsx" can be uploaded to the ___ and the file named "names.xlsx" can be upload to the ___ input. 

**Step 2:** Explore the Application!

**Step 3:** Download Plots and Reports
