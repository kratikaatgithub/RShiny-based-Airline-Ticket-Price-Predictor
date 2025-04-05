## Flight Ticket Price Prediction Explorer  

## 🛫 Overview  

This **Shiny application** is an interactive tool for exploring and predicting airline ticket prices using different statistical models. It allows users to analyze historical flight data, compare prediction models, and visualize results—demonstrating **data science, statistical modeling, and Shiny development** skills.  

🚀 **Deployed App**: https://kratikag.shinyapps.io/airlinepricepredictor/
---  

## 🔍 Key Features  

### 📊 **Model Comparison & Customization**  
- **Linear Regression** with selectable features (airline, route, stops, duration, etc.)  
- **ARIMA** with adjustable (p,d,q) parameters for time series forecasting  
- **Holt-Winters** exponential smoothing (customizable alpha, beta, gamma)  

### 📈 **Interactive Visualizations**  
- Actual vs. predicted price plots (Liner Regression)  
- Time series fitting visualizations (ARIMA/Holt-Winters)  
- Dynamic data tables with filtering  

### 🛠 **Automated Data Processing**  
- Handles missing values and outliers  
- Converts data for time-series analysis (daily averages)  

---  

## 💻 Technical Stack  
- **R Shiny** (frontend + backend)  
- **Packages**: `forecast`, `dplyr`, `lubridate`, `ggplot2`, `DT`  
- **Deployment**: Hosted on **shinyapps.io**  

---  

Built with ❤️ by Kratika Garg  
