## Airline Ticket Price Prediction Explorer  

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
- Actual vs. predicted price plots (regression)  
- Time series fitting visualizations (ARIMA/Holt-Winters)  
- Dynamic data tables with filtering  

### 🛠 **Automated Data Processing**  
- Cleans raw flight data (date parsing, duration conversion, stop-count normalization)  
- Handles missing values and outliers  
- Converts data for time-series analysis (daily averages)  

---  

## 💻 Technical Stack  
- **R Shiny** (frontend + backend)  
- **Packages**: `forecast`, `dplyr`, `lubridate`, `ggplot2`, `DT`  
- **Deployment**: Hosted on **shinyapps.io**  

---  

## 🎯 Use Cases  
✔ **Recruiters**: Evaluate predictive modeling and Shiny development skills  
✔ **Data Scientists**: Explore flight price trends and model performance  
✔ **Analysts**: Compare regression vs. time-series approaches  

---  

Built with ❤️ by Kratika Garg  
