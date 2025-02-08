# Q1

e_com = read.csv(r"(A1\E-Commerce.csv)", header = TRUE, stringsAsFactors = FALSE)

# Specify the year
e_com$Year = as.numeric(substr(e_com$Quarter, 1,4))

# Specify the indicator variables
e_com$Q1 = as.factor(ifelse(substr(e_com$Quarter, 7,8)=="1",1,0))
e_com$Q2 = as.factor(ifelse(substr(e_com$Quarter, 7,8)=="2",1,0))
e_com$Q3 = as.factor(ifelse(substr(e_com$Quarter, 7,8)=="3",1,0))
e_com$Q4 = as.factor(ifelse(substr(e_com$Quarter, 7,8)=="4",1,0))

# Fit the model
model_reg = lm(Value ~ Year + Q1 + Q2 + Q3 + Q4 -1, data = e_com)

print(model_reg)

# plot results
e_com$Time <- 1:nrow(e_com)
e_com$Fitted <- predict(model_reg)

library(ggplot2)

ggplot(ts(residuals(model_reg)))
ggplot(e_com, aes(x = Time)) + geom_line(aes(y = Value, color = "Actual"), size = 1) +   geom_line(aes(y = Fitted, color = "Fitted"), size = 1, linetype = "dashed") +  scale_x_continuous(breaks = e_com$Time, labels = e_com$Quarter) + 
labs(title = "Actual vs. Fitted Values", x = "Time (Quarterly)", y = "Value") +theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red"))  

residuals_df <- data.frame(Time = e_com$Time, Residuals = residuals(model_reg))
ggplot(residuals_df, aes(x = Time, y = Residuals)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line at 0
  ggtitle("Residual Plot of Regression Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()
# Q2

library(fpp2)

aus = read.csv(r"(A1\AUS.csv)", header = TRUE, stringsAsFactors = FALSE)

ses_aus = ses(aus$Value)

summary(ses_aus)

autoplot(ts(aus$Value), series="Observed") + autolayer(fitted(ses_aus), series="ETS(ANN) Fitted")  + ggtitle("ETS(ANN) Model")

autoplot(ts(residuals(ses_aus)), series = "Residuals")+ ggtitle("ETS(ANN) Model Residuals")
aus_ts = ts(aus$Value, frequency=4)
hw_aus = ets(aus_ts,model = "AAA")
autoplot(hw_aus) + ggtitle("ETS Decomposition of ETS(AAA)")
autoplot(aus_ts, series="Observed") + autolayer(fitted(hw_aus), series="Fitted")+ ggtitle("ETS(AAA) Model")

alpha_hat <- hw_aus$par["alpha"]
beta_hat <- hw_aus$par["beta"]
gamma_hat <- hw_aus$par["gamma"]

cat("Fitted Level (α):", alpha_hat, "\n")
cat("Fitted Trend (β):", beta_hat, "\n")
cat("Fitted Seasonality (γ):", gamma_hat, "\n")

forecasts_2y <- forecast(hw_aus, h = 8)

autoplot(aus_ts, series="Observed") + autolayer(fitted(hw_aus), series="Fitted")+ autolayer(forecasts_2y) + ggtitle("ETS(AAA) Model")

#Q3 
install.packages("astsa")
library(astsa) 
library(ggplot2)

n = length(varve)
first_half = varve[1:(n/2)]
second_half = varve[(n/2 + 1):n]

var_first_half = var(first_half)
var_second_half = var(second_half)

cat("Variance of First Half: ", var_first_half, "\n")
cat("Variance of Second Half:", var_second_half, "\n")

log_varve = log(varve)

autoplot(log_varve) + ggtitle("Log Transformed Series")

acf(log_varve)

z_t = diff(log_varve)
autoplot(z_t) + ggtitle("First Order Difference")

hist(z_t) 
acf(z_t)


autocov_values = acf(z_t, type = "covariance", plot = FALSE)
autocor_values =acf(z_t, type = "correlation", plot = FALSE)

gamma_0 = autocov_values$acf[1] 
gamma_1 = autocov_values$acf[2]

rho_1 = autocor_values$acf[2]

print(gamma_0)
print(rho_1)
print(gamma_1/gamma_0)
