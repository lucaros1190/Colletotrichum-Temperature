
# fitlogit.py Python script to fit data with a logistic function
# Created by Luca Rossini on 26 February 2021
# E-mail luca.rossini@unitus.it
# Last update 27 Febryary 2021

import pandas as pd
import plotly.graph_objs as go
from math import *
from scipy import stats
from scipy.optimize import curve_fit
from scipy.stats.distributions import chi2
from scipy import odr
import numpy as np
import matplotlib.pyplot as plt

from scipy.optimize import curve_fit

# Read the data to fit and plot

Data = pd.read_csv("data.txt", sep="\t", header=None)


# Set the header of the dataset

Data.columns = ["x", "y", "err_x", "err_y"]

x = Data['x']
y = Data['y']
err_x = Data['err_x']
err_y = Data['err_y']

# Fit data with a Logistic function
    # Definition of the Logistic function
    
def logitfun(x, N_0, r):
    k = 100
    return (k * N_0)/(N_0 + (k-N_0)* np.exp(-r*x))

    # Fit with curve_fit
    
popt, pcov = curve_fit(logitfun, x, y, bounds=([0., 0.00001], [85., 10]), p0=(0., 0.1))#, method='dogbox')

    # Best fit values
    
N_0 = popt[0]
r = popt[1]

    # Perameters error

perr = np.sqrt(np.diag(pcov))

err_N_0 = perr[0]
err_r = perr[1]

# Ask how many sigma do you want to include in the confidence band

print('\n How many sigma do you want to include in the confidence band? (2 sigma is 95%): \n')

num_sigma = float(input())

# Upper ad lower confidence bands

    # Create the linespace to plot the best fit curve
x_fun = np.linspace(0, 50, 2000)

    # Calculations
    
fitted_fun = logitfun(x, *popt)
fitted_plot = logitfun(x_fun, *popt)

N_0_up = N_0 + num_sigma * err_N_0
N_0_low = N_0 - num_sigma * err_N_0
r_up = r + num_sigma * err_r
r_low = r - num_sigma * err_r


upper_fit = logitfun(x_fun, N_0_up, r_up)
lower_fit = logitfun(x_fun, N_0_low, r_low)

# Calculating R-squared

resid = y - fitted_fun
ss_res = np.sum(resid**2)
ss_tot = np.sum((y - np.mean(y))**2)
r_squared = 1 - (ss_res / ss_tot)


# Number of degrees of freedom (NDF)

ndf = len(x) - 2

# Calculate the chi-squared (with error below the fraction)

chi_sq = 0
for i in range(len(x)):
    chi_sq = pow((y[i] - fitted_fun[i]), 2)/err_y[i]

# Calculate the P-value from chi-square

Pvalue = 1 - chi2.sf(chi_sq, ndf)

# Calculate AIC and BIC

AIC = 2 * 3 - 2 * np.log(ss_res/len(x))
BIC = 3 * np.log(len(x)) - 2 * np.log(ss_res/len(x))


# Print the results

print('\n Logistic fit (k * N_0)/(N_0 + (k-N_0)*exp(-r*x)) results: \n')

    # Define the parameters' name
parname = (' N_0 = ', ' r = ')

for i in range(len(popt)):
    print(parname[i] + str(round(popt[i], 7)) + ' +/- ' + str(round(perr[i],7)))

print(' R-squared = ', round(r_squared, 5))
print(' Chi-squared = ', round(chi_sq, 5))
print(' P-value = ', round(Pvalue, 7))
print(' Number of degrees of freedom (NDF) =', ndf)
print(' Akaike Information Criterion (AIC):', round(AIC, 5))
print(' Bayesian Information Criterion (BIC)', round(BIC, 5))
print('\n')

print(' Covariance matrix: \n')
    # Define the row names to print
rowname = (' ', 'N_0 ', 'r   ')
print(rowname[0] + '     \t' + rowname[1] + '     \t' + rowname[2])

for i in range(len(pcov)):
    print(rowname[i+1] + ' ' + str(pcov[i]))

print(' ')

# Plot the data

plt.figure(1)

plt.scatter(x, y, color="C0", alpha=0.5, label=f"$Experimental data$")
plt.errorbar(x, y, xerr=err_x, yerr=err_y, fmt='o')
plt.plot(x_fun, fitted_plot, color="C1", alpha=0.5, label=f"$Best fit function$")
plt.fill_between(x_fun, upper_fit, lower_fit, color='b', alpha=0.1)
plt.xlabel('Temperature (°C)')
plt.ylabel('Mortality rate (individual/day)')
plt.legend()

plt.show()
