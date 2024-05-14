#!/usr/bin/env python3

import sys
import pandas as pd
import statsmodels.formula.api as smf

def adjust_lifestyle_column(data):
    """Adjust the 'lifestyle' column to have ordered categories with Pastoralist as the reference."""
    lifestyle_order = ['Pastoralist', 'Rural', 'Market_Integrated']
    data['lifestyle'] = pd.Categorical(data['lifestyle'], categories=lifestyle_order, ordered=True)
    return data

def calculate_aic(data, formula):
    """Calculate AIC for a given model formula."""
    model = smf.ols(formula, data).fit()
    return model.aic

def main(file_path):
    data = pd.read_csv(file_path)
    data = adjust_lifestyle_column(data)

    response_vars = ["HDL", "LDL", "Chol", "BMI", "Body_Fat", "Pulse_pressure", "Trig", "cvd_risk"]
    model_formulas = {
        'Model with urb_score': "{response_var} ~ urb_score + Age + Gender + Diet + Fasting + Alcohol",
        'Model with h_sol': "{response_var} ~ h_sol + Age + Gender + Diet + Fasting + Alcohol",
        'Model with lifestyle': "{response_var} ~ C(lifestyle) + Age + Gender + Diet + Fasting + Alcohol"
    }

    for response_var in response_vars:
        aic_values = {}
        for model_name, formula_template in model_formulas.items():
            formula = formula_template.format(response_var=response_var)
            aic = calculate_aic(data, formula)
            aic_values[model_name] = aic
        
        best_model = min(aic_values, key=aic_values.get)
        print(f"Best model for {response_var}: {best_model} with AIC = {aic_values[best_model]}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 script.py <file_path>")
        sys.exit(1)
    main(sys.argv[1])

