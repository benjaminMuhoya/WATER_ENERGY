#!/usr/bin/env python3

import sys
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm

def adjust_lifestyle_column(data):
    """Adjust the 'lifestyle' column to have ordered categories with Pastoralist as the reference."""
    lifestyle_order = ['Pastoralist', 'Rural', 'Market_Integrated']
    data['lifestyle'] = pd.Categorical(data['lifestyle'], categories=lifestyle_order, ordered=True)
    return data

def run_regression_analysis(data, formula):
    """Run regression analysis on the provided data using the specified formula."""
    model = smf.ols(formula, data).fit()
    return model.summary2().tables[1], model

def plot_scatter_and_line(model, data, predictor, response_var):
    """Plot scatter plot with regression line for continuous predictors or mean with CI for categorical."""
    plt.figure(figsize=(10, 6))

    if 'lifestyle' in predictor:
        # Plot mean of response variable for each category of lifestyle with CI
        sns.pointplot(x='lifestyle', y=response_var, data=data, join=False, palette='dark', markers='D', capsize=0.2, errwidth=1)
        plt.title(f"Mean {response_var} by {predictor.replace('[T.', ' ').replace(']', '')}")
    else:
        # Scatter plot with regression line for continuous predictors
        sns.regplot(x=predictor, y=response_var, data=data, scatter=True, ci=95, color='blue', line_kws={'color': 'red'})
        plt.title(f"Effect of {predictor} on {response_var}")

    # Display p-value for the predictor
    p_value = model.pvalues[predictor] if predictor in model.pvalues else np.nan
    plt.figtext(0.2, 0.8, f'p = {p_value:.4e}', ha='center', va='center', fontsize=12, color='red')

    plt.xlabel(predictor.replace('[T.', ' ').replace(']', ''))
    plt.ylabel(response_var)
    plt.tight_layout()
    plt.show()

def main(file_path):
    data = pd.read_csv(file_path)
    data = adjust_lifestyle_column(data)

    response_vars = ["HDL", "LDL", "Chol", "BMI", "Body_Fat", "Pulse_pressure", "Trig", "cvd_risk"]
    model_formulas = {
        'urb_score': "{response_var} ~ urb_score + Age + Gender + Diet + Fasting + Alcohol",
        'h_sol': "{response_var} ~ h_sol + Age + Gender + Diet + Fasting + Alcohol",
        'lifestyle': "{response_var} ~ lifestyle + Age + Gender + Diet + Fasting + Alcohol"
    }

    for response_var in response_vars:
        for metric, formula_template in model_formulas.items():
            formula = formula_template.format(response_var=response_var)
            summary, model = run_regression_analysis(data, formula)

            significant_predictors = summary[(summary['P>|t|'] < 0.05) & (summary.index != 'Intercept')]
            for idx, row in significant_predictors.iterrows():
                if idx in ['urb_score', 'h_sol'] or 'lifestyle' in idx:
                    plot_scatter_and_line(model, data, idx, response_var)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 script.py <file_path>")
        sys.exit(1)
    main(sys.argv[1])

