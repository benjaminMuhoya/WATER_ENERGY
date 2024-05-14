#!/usr/bin/env python3

import pandas as pd
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import seaborn as sns
import sys

def adjust_lifestyle_column(data):
    """Adjust the 'lifestyle' column to ordered categories."""
    lifestyle_order = ['Pastoralist', 'Rural', 'Market_Integrated']
    data['lifestyle'] = pd.Categorical(data['lifestyle'], categories=lifestyle_order, ordered=True)
    return data

def run_regression_analysis(data, formula):
    """Run regression analysis using the specified formula."""
    model = smf.ols(formula, data).fit()
    return model

def plot_age_lifestyle_interaction(data, response_var, metric):
    """Plot regression line showing interaction between lifestyle metric and Age for a response variable."""
    assert metric == "lifestyle", "This function is specifically designed for lifestyle metric."
    
    # Ensure the lifestyle column is adjusted for ordered categories
    data = adjust_lifestyle_column(data)

    # Define a list of colors to visually distinguish the lifestyle categories
    colors = ['black', 'red', 'blue']  # Assuming 3 categories; adjust if more
    palette = sns.color_palette(colors[:len(data['lifestyle'].cat.categories)])
    
    plt.figure(figsize=(12, 8))
    sns.lmplot(x="Age", y=response_var, hue=metric, data=data, palette=palette, aspect=1.5, ci=None, legend_out=True)

    plt.title(f"Interaction of {metric} and Age on {response_var}")
    plt.xlabel("Age")
    plt.ylabel(response_var)
    
    # Save the plot
    filename = f"./{response_var}_{metric}_age_interaction.png"
    plt.savefig(filename)
    plt.close()

def main(file_path):
    data = pd.read_csv(file_path)
    response_var = "Pulse_pressure"
    metric = "lifestyle"
    
    # Plot the significant age interaction for Pulse_pressure and lifestyle
    plot_age_lifestyle_interaction(data, response_var, metric)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 script.py <file_path>")
        sys.exit(1)
    main(sys.argv[1])

