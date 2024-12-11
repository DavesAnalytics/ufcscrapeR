<img src="man/figures/ufcscrapeR-hex.png" alt="ufcscrapeR logo" width="300px"/>

# ufcscrapeR

**Author:** Dave Yount

The `ufcscrapeR` package provides a straightforward way to scrape UFC and Pride fight data directly from [UFCStats.com](http://ufcstats.com/). With `ufcscrapeR`, you can quickly access round-by-round statistics, total fight metrics, control times, strikes by location, takedown data, submission attempts, and more—making it ideal for predictive modeling, data visualization, and advanced MMA analytics. This tool is particularly useful for analysts, researchers, and MMA enthusiasts seeking to transform raw fight data into actionable insights.

## Key Features

- **Scrape UFC and Pride Fights:** Retrieve data for any fighter who has participated in the UFC or in Pride Fighting Championships (owned by the UFC).
- **Round-by-Round Data:** Gain granular insights, including strikes landed/attempted, takedowns, submission attempts, and control time for each round.
- **Total Fight Metrics:** Access overall fight statistics, such as total strikes, significant strikes, takedown percentages, and submission attempts.
- **Clean & Structured Output:** The returned data is organized in a user-friendly format, ready for immediate analysis.
- **Integration with R Ecosystem:** Output data as data frames or CSV, making it easy to incorporate into R-based modeling, plotting, or machine learning workflows.

## Installation

Currently, `ufcscrapeR` is not on CRAN. You can install it from source after downloading or cloning the repository:

```r
# Install devtools if not already installed
install.packages("devtools")

# Replace 'path_to_ufcscrapeR' with your local directory path containing ufcscrapeR source
devtools::install_local("path_to_ufcscrapeR")
```


Once installed, load it as usual:
```r
library(ufcscrapeR)
```

## Getting Started

Identify the fighter: You can provide a fighter’s full name, last name, or first name. For example, "Israel Adesanya" or "Adesanya".

Retrieve the data: Use get_ufc_data() to download and parse the fighter’s entire fight history (as available on UFCStats.com).

```r
df <- get_ufc_data("Israel Adesanya")
head(df)
```

This command returns a data frame with columns representing fight-level and round-level statistics.

## Output Format

The returned data frame will contain columns such as:

Basic Info: fighter_name, opponent_name, event_name, event_date, round, Time, method_main (e.g., KO/TKO, SUB, DEC), and method_detail (e.g., Punches, Rear Naked Choke).
Total Fight Metrics (Prefixed with TOT_): TOT_fighter_SigStr, TOT_opponent_SigStr, TOT_fighter_TotalStr, TOT_opponent_TotalStr, TOT_fighter_Td, TOT_opponent_Td, TOT_fighter_SubAtt, TOT_opponent_SubAtt, TOT_fighter_Ctrl, TOT_opponent_Ctrl.
Round-by-Round Data (Prefixed with RoundX_): Detailed per-round metrics for each round, such as Round1_fighter_SigStr, Round1_fighter_SigStr_percentage, Round1_opponent_SigStr, Round1_opponent_Ctrl, etc. These round-wise columns cover significant strikes (by head, body, leg), total strikes, takedown percentages, submissions, reversals, and control times.

## Exporting Results to CSV

If you want to save the output to a CSV file, you can use the helper function export_ufc_data_csv():

```r
export_ufc_data_csv(df, "Israel_Adesanya_Fight_Data.csv")
```

This will write the data frame df to a CSV file named Israel_Adesanya_Fight_Data.csv.

## Use Cases

Predictive Modeling: Use the extracted features (e.g., average strikes per round, takedown success rate, control times) as inputs into a machine learning model to predict future fight outcomes.
Data Visualization: Leverage round-by-round stats to create timeline charts, heatmaps, or fighter comparison plots using packages like ggplot2.
Historical Analysis: Investigate how a fighter's performance metrics have evolved over time or compare metrics between different weight classes or organizations (UFC vs. Pride).
Opponent Scouting: Analyze how different opponents performed against the same fighter, looking for patterns in strike distributions, takedown defenses, or submission attempts.

## Tips & Best Practices

Be Specific with Fighter Names: If the fighter has a common name, start with the last name or try the full name to increase the accuracy of the match.
Check for Missing Data: Some older Pride fights might have less detailed stats available. Always inspect your returned data to ensure completeness.
Update Regularly: UFCStats.com updates after each event. Rerun the scraper periodically to keep your dataset current.
Integration with Other Tools: Combine ufcscrapeR output with your existing R workflows—such as dplyr for data manipulation, ggplot2 for visualization, caret or tidymodels for modeling, and shiny for building interactive web apps around your MMA data.

## Data Dictionary

For a full data dictionary describing all columns, see the included DATA_DICTIONARY.Rmd file in the repository. It provides detailed explanations of each metric, including how they are calculated and interpreted.
Future Enhancements

Multithreading: Speed up scraping by parallelizing requests (where ethically and technically allowed).
Enhanced Data Coverage: Potential integration with other MMA organizations' data sources if they become publicly accessible.
API Integration: If UFCStats.com introduces a public API, ufcscrapeR can evolve to utilize official endpoints, increasing reliability and coverage.

## Getting Help

Issues & Bug Reports: If you encounter any problems, please open an issue on the GitHub repository.
Feature Requests: Suggestions for new features or improvements are always welcome.
Discussions: MMA analytics is a thriving community. Join forums, Slack channels, or local groups to share insights and best practices.

## Contributing

We welcome contributions! Whether it's fixing bugs, adding new features, or improving documentation, your help is appreciated. Please see the CONTRIBUTING.md for guidelines.

This package is released under the MIT License. See the LICENSE file for more details.

Start extracting data and exploring MMA analytics with ufcscrapeR today!
