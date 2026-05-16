import argparse
from datetime import datetime

import pandas as pd
from sklearn import linear_model
from sklearn import preprocessing

def load_data():
  """
  Load the data from the CSV file.
  """
  data = pd.read_csv("./data/team_comparison.csv")
  data["date"] = pd.to_datetime(data["date"])
  return data

def compute_derived_features(data: pd.DataFrame):
  # Drop the opponent column as it only exists to be able to filter games
  # played versus a specific opponent.
  data = data.drop(columns=["opponent"])

  # Calculate the point difference between the team and the opponent. This is
  # the target variable we want to predict.
  data["pts_diff"] = data["pts"] - data["opp_pts"]

  # Calculate a proxy for offensive and defensive ratings (number of points
  # scored and allowed per 100 possessions).
  data["offensive_rating"] = data["pts"] / data["possession"]
  data["defensive_rating"] = data["opp_pts"] / data["opp_possession"]

  data = data.drop(columns=["pts", "opp_pts", "possession", "opp_possession"])

  # From the date column, extract the number of days since the previous game.
  # The zeroth game is assumed to be 30 days ago. Discard the date column
  # after the calculation as it is not needed by itself for the prediction.
  data["days_since_last_game"] = data["date"].diff().dt.days
  data.loc[0, "days_since_last_game"] = 30
  data = data.drop(columns=["date"])

  # Calculate stats that benefit from percentages and ratios. To avoid division
  # by zero, add a small constant to the denominator.
  ratiod_stats = [
    ["fgm", "fga"], ["3ptm", "3pta"], ["ftm", "fta"], ["assists", "to"],
  ]
  for numerator, denominator in ratiod_stats:
    opp_numerator, opp_denominator = f"opp_{numerator}", f"opp_{denominator}"
    ratiod_column, opp_ratiod_column = f"{numerator}_per_{denominator}", f"opp_{numerator}_per_{denominator}"
    data[ratiod_column] = data[numerator] / (.01 + data[denominator])
    data[opp_ratiod_column] = data[opp_numerator] / (.01 + data[opp_denominator])

    for column in [ratiod_column, opp_ratiod_column]:
      data[column] = data[column].fillna(0)

  # Calculate the difference between the team's and the opponent's statistics.
  stats_for_differentials = [
    "fgm", "fga", "fgm_per_fga", "3ptm", "3pta", "3ptm_per_3pta", "ftm", "fta",
    "ftm_per_fta", "fast_break_pts", "paint_pts", "second_chance_pts",
    "bench_pts", "assists", "off_reb", "def_reb", "stl", "blk", "to",
    "assists_per_to", "team_fouls",
  ]
  for column in stats_for_differentials:
    data[f"{column}_diff"] = data[column] - data[f"opp_{column}"]
    data = data.drop(columns=[column, f"opp_{column}"])

  return data

def preprocess_data(data: pd.DataFrame):
  y = data["pts_diff"]
  X = data.drop(columns=["pts_diff"])

  # Preprocess the data.
  # https://scikit-learn.org/stable/modules/preprocessing.html

  # Normalize the data to be in the range [0, 1]. That way, features with a
  # larger range do not dominate the model.
  scaler = preprocessing.StandardScaler().fit(X)

  return scaler.transform(X), y, X.columns

def bayesian_ridge_regression(data: pd.DataFrame):
  X, y, features = preprocess_data(data)

  clf = linear_model.BayesianRidge()
  clf.fit(X, y)

  features_with_coefficients = sorted(
    zip(clf.coef_, features), key=lambda x: abs(x[0]), reverse=True)

  print("| | Feature | Coefficient |")
  print("| --- | --- | --- |")
  for idx, (coefficient, feature) in enumerate(features_with_coefficients):
    feature = feature.replace("_diff", " ").upper()
    print(f"| {idx + 1} | {feature} | {coefficient:.4f} |")

def main():
  # Parse command line arguments: --start, --end, --opponent
  parser = argparse.ArgumentParser(
    description="Analyze NBA 2K MyNBA team comparison data.")
  parser.add_argument(
    "--start", type=datetime.fromisoformat,
    help="Start date in YYYY-MM-DD format. Inclusive.")
  parser.add_argument(
    "--end", type=datetime.fromisoformat,
    help="End date in YYYY-MM-DD format. Inclusive.")
  parser.add_argument(
    "--home", help="Filter to home games.", action='store_true')
  parser.add_argument(
    "--away", help="Filter to away games.", action='store_true')
  args = parser.parse_args()

  data = load_data()
  N = len(data)
  if args.start:
    data = data[data["date"] >= args.start]
  if args.end:
    data = data[data["date"] <= args.end]
  if args.home:
    data = data[data["home"]]
  if args.away:
    data = data[~data["home"]]
  print(f"Loaded {len(data)} out of {N} games.")

  data = compute_derived_features(data)
  bayesian_ridge_regression(data)

if __name__ == "__main__":
  main()
