import pandas as pd
from sklearn.model_selection import train_test_split

def combine_and_add_star_player(stats_file, rosters_file, all_star_file, market_file):
    """
    Combines NBA stats and roster data, and adds a binary 'star_player' column.

    Args:
        stats_file (str): Path to the CSV file containing player stats and salary.
        rosters_file (str): Path to the CSV file containing player roster information.
        all_star_file (str): Path to the CSV file containing the list of all-star players.

    Returns:
        pandas.DataFrame: A DataFrame containing the combined data with the added 'star_player' column.
    """

    # Load the data
    df_stats = pd.read_csv(stats_file).drop(columns=['Unnamed: 0'])  # Drop the unwanted index column
    # Duplicate check for df_stats
    stats_dupes = df_stats['Player Name'].duplicated()

    num_stats_dupes = stats_dupes.sum()
    if num_stats_dupes > 0:
        print(f"Found {num_stats_dupes} duplicates in 'Player Name' of df_stats:")
        print(df_stats[stats_dupes]['Player Name'].value_counts())
    else:
        print("No duplicates found in 'Player Name' of df_stats.")

    # Handle the traded players, if "/" is in the col "Team", split it to 2 rows by /
    # For example, if "MIN/UTA" is in the "Team" column, split it into two rows: "MIN" and "UTA" and keep all the other columns the same
    df_stats['Team'] = df_stats['Team'].str.split('/').apply(lambda x: [team.strip() for team in x])
    df_stats = df_stats.explode('Team')
    df_stats['Team'] = df_stats['Team'].str.strip()  # Remove any leading/trailing whitespace


    df_rosters = pd.read_csv(rosters_file)
    # Duplicate check for df_rosters
    rosters_dupes = df_rosters['Player'].duplicated()
    num_rosters_dupes = rosters_dupes.sum()
    if num_rosters_dupes > 0:
        print(f"Found {num_rosters_dupes} duplicates in 'Player' of df_rosters, these are traded player so have 2 teams:")
        print(df_rosters[rosters_dupes]['Player'].value_counts())
        # Save duplicated rows for evaluation
        df_rosters[rosters_dupes].to_csv('nba_rosters_2023_traded_players.csv', index=False)
        print("Traded players rows saved to nba_rosters_2023_traded_players.csv for double check.")

    else:
        print("No duplicates (traded players) found in 'Player' of df_rosters.")

    df_all_stars = pd.read_csv(all_star_file)
    # Duplicate check for df_all_stars
    allstars_dupes = df_all_stars['Player'].duplicated()
    num_allstars_dupes = allstars_dupes.sum()
    if num_allstars_dupes > 0:
        print(f"Found {num_allstars_dupes} duplicates in 'Player' of df_all_stars:")
        print(df_all_stars[allstars_dupes]['Player'].value_counts())
    else:
        print("No duplicates found in 'Player' of df_all_stars.")

    # Left merge the stats and rosters dataframes on player names
    df_combined = pd.merge(df_stats, df_rosters, left_on=['Player Name','Team'], right_on=['Player','Team'], how='left').drop(columns=['Player'])
    # Duplicate check for df_combined
    combined_dupes = df_combined.duplicated(subset=['Player Name', 'Team'])  # Corrected syntax for multiple columns
    num_combined_dupes = combined_dupes.sum()
    if num_combined_dupes > 0:
        print(f"Found {num_combined_dupes} duplicates in ['Player Name','Team'] of df_combined:")
        print(df_combined[combined_dupes][['Player Name', 'Team']].value_counts())
    else:
        print("No duplicates found in ['Player Name', 'Team'] of df_combined.")

    # Add market size data
    df_market = pd.read_csv(market_file)
    # Duplicate check for df_market
    market_dupes = df_market['team'].duplicated()
    num_market_dupes = market_dupes.sum()
    if num_market_dupes > 0:
        print(f"Found {num_market_dupes} duplicates in 'team' of df_market:")
        print(df_market[market_dupes]['team'].value_counts())
    else:
        print("No duplicates found in 'team' of df_market.")

    # Merge market size info into combined dataframe
    df_combined = pd.merge(df_combined, df_market, left_on='Team', right_on='team', how='left').drop(columns=['team'])

    # Duplicate check after adding market size
    combined_dupes2 = df_combined[['Player Name','Team']].duplicated()
    num_combined_dupes2 = combined_dupes2.sum()
    if num_combined_dupes2 > 0:
        print(f"After market size merge: Found {num_combined_dupes2} duplicates in ['Player Name','Team'] of df_combined:")
        print(df_combined[combined_dupes2][['Player Name','Team']].value_counts())
    else:
        print("After market size merge: No duplicates found in ['Player Name','Team'] of df_combined.")

    # Create a set of all-star players
    all_star_set = set(df_all_stars['Player'])

    # Create the star_player column: 1 if the player is in the all-star set, else 0
    df_combined['star_player'] = df_combined['Player Name'].apply(lambda x: 1 if x in all_star_set else 0)

    return df_combined

if __name__ == '__main__':
    # Define file paths
    stats_file = 'nba_2022-23_all_stats_with_salary.csv'
    rosters_file = 'nba_rosters_2023.csv'
    all_star_file = 'nba_2023_all_star_team.csv'
    market_file = 'nba_market_size.csv'

    # Call the function and get the combined DataFrame
    combined_df = combine_and_add_star_player(stats_file, rosters_file, all_star_file, market_file)
    
    # We will remove the duplicate row in Player Name col, these are traded players
    # and keep the first one
    combined_df = combined_df.drop_duplicates(subset=['Player Name'], keep='first')

    # Duplicate check after removing duplicates
    combined_dupes = combined_df['Player Name'].duplicated()
    num_combined_dupes = combined_dupes.sum()
    if num_combined_dupes > 0:
        print(f"Found {num_combined_dupes} duplicates in 'Player Name' of combined_df:")
        print(combined_df[combined_dupes]['Player Name'].value_counts())
    else:
        print("No duplicates found in 'Player Name' of combined_df.")

    # Check for any rows with salary or GP = 0 and remove them
    invalid_rows = combined_df[(combined_df['Salary'] == 0) | (combined_df['GP'] == 0)]
    if not invalid_rows.empty:
        print("Rows with Salary or GP = 0:")
        print(invalid_rows)
        combined_df = combined_df.drop(invalid_rows.index)
        print("Invalid rows removed.")
    else:
        print("No rows with Salary or GP = 0 found.")

    # Check for missing values in the specified columns
    columns_to_check = [
        'Salary', 'PTS', 'MP', 'FG', 'TRB', '3P', 'TOV', 'WS', 'GP', 'AST', 
        'tv_market_size', 'TS%', 'star_player', 'Position', 'Age', 'Exp', 'Team'
    ]
    missing_values = combined_df[columns_to_check].isnull().sum()
    total_missing = missing_values.sum()
    missing_rows_count = len(combined_df[combined_df[columns_to_check].isnull().any(axis=1)])

    if total_missing > 0:
        print(f"Summary: Found {total_missing} missing values across {missing_rows_count} rows within the specified columns.")
        # Optionally, still print which columns have missing values if needed:
        print("Columns with missing values (specified subset):")
        print(missing_values[missing_values > 0])
        # Print the rows with missing values in the specified columns
        print("Rows with missing values in specified columns:")
        print(combined_df[combined_df[columns_to_check].isnull().any(axis=1)])
    else:
        print("Summary: No missing values found within the specified columns.")



    # # Save a copy of the combined DataFrame
    # combined_df.to_csv('nba_2023_combined.csv', index=False)

    # # Split the combined DataFrame:
    # # - eda_df: 30% subset for exploratory data analysis
    # # - confirmatory_df: 70% subset for confirmatory analysis
    # eda_df, confirmatory_df = train_test_split(combined_df, test_size=0.70, stratify=combined_df['Team'], random_state=413)

    # # Check the amount of star players in each split
    # eda_star_count = eda_df['star_player'].sum()
    # confirmatory_star_count = confirmatory_df['star_player'].sum()
    # print(f"EDA split star players count: {eda_star_count}")
    # print(f"Confirmatory split star players count: {confirmatory_star_count}")

    # # Chcek if LeBron James is in the EDA split
    # lebron_in_eda = eda_df[eda_df['Player Name'] == 'LeBron James']
    # if not lebron_in_eda.empty:
    #     print("LeBron James is in the EDA split:")
    #     print(lebron_in_eda)
    # else:
    #     print("LeBron James is not in the EDA split.")

    # # Save the splits to CSV files
    # eda_df.to_csv('eda.csv', index=False)
    # confirmatory_df.to_csv('confirmatory.csv', index=False)

