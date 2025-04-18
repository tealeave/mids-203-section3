import requests  
from bs4 import BeautifulSoup, Comment  
import pandas as pd  
import time  

# List of NBA team abbreviations for the 2023 season on Basketball-Reference.  
team_codes = [  
    "ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET",  
    "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN",  
    "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS",  
    "TOR", "UTA", "WAS", 
]

url_pattern = "https://www.basketball-reference.com/teams/{}/2023.html"

def scrape_roster(team_code):  
    url = url_pattern.format(team_code)  
    print(f"Processing team {team_code} from {url}")  
    response = requests.get(url)  
    if response.status_code != 200:  
        print(f"Failed to retrieve data for team {team_code}")  
        return None  
      
    # Try using lxml parser for improved parsing  
    soup = BeautifulSoup(response.text, "lxml")  

    # First, attempt to find the roster table directly  
    roster_table = soup.find("table", {"id": "roster"})  
      
    if roster_table is None:  
        # If not found directly, also scan through comments  
        comments = soup.find_all(string=lambda text: isinstance(text, Comment))  
        for comment in comments:  
            if 'id="roster"' in comment:  
                comment_soup = BeautifulSoup(comment, "lxml")  
                roster_table = comment_soup.find("table", {"id": "roster"})  
                if roster_table is not None:  
                    break  
      
    if roster_table is None:  
        print(f"Roster table not found for team {team_code}")  
        return None  

    # Use pandas to read the HTML table  
    df = pd.read_html(str(roster_table))[0]

    # Define desired columns  
    desired_columns = ["No.", "Player", "Pos", "Ht", "Wt", "Birth Date", "Birth", "Exp", "College"]
    available_cols = list(df.columns)  
    print("Columns found:", available_cols)

    # Map alternative names (for example, if 'Birth' might be 'Birthplace')  
    col_mapping = {}  
    for col in desired_columns:  
        if col in available_cols:  
            col_mapping[col] = col  
        else:  
            if col == "Birth":  
                # Look for a column starting with "Birth"  
                for ac in available_cols:  
                    if ac.startswith("Birth"):  
                        col_mapping[col] = ac  
                        break  
      
    selected_cols = [orig for orig in desired_columns if orig in col_mapping]  
    df = df[[col_mapping[c] for c in selected_cols]]  
      
    # Add team code column  
    df["Team"] = team_code  
      
    return df

df_list = []
for code in team_codes:  
    team_df = scrape_roster(code)  
    if team_df is not None:  
        df_list.append(team_df)  
    time.sleep(1)  # be respectful to the server

if df_list:  
    all_rosters = pd.concat(df_list, ignore_index=True)  
    print("Combined roster dataframe:")  
    print(all_rosters.head())  
    all_rosters.to_csv("nba_rosters_2023.csv", index=False)  
else:  
    print("No data scraped.")
