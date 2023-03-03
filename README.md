# fantasy-data-science
An R project from summer 2022 to analyze NFL fantasy football statistics and create draft rankings.

Files:
- scraping.R contains the script to scrape the data source (fantasypros.com)
  - scrape one position, one year, one week at a time
    - do for 4 most recent years
  - do minimal cleaning on the data, including fixing some column names
  - save csv data files for each week
  - save csv data files for all weeks in each year, arranged by Player and descending fantasy points
  - save csv data files for each full year aggregated
  - save csv data files for all years, arranged by Player and descending fantasy points

- **cleaning.R
  - each position has its own script
  - summarize data using the "all weeks" csv data files
    - create new columns which summarized the data 
      - games played, games played percentage, pass/run attempts, targets, etc
      - mean, median, standard deviation for fantasy points
      - made up variables like adjusted means, good-bad game percentages
      - "Score" which aimed to assign a certain value to each player based on combinations of all of the above columns
        - combined values like median, adjusted mean, good-bad percentages, game percentages, and others
    - remove extraneous columns
  - summarize across the four seasons by created a weighted average of Score
    - most recent season: 60%
    - two seasons ago: 25%
    - three seasons ago: 10%
    - four seasons ago: 5%
  - save an "all score" csv data file

- ranking.R
  - script joins all of the cleaned and summarized "all score" csv data files from each position
  - creates a new "Value" column which takes "Score" and applies a positional value to it
    - RBs, WRs, TEs, and DSTs are full value
    - Ks are 90% value of those
    - QBs are 50% value of those
  - arranges by descending Value
  - mutates extra columns to assist the table viewer
    - Rank (row number), Round (each ten players is another round higher)
  - creates csv data files for each position's rankings to be viewed individually or side by side
  - creates an "all rankings" csv data file binding all of the rankings into one
  - has a small script at the bottom to only be ran when mock draft files are present
    - used to practice multiple mock drafts with the rankings and track where players tend to fall in relation to these rankings
    
- /data contains all of the csv files created

- /data_backup contains a backup of the csv files, manually copied

- /old rankings contains rankings files from before the current season
