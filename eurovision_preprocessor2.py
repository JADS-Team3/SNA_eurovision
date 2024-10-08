import streamlit as st
import pandas as pd

# Load datasets
@st.cache_data
def load_data():
    contestants_df = pd.read_csv('contestants.csv')
    votes_df = pd.read_csv('votes.csv')
    cultural_df = pd.read_csv('hofstede_dimensions.csv')
    return contestants_df, votes_df, cultural_df

def convert_df_to_csv(df):
    return df.to_csv(index=False).encode('utf-8')

# Preprocess data
def preprocess_data(contestants_df, votes_df, cultural_df, top3_only, final_only):
    # Merge the dataframes
    votes_df['round'] = votes_df['round'].apply(lambda x: 'final' if 'final' in x.lower() else 'semi-final')
    
    # Handle "TOP 3 ONLY" (Filter votes with points >= 8)
    if top3_only:
        votes_df = votes_df[votes_df['total_points'] >= 8]
    
    # Handle "ONLY FINAL" (Filter votes with "final" round)
    if final_only:
        votes_df = votes_df[votes_df['round'] == 'final']
    
    merged_df = pd.merge(votes_df, contestants_df, how='left', left_on=['year', 'to_country_id'], right_on=['year', 'to_country_id'])

    # Calculate total points overall
    merged_df['total_points_overall'] = merged_df.apply(lambda row: row['points_final'] if row['round'] == 'final' else row['points_sf'], axis=1)
    final_df = merged_df[['year', 'round', 'from_country_id', 'to_country_id', 'total_points', 'total_points_overall']]
    final_df.columns = ['year', 'round', 'from_country', 'to_country', 'points_given', 'total_points']

    # Country participation counts (handling duplicate names)
    unique_years_df = merged_df[['year', 'round', 'to_country_x', 'to_country_y', 'to_country_id']].drop_duplicates()
    
    # Group countries with the same country_id but different names
    country_counts_df = unique_years_df.groupby('to_country_id').agg({'to_country_y': 'first', 'year': 'size'}).reset_index()
    country_counts_df.columns = ['country_id', 'country_name', 'count']

    country_counts_df = pd.merge(country_counts_df, cultural_df, how='left', on='country_id')

    # Participation map
    participation_map = country_counts_df.set_index('country_id')['count'].to_dict()
    final_df['to_country_participation'] = final_df['to_country'].map(participation_map)
    
    return final_df, country_counts_df

# Weighted vote calculation based on user selection
def calculate_weighted_votes(final_df, weighting_method):
    if weighting_method == 'No weights':
        final_df['weighted_points'] = final_df['points_given']
    elif weighting_method == 'Divide by participation count':
        final_df['weighted_points'] = final_df['points_given'] / final_df['to_country_participation']
    elif weighting_method == 'Divide by total points':
        final_df['weighted_points'] = final_df['points_given'] / final_df['total_points']
    elif weighting_method == 'Divide by both':
        final_df['weighted_points'] = final_df['points_given'] / (final_df['total_points'] * final_df['to_country_participation'])
    return final_df

# Filter countries based on user selection (by IDs)
def filter_countries(final_df, country_counts_df, selected_countries):
    country_mapping = dict(zip(country_counts_df['country_name'], country_counts_df['country_id']))
    selected_country_ids = [country_mapping[country] for country in selected_countries if country in country_mapping]
    return final_df[(final_df['to_country'].isin(selected_country_ids) & final_df['from_country'].isin(selected_country_ids))]

# Streamlit app interface
def main():
    st.title('Eurovision Votes Preprocessing Application')

    # Load data
    contestants_df, votes_df, cultural_df = load_data()

    # User inputs for additional functionality
    top3_only = st.checkbox('TOP 3 ONLY (Votes with 8 or more points)', value=False)
    final_only = st.checkbox('ONLY FINAL (Final round votes only)', value=False)

    weighting_method = st.radio(
        "Select the weighting method for votes:",
        ('No weights', 'Divide by participation count', 'Divide by total points', 'Divide by both')
    )

    min_participations = st.number_input(
        "Minimum number of participations:",
        min_value=0, max_value=100, value=1
    )

    # Preprocess the data with top3_only and final_only options
    final_df, country_counts_df = preprocess_data(contestants_df, votes_df, cultural_df, top3_only, final_only)

    # Filter by minimum participation count
    country_counts_df = country_counts_df[country_counts_df['count'] >= min_participations]
    final_df = final_df[final_df['to_country'].isin(country_counts_df['country_id'])]
    final_df = final_df[final_df['from_country'].isin(country_counts_df['country_id'])]


    # Country selection for filtering (name-based but maps to IDs)
    all_countries = country_counts_df['country_name'].unique()
    selected_country_names = st.multiselect(
        "Select countries to include in the analysis:",
        options=all_countries, default=all_countries
    )

    # Filter final_df by selected country IDs
    final_df = filter_countries(final_df, country_counts_df, selected_country_names)
    country_counts_df = country_counts_df[country_counts_df['country_name'].isin(selected_country_names)]

    # Calculate weighted votes
    final_df = calculate_weighted_votes(final_df, weighting_method)

    df_filtered = final_df[final_df['from_country'] != final_df['to_country']]
    eurovision_votes = df_filtered.groupby(['from_country', 'to_country'], as_index=False)['weighted_points'].sum()

    eurovision_votes = eurovision_votes[eurovision_votes['weighted_points'] != 0]

    col1, col2 = st.columns(2)
    with col1:
        st.write("Edgelist:", eurovision_votes)
    with col2:
        st.write("Nodelist:", country_counts_df)


    # Convert dataframes to CSV
    edge_list_csv = convert_df_to_csv(final_df[['from_country', 'to_country', 'weighted_points']])
    node_list_csv = convert_df_to_csv(country_counts_df)

    # Provide download buttons for each CSV
    st.download_button(
        label="Download Edge List as CSV",
        data=edge_list_csv,
        file_name='eurovision_edgelist.csv',
        mime='text/csv',
    )

    st.download_button(
        label="Download Node List as CSV",
        data=node_list_csv,
        file_name='eurovision_nodelist.csv',
        mime='text/csv',
    )


if __name__ == "__main__":
    main()
