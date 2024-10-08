import streamlit as st
import pandas as pd

# Load datasets
@st.cache_data
def load_data():
    contestants_df = pd.read_csv('contestants.csv')
    votes_df = pd.read_csv('votes.csv')
    return contestants_df, votes_df

# Preprocess data
def preprocess_data(contestants_df, votes_df):
    # Merge the dataframes
    votes_df['round'] = votes_df['round'].apply(lambda x: 'final' if 'final' in x.lower() else 'semi-final')
    merged_df = pd.merge(votes_df, contestants_df, how='left', left_on=['year', 'to_country_id'], right_on=['year', 'to_country_id'])

    # Calculate total points overall
    merged_df['total_points_overall'] = merged_df.apply(lambda row: row['points_final'] if row['round'] == 'final' else row['points_sf'], axis=1)
    final_df = merged_df[['year', 'round', 'from_country_id', 'to_country_id', 'to_country_y', 'total_points', 'total_points_overall']]
    final_df.columns = ['year', 'round', 'from_country', 'to_country', 'to_country_name', 'points_given', 'total_points']

    # Country participation counts
    unique_years_df = merged_df[['year', 'round', 'to_country_x', 'to_country_y']].drop_duplicates()
    country_counts_df = unique_years_df.groupby(['to_country_x', 'to_country_y']).size().reset_index(name='count')
    country_counts_df.columns = ['country_id', 'country_name', 'count']

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

# Filter countries based on user selection
def filter_countries(final_df, country_counts_df, selected_countries):
    country_mapping = dict(zip(country_counts_df['country_name'], country_counts_df['country_id']))
    selected_country_ids = [country_mapping[country] for country in selected_countries if country in country_mapping]
    return final_df[(final_df['to_country'].isin(selected_country_ids) & final_df['from_country'].isin(selected_country_ids))]

# Streamlit app interface
def main():
    st.title('Eurovision Votes Preprocessing Application')

    # Load data
    contestants_df, votes_df = load_data()

    # User inputs
    weighting_method = st.radio(
        "Select the weighting method for votes:",
        ('No weights', 'Divide by participation count', 'Divide by total points', 'Divide by both')
    )

    min_participations = st.number_input(
        "Minimum number of participations:",
        min_value=0, max_value=100, value=1
    )

    # Preprocess the data
    final_df, country_counts_df = preprocess_data(contestants_df, votes_df)

    # Filter by minimum participation count
    country_counts_df = country_counts_df[country_counts_df['count'] >= min_participations]
    final_df = final_df[final_df['to_country'].isin(country_counts_df['country_id'])]
    final_df = final_df[final_df['from_country'].isin(country_counts_df['country_id'])]

    # Country selection for filtering
    all_countries = country_counts_df['country_name'].unique()
    selected_countries = st.multiselect(
        "Select countries to include in the analysis:",
        options=all_countries, default=all_countries
    )

    final_df = filter_countries(final_df, country_counts_df, selected_countries)
    country_counts_df = country_counts_df[country_counts_df['country_name'].isin(selected_countries)]

    # Calculate weighted votes
    final_df = calculate_weighted_votes(final_df, weighting_method)

    df_filtered = final_df[final_df['from_country'] != final_df['to_country']]
    eurovision_votes = df_filtered.groupby(['from_country', 'to_country'], as_index=False)['weighted_points'].sum()

    # Show results
    st.write("Edgelist:", eurovision_votes)
    st.write("Nodelist:", country_counts_df)

    # Save edges and nodes to CSV
    if st.button('Save edge and node lists as CSV'):
        eurovision_votes.to_csv('edgelist.csv', index=False)
        country_counts_df[['country_id', 'country_name', 'count']].to_csv('nodelist.csv', index=False)
        st.success('Edge and node lists saved!')

if __name__ == "__main__":
    main()

