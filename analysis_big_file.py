import pandas as pd
from nltk import tokenize
from nltk.sentiment.vader import SentimentIntensityAnalyzer

def only_english():
    """
    Creates a sample, 1/10000 tweets from the general twitter posts, grabbing only English tweets

    Saves the sample to sampled.csv
    :return:
    """
    chunksize = 10 ** 4
    i = 0
    sampled = pd.DataFrame()
    # sampled.columns = ['lang', 'created_at', 'retweet_count', 'full_text', 'userid', 'tweetid', 'sentiment']
    with pd.read_csv("tweets\\combined_csv1.csv", chunksize=chunksize, engine='python') as reader:
        for chunk in reader:
            en = chunk[chunk["language"] == "en"]
            one_row = en.iloc[0]
            add_sentiment_column(one_row)
            sampled = sampled.append(one_row)
            print(i, "rownum", chunksize * (i + 1))
            i += 1
    sampled.to_csv("combined\\sampled.csv")


def add_sentiment_column(df):
    """
    Adds a sentiment column to the dataset based on the sentences in the tweet.
    :param df:
    :return:
    """
    sum_compounds = 0
    lines_list = tokenize.sent_tokenize(df["text"])
    num_sentences_per_tweet = len(lines_list)
    for sentence in lines_list:
        sid = SentimentIntensityAnalyzer()
        ss = sid.polarity_scores(sentence)
        sum_compounds += ss["compound"]
    df["sentiment"] = sum_compounds / num_sentences_per_tweet

only_english()



