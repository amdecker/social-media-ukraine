library(rtweet)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(ggthemes)
library(quanteda)
library(data.table)

save(tweets_df, file = "zelensky_tweets")
save(ukraine, file = "ukraine_tweets")
load("./zelensky_tweets")
load("./ukraine_tweets")


app <- rtweet_app(bearer_token = "AAAAAAAAAAAAAAAAAAAAAD1fPgEAAAAAXGEZV6e53RGyJqo56KvWgeFkmyo%3DSAagoZPejIBOOJaUGpP2VhaVpA9fb2sjbq5ed0e0M5OzcIDOye")
auth_as(app)


tweets_df <- get_timeline("ZelenskyyUa", n = Inf, parse = TRUE, retryonratelimit = TRUE)
ukraine <- search_tweets(
    "Ukraine OR #Ukraine", 
    n = 1000000, 
    parse = TRUE, 
    type = "mixed", 
    lang = "en", 
    retryonratelimit = TRUE
)

tweets_df <- tweets_df %>% 
    select(created_at, full_text, retweet_count, favorite_count, lang) %>%
    mutate(year = str_extract(created_at, pattern = "^\\d{4}")) %>%
    mutate(date = str_extract(created_at, pattern = "^\\d{4}-\\d{2}-\\d{2}")) %>%
    filter(year == "2022") %>%
    mutate(week = week(created_at))


# Time line of Zelensky's Tweet Counts
zelensky_datecounts <- tweets_df %>%
    group_by(date) %>%
    count() %>%
    mutate(date = as.Date(date))

ggplot(zelensky_datecounts) +
    geom_line(aes(x = date, y = n), size = 0.7) +
    geom_vline(xintercept = as.Date("2022-02-24"), size = 0.75, color = "red") +
    annotate(geom = "text", label = "2022-02-24", x = as.Date("2022-02-24"), y = 30) +
    theme_stata()

# Language Use of Zelensky's Tweets
zelensky_lang <- tweets_df %>%
    group_by(lang) %>%
    count() %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    top_n(n = 5)
    
ggplot(zelensky_lang) +
    geom_bar(aes(x = reorder(lang, n),
                 y = n, 
                 fill = lang), 
             stat = "identity") +
    labs(x = "Language", 
         y = "Tweet Counts") +
    theme_stata()

# Focus on English Contents
stopword <- tibble(word = stopwords::stopwords(language = "en"))
zelensky_en <- tweets_df %>%
    filter(lang == "en") %>%
    mutate(clean_text = str_replace_all(full_text, 
                                        pattern = "[0-9]+|\\$[0-9]+|https://t.co/[A-Za-z\\d]+|&amp;", 
                                        replacement = "")) %>%
    mutate(id = seq_along(created_at))

zelensky_en_clean <- zelensky_en %>%
    # 斷詞、移除停用字
    tidytext::unnest_tokens(input = clean_text, output = tokenized_text, token = "tweets") %>%
    anti_join(stopword, by = c("tokenized_text" = "word")) %>%
    mutate(year_month = str_extract(created_at, pattern = "^\\d{4}-\\d{2}"))
    

# Tf-idf
zelensky_tfidf <- zelensky_en_clean %>% 
    count(year_month, tokenized_text) %>%
    tidytext::bind_tf_idf(term = tokenized_text, document = year_month, n = n) %>%
    # 選取出每月TF-IDF值最高的10字
    group_by(year_month) %>%
    arrange(desc(tf_idf)) %>%
    filter(nchar(tokenized_text) >= 3) %>%
    slice_head(n = 10) %>%
    ungroup()

ggplot(zelensky_tfidf) +
    geom_bar(aes(x = tidytext::reorder_within(x = tokenized_text, 
                                              by = tf_idf, 
                                              within = year_month), 
                 y = tf_idf, 
                 fill = year_month), 
             stat = "identity") +
    tidytext::scale_x_reordered() +
    # 以year_month為類別變項繪製圖表
    facet_wrap(vars(year_month), scales = "free") +
    # 增加標題、副標、xy軸名稱、出處
    labs(title = "Zelensky's Salient Wording", 
         subtitle = "2022/01/01-present", 
         x = "tokens", 
         y = "TF-IDF") + 
    # 翻轉xy軸
    coord_flip() +
    # 移除圖例
    theme(legend.position = "none")

# Retweets
zelensky_dfm <- zelensky_en_clean %>% 
    count(id, tokenized_text) %>% 
    # 轉換成dfm的格式
    tidytext::cast_dfm(document = id, term = tokenized_text, value = n)

zelensky_fcm <- quanteda::fcm(
    x = zelensky_dfm,
    context = "document", # dfm只能設document不能設window
    count = "frequency", 
    window = 10L, 
    ordered = FALSE)

sum(zelensky_fcm)
# 取出 feature frequency 前50大的詞，然後只取出這些字
feat <- names(quanteda::topfeatures(zelensky_fcm, 50))
# fcm中只留下前50常出現的詞彙
zelensky_fcm_top <- fcm_keep(zelensky_fcm, pattern = feat)
dim(zelensky_fcm_top)
size <- log(colSums(zelensky_fcm_top))

# 接著要把共現網絡的詞彙畫出來
library(quanteda.textplots)
library(quanteda.textmodels)
set.seed(232)
zelensky_co_word <- textplot_network(zelensky_fcm_top, min_freq = 0.85, 
                                 vertex_size = size / max(size) * 3, 
                                 vertex_labelsize = 6, 
                                 vertex_labelcolor = "red") +
    labs(title = "澤倫斯基推文詞共現關聯圖", 
         subtitle = "2022/01-2022/12") +
    theme(plot.title = element_text(hjust = 0.5, size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 15), 
          plot.caption = element_text(size = 15))

zelensky_co_word

UA <- fread("./combined_csv1.csv", sep = ",", encoding = "UTF-8")
