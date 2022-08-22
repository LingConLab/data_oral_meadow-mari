library(tidyverse)
files <- list.files("data", pattern = "json")

map_dfr(files, function(json){
  js <- jsonlite::read_json(str_c("data/", json))
  
  map_dfr(seq_along(js$sentences), function(i){
    js$sentences[[i]]$words %>% 
      map("wf") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      word_forms
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("gloss_index") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      gloss
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("parts") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      morphonology
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("parts") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      morphonology
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("src") ->
      source_file
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("off_start_src") ->
      time_start
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("off_end_src") ->
      time_end
    
    tibble(filename = source_file,
           time_start = time_start,
           time_end = time_end,
           speaker = js$sentences[[i]]$meta$speaker,
           recorded = js$meta$year,
           lang = js$sentences[[i]]$lang,
           text = js$sentences[[i]]$text,
           word_forms,
           morphonology,
           gloss,
           language = "east2328",
           dataset_creator  = "Anna Volkova, Aigul Zakirova, Mikhail Voronov, Maria Dolgodvorova, Zinaida Klyucheva, Svetlana Kokoreva, Ilya Makarchuk, Irina Khomchenkova, Timofey Arkhangelskiy, Elena Sokur",
           dataset_provider = "George Moroz") 
  }) 
}) ->
  result

result %>% 
  distinct(filename, recorded, lang, text) %>% 
  group_by(filename, recorded, lang) %>%
  mutate(sentence_id = 1:n()) %>% 
  pivot_wider(names_from = lang, values_from = text) %>% 
  rename(text = `0`,
         translation = `1`) ->
  translation_pairs

result %>% 
  filter(lang == 0,
         word_forms != "\n",
         word_forms != "") %>% 
  left_join(translation_pairs) %>%
  mutate(text = str_remove_all(text, "^\n"),
         text = str_remove_all(text, "\n$"),
         translation = str_remove_all(translation, "^\n"),
         translation = str_remove_all(translation, "\n$")) %>% 
  select(filename, time_start, time_end, speaker, recorded, sentence_id, text, translation, word_forms, morphonology, gloss, language, dataset_creator, dataset_provider, sentence_id, translation)  ->
  result
  
# anonimyzing -------------------------------------------------------------

result %>% 
  distinct(speaker) %>%
  mutate(new_name = speaker %>% factor() %>% as.double() %>% as.character()) ->
  speaker_ids

speaker_ids %>% 
  mutate(speaker = str_to_upper(speaker),
         speaker = case_when(speaker == "YEPT" ~ "YePT",
                   speaker == "VVKA" ~ "VVKa",
                   speaker == "AVYE" ~ "AVYe",
                   speaker == "YNP" ~ "YeNP",
                   TRUE ~ speaker)) %>% 
  bind_rows(speaker_ids) ->
  speaker_ids

map(seq_along(speaker_ids$speaker), function(i){
  result %>% 
    mutate(speaker = str_replace(speaker, 
                                 speaker_ids$speaker[i], 
                                 speaker_ids$new_name[i]),
           text = str_replace_all(text, 
                                  speaker_ids$speaker[i], 
                                  speaker_ids$new_name[i]),
           translation = str_replace_all(translation,
                                         speaker_ids$speaker[i], 
                                         speaker_ids$new_name[i]),
           word_forms = str_replace(word_forms, 
                                   speaker_ids$speaker[i], 
                                   speaker_ids$new_name[i]),
           filename = str_replace(filename,
                                  speaker_ids$speaker[i],
                                  "***")) ->>
    result
})

result %>%
  mutate(language = ifelse(text == translation, "russ1263", language),
         text = str_remove_all(text, "^\n")) %>% 
  filter(text != "[2] Мылам коч Путин лийже,",
         text != "коч Мутин лийже,",
         text != "ынде мыланем эре всё равно.") %>% 
  write_csv("data_oral_meadow-mari.csv")
