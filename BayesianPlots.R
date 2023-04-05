ggplot(dbmte_pos, 
       aes(x = yield,
           y = titre)) +
  geom_point() +
  geom_smooth()

ggplot(data_brms,
       aes(x = meantitrenegcows,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
       aes(x = titre, y = bayesfactor)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Bayes Factor (Raw)")

ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
       aes(x = titre, y = bayesfactor_pred)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Bayes Factor (LM Predicted")


plot_ly(x = bf_tab$titre, y = bf_tab$age, z = bf_tab$bayesfactor_pred)

