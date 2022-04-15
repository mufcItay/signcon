
# data <- read.csv('C:\\U\\PhD\\Matan_Nathan\\nondirectionalPriming\\data\\BM_E1.csv')
# get_sign_consistency(data, idv = "subNum", dv = "rt", iv = "cong")
# test_sign_consistency(data, idv = "subNum", dv = "rt", iv = "cong")

# get_sign_consistency(sampled_data_strong, idv = "id", dv = "var", iv = "condition")
res_effect <- test_sign_consistency(sampled_data_effect, idv = "id", dv = "var", iv = "condition", null_dist_samples = 1000)
res <- get_sign_consistency(sampled_data_effect, idv = "id", dv = "var", iv = "condition")

res_strong <- test_sign_consistency(sampled_data_strong, idv = "id", dv = "var", iv = "condition", null_dist_samples = 1000)
res_weak <- test_sign_consistency(sampled_data_weak, idv = "id", dv = "var", iv = "condition", null_dist_samples = 1000)
