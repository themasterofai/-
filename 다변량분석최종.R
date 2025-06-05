# 📦 패키지 로드
library(FactoMineR)
library(factoextra)
library(cluster)
library(dplyr)

# 1. 데이터 불러오기
df <- read.csv("klips26p_processed.csv")  # 파일명 맞게 수정

# 2. 필요한 변수 지정
personal_vars <- c("여자", "연령", "혼인상태", "가구주", "학력", "주관적건강", "건강제한")
outcome_vars  <- c("월평균임금.로그.", "주당근로시간", "취업안정성만족도", 
                   "사회보험가입개수", "직무만족도")
contract_vars <- c("기간제", "비기간제_갱신", "비기간제_불안정", 
                   "시간제", "파견", "용역", "도급", "임시직", "일용직")

# 전체 변수 한번에
all_vars <- c(personal_vars, outcome_vars, contract_vars)

# 필요한 변수만 추출
df_sub <- df[, all_vars]
df_sub <- na.omit(df_sub)  # 결측 제거

# 조건: contract_vars 중 하나라도 1인 행만 남기기
df_sub <- df_sub %>%
  filter(rowSums(select(., all_of(contract_vars)) == 1, na.rm = TRUE) > 0)

# 3. 만족도 변수 변환
# 취업안정성만족도: 1(매우 만족) ~ 5(매우 불만족)
df_sub$취업안정성만족도 <- factor(df_sub$취업안정성만족도,
                          levels = 1:5,
                          labels = c("매우 만족", "만족", "보통", "불만족", "매우 불만족"),
                          ordered = TRUE)

df_sub$직무만족도 <- factor(df_sub$직무만족도,
                       levels = 1:5,
                       labels = c("매우 만족", "만족", "보통", "불만족", "매우 불만족"),
                       ordered = TRUE)


# 4. 성과 변수만 추출하여 FAMD
outcome_famd_vars <- c("월평균임금.로그.", "주당근로시간", "사회보험가입개수", 
                       "취업안정성만족도", "직무만족도")

df_outcome_famd <- df_sub[, outcome_famd_vars]
famd_outcome <- FAMD(df_outcome_famd, ncp = 3, graph = FALSE)
famd_dims <- famd_outcome$ind$coord  # 성과 주성분

# 5. 개인 특성 변수 원본 그대로
df_personal <- df_sub[, personal_vars]
df_personal$여자 <- as.factor(df_personal$여자)  # 범주형 처리
df_personal$혼인상태 <- as.factor(df_personal$혼인상태)
df_personal$가구주 <- as.factor(df_personal$가구주)
df_personal$학력 <- as.factor(df_personal$학력)
df_personal$주관적건강 <- as.factor(df_personal$주관적건강)
df_personal$건강제한 <- as.factor(df_personal$건강제한)

# 6. 결합 → 군집분석
cluster_input <- cbind(df_personal, famd_dims)

# 7. Gower 거리 
gower_dist <- daisy(cluster_input, metric = "gower")

# 8. PAM 군집
sil_width <- c()

# k = 2 ~ 6까지 PAM 클러스터링을 반복하며 silhouette 평균 계산
for (k in 2:6) {
  pam_fit <- pam(gower_dist, diss = TRUE, k = k)
  sil_width[k] <- pam_fit$silinfo$avg.width
}

# 결과 시각화
plot(2:6, sil_width[2:6],
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters k",
     ylab = "Average silhouette width",
     main = "Optimal number of clusters (Silhouette Method)")

pam_res <- pam(gower_dist, k = 3)

# 9. 군집 결과 결합
df_cluster <- cbind(df_sub, cluster = as.factor(pam_res$clustering))

# 🔎 군집별 평균 특성
summary_by_cluster <- df_cluster %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

print(summary_by_cluster)

# 🔎 군집별 성별 비율 확인 (예시)
prop.table(table(df_cluster$cluster, df_cluster$여자), 1)

# 🔍 FAMD 차원별 설명력 확인
fviz_screeplot(famd_outcome, addlabels = TRUE, main = "성과 변수 - FAMD 설명력")
fviz_contrib(famd_outcome, choice = "var", axes = 1, top = 10)

famd_outcome$eig

# 군집별 평균 = 비율로 해석 가능
df_cluster %>%
  group_by(cluster) %>%
  summarise(across(all_of(contract_vars), \(x) mean(as.numeric(x), na.rm = TRUE)))

# 군집별 임금, 근로시간, 보험가입 boxplot
library(ggplot2)

ggplot(df_cluster, aes(x = cluster, y = 월평균임금.로그.)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "군집별 월평균임금(로그)", x = "군집", y = "임금") +
  theme_minimal()

ggplot(df_cluster, aes(x = cluster, y = 주당근로시간)) +
  geom_boxplot(fill = "#FFDD99") +
  labs(title = "군집별 주당근로시간", x = "군집", y = "근로시간") +
  theme_minimal()

ggplot(df_cluster, aes(x = cluster, y = 사회보험가입개수)) +
  geom_boxplot(fill = "#AACCEE") +
  labs(title = "군집별 사회보험가입개수", x = "군집", y = "가입 개수") +
  theme_minimal()

# 군집별 성별 비율 barplot
library(dplyr)

df_cluster %>%
  count(cluster, 여자) %>%
  group_by(cluster) %>%
  mutate(비율 = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = 비율, fill = as.factor(여자))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("lightblue", "pink"), labels = c("남자", "여자")) +
  labs(title = "군집별 성별 비율", y = "비율", fill = "성별") +
  theme_minimal()

# 군집별 비정규직 유형 barplot
library(reshape2)

df_long <- melt(df_cluster[, c("cluster", contract_vars)], id.vars = "cluster")

ggplot(df_long, aes(x = variable, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "군집별 비정규직 유형 비율", y = "비율", x = "비정규직 유형") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------
# 군집별 평균 비율 계산
library(tidyr)
library(ggplot2)

contract_summary <- df_cluster %>%
  group_by(cluster) %>%
  summarise(across(all_of(contract_vars), ~ mean(as.numeric(.), na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(contract_vars), names_to = "비정규직유형", values_to = "비율")

# 시각화: 각 군집별 비정규직 유형 비율 (cluster별 막대 나눔)
ggplot(contract_summary, aes(x = 비정규직유형, y = 비율, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "군집별 비정규직 유형 비율 비교", y = "비율", x = "비정규직 유형", fill = "군집") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. 군집 분포 비율 계산: 비정규직 유형 내에서 cluster 비율
contract_long <- df_cluster %>%
  select(cluster, all_of(contract_vars)) %>%
  pivot_longer(cols = all_of(contract_vars), names_to = "비정규직유형", values_to = "여부") %>%
  filter(여부 == 1) %>%
  group_by(비정규직유형, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(비정규직유형) %>%
  mutate(비율 = n / sum(n))

# 2. 시각화
ggplot(contract_long, aes(x = 비정규직유형, y = 비율, fill = cluster)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "비정규직 유형별 군집 분포", y = "비율", x = "비정규직 유형", fill = "군집") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------
library(reshape2)
library(ggplot2)

# 히트맵용 데이터 준비
heat_data <- df_cluster %>%
  group_by(cluster) %>%
  summarise(across(all_of(contract_vars), ~ mean(as.numeric(.), na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(contract_vars), names_to = "비정규직유형", values_to = "비율")

# 시각화
ggplot(heat_data, aes(x = cluster, y = 비정규직유형, fill = 비율)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "군집별 비정규직 유형 구성 히트맵", x = "군집", y = "비정규직 유형") +
  theme_minimal()

#----------
ggplot(heat_data, aes(x = 비정규직유형, y = 비율, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "군집별 비정규직 유형 비율 비교", y = "비율", x = "비정규직 유형", fill = "군집") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------- 유의성 검정
# 군집별 임금 평균 차이 검정 (ANOVA)
anova_result <- aov(월평균임금.로그. ~ cluster, data = df_cluster)
summary(anova_result)
# 군집별 성별 비율 차이 검정 (카이제곱)
table_gender <- table(df_cluster$cluster, df_cluster$여자)
chisq.test(table_gender)
# 군집별 비정규직 유형별 비율 차이 검정
# 비정규직 변수 목록
contract_vars <- c("기간제", "비기간제_갱신", "비기간제_불안정", 
                   "시간제", "파견", "용역", "도급", "임시직", "일용직")

# 결과 저장용 리스트
chisq_results <- list()

# 각 변수에 대해 카이제곱 검정 수행
for (var in contract_vars) {
  tbl <- table(df_cluster$cluster, df_cluster[[var]])
  test <- chisq.test(tbl)
  
  chisq_results[[var]] <- list(
    변수 = var,
    카이제곱통계량 = test$statistic,
    자유도 = test$parameter,
    p값 = test$p.value
  )
}

# 보기 좋게 데이터프레임으로 정리
library(tibble)
chisq_summary <- tibble(
  변수 = contract_vars,
  카이제곱통계량 = sapply(chisq_results, function(x) round(x$카이제곱통계량, 2)),
  자유도 = sapply(chisq_results, function(x) x$자유도),
  p값 = sapply(chisq_results, function(x) signif(x$p값, 3)),
  유의함 = sapply(chisq_results, function(x) ifelse(x$p값 < 0.05, "✓", ""))
)

print(chisq_summary)
