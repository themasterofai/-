import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
import matplotlib.pyplot as plt
import seaborn as sns

# 0. 데이터 로딩 및 기본 전처리
excel_file_path = "klips26p_processed.xlsx" # 이전 단계에서 생성된 파일로 가정
sheet_name_to_load = "klips26p" # 실제 시트 이름으로 변경

try:
    df = pd.read_excel(excel_file_path, engine='openpyxl') # .xlsx 파일 로딩
except FileNotFoundError:
    print(f"오류: {excel_file_path} 파일을 찾을 수 없습니다. 이전 단계에서 파일을 생성했는지, 경로가 정확한지 확인하세요.")
    # ----- 예시 데이터프레임 생성 (실제 데이터 로딩 실패 시 대체) -----
    data_example = {
        'p260314': np.random.choice([1, 2, 3, 4, 5], size=1000),
        'p260501': np.random.choice([1, 2, np.nan], size=1000, p=[0.4, 0.5, 0.1]),
        'p260601': np.random.choice([1, 2, np.nan], size=1000, p=[0.4, 0.5, 0.1]),
        'p260602': np.random.choice([1, 2, 3, np.nan], size=1000, p=[0.3,0.3,0.3,0.1]),
        'p260605': np.random.choice(list(range(1,7)) + [np.nan], size=1000, p=[0.1]*6 + [0.4]),
        'p260315': np.random.choice([1, 2, np.nan], size=1000, p=[0.3, 0.6, 0.1]),
        'p260611': np.random.choice([1, 2, 3, np.nan], size=1000, p=[0.6, 0.1, 0.1, 0.2]),
        'p260612': np.random.choice([1, 2, np.nan], size=1000, p=[0.2, 0.7, 0.1]),
        'p260508': np.random.choice([1, 2, np.nan], size=1000, p=[0.1, 0.8, 0.1]),
        'p260101': np.random.choice([1, 2], size=1000),
        'p260107': np.random.randint(15, 80, size=1000),
        'p265501': np.random.choice([1, 2, 3, 4, 5], size=1000),
        'p260102': np.random.choice([10, 20, 30, 99], size=1000),
        'p260110': np.random.choice(list(range(1,10)), size=1000),
        'p260111': np.random.choice(list(range(1,6)), size=1000),
        'p266101': np.random.choice(list(range(1,6)) + [np.nan], size=1000, p=[0.15]*5 +[0.25]),
        'p266109': np.random.choice([1,2,np.nan], size=1000, p=[0.2,0.6,0.2]),
        'p261642': np.random.uniform(100, 500, size=1000) * 10000, # 원 단위로 변경 후 만원단위로 가정
        'p261006': np.random.uniform(20, 60, size=1000),
        'p261012': np.random.uniform(0, 20, size=1000),
        'p261003': np.random.choice([1,2, np.nan], size=1000, p=[0.7,0.2,0.1]),
        'p261004': np.random.uniform(20,60,size=1000),
        'p261011': np.random.choice([1,2, np.nan], size=1000, p=[0.4,0.5,0.1]),
        'p261019': np.random.choice([1,2, np.nan], size=1000, p=[0.5,0.4,0.1]),
        'p264312': np.random.choice(list(range(1,6))+ [np.nan], size=1000,p=[0.18]*5+[0.1]),
        'p262101': np.random.choice([1,2,np.nan], size=1000, p=[0.7,0.2,0.1]),
        'p262102': np.random.choice([1,2,np.nan], size=1000, p=[0.05,0.85,0.1]),
        'p262103': np.random.choice([1,2,np.nan], size=1000, p=[0.8,0.1,0.1]),
        'p262104': np.random.choice([1,2,np.nan], size=1000, p=[0.6,0.3,0.1]),
        'p262105': np.random.choice([1,2,np.nan], size=1000, p=[0.7,0.2,0.1]),
        'p264101': np.random.choice([1, 2, np.nan], size=1000, p=[0.6, 0.3, 0.1]),
        'p264104': np.random.choice([1, 2, np.nan], size=1000, p=[0.7, 0.2, 0.1]),
        'p264105': np.random.choice([1, 2, np.nan], size=1000, p=[0.5, 0.4, 0.1]),
        'p264106': np.random.choice([1, 2, np.nan], size=1000, p=[0.8, 0.1, 0.1]),
        'p264110': np.random.choice([1, 2, np.nan], size=1000, p=[0.7, 0.2, 0.1]),
        'p264113': np.random.choice([1, 2, np.nan], size=1000, p=[0.3, 0.6, 0.1]),
        'p264123': np.random.choice([1, 2, np.nan], size=1000, p=[0.4, 0.5, 0.1]),
        'p264321': np.random.choice(list(range(1,6))+ [np.nan], size=1000,p=[0.18]*5+[0.1]),
        'p264501': np.random.choice([1,2,np.nan], size=1000, p=[0.3,0.6,0.1])
    }
    df = pd.DataFrame(data_example)
    print("경고: 실제 엑셀 파일을 로드하지 못했습니다. 예시 데이터프레임으로 코드를 실행합니다.")
    # ----- 예시 데이터프레임 생성 종료 -----

# 분석 대상 선정: 임금근로자
df = df[df['p260314'].isin([1, 2, 3])].copy()

# 결측치 처리 함수
def convert_klips_missing(value):
    if pd.isna(value) or value in [-1, -2, -9, -99]: # -99도 결측으로 처리
        return np.nan
    return value

# 전처리 및 파생변수 생성을 위한 원본 변수 목록 (문서 및 이전 대화 기반)
raw_vars_needed = [
    'p260501', 'p260601', 'p260602', 'p260605', 'p260315', 'p260611', 'p260612', 'p260508', # 비정규직 정의용
    'p260314', # 종사상 지위 (비정규직 정의 및 임금근로자 필터링용)
    # 'p260317', # 주관적 비정규직 (문서에서 최종적으로 사용하기로 한 변수 목록에는 없음)
    'p260101', 'p260107', 'p265501', 'p260102', 'p260110', 'p260111', # 집단특성: 인구학, 교육
    'p266101', 'p266109', # 집단특성: 건강
    'p261642', # 성과: 임금
    'p261006', 'p261012', 'p261003', 'p261004', 'p261011', 'p261019', # 성과: 근로시간
    'p264312', # 성과: 고용안정성 인식
    'p262101', 'p262102', 'p262103', 'p262104', 'p262105', # 성과: 사회보험
    # 복리후생 (문서에서 선정한 7개 항목의 원천 변수명)
    'p264101', 'p264104', 'p264105', 'p264106', 'p264110', 'p264113', 'p264123',
    'p264321', # 성과: 직무만족도
    'p264501'  # 성과: 교육훈련
]

for var in raw_vars_needed:
    if var in df.columns:
        df[var] = df[var].apply(convert_klips_missing)
    # else:
    #     print(f"주의: 원본 변수 '{var}'가 데이터프레임에 없습니다. 확인이 필요합니다.")

# --- 파생변수 생성 (이전 코드의 생성 로직 활용) ---
# 1. 비정규직 정의 변수
df['fixed_term'] = np.where(df['p260501'] == 1, 1, 0)
df['nonfixed_a'] = np.where((df['p260501'] == 2) & (df['p260601'] == 1) & (df['p260602'] == 2), 1, 0)
df['nonfixed_b'] = np.where((df['p260501'] == 2) & (df['p260601'] == 2) & (df['p260605'].isin([1,2,3,4,5,6])), 1, 0)
df['part_time'] = np.where(df['p260315'] == 1, 1, 0)
df['dispatched_worker'] = np.where(df['p260611'] == 2, 1, 0)
df['subcontracted_worker'] = np.where(df['p260611'] == 3, 1, 0)
df['contract_worker_special'] = np.where(df['p260612'] == 1, 1, 0)
df['temporary_worker_status'] = np.where(df['p260314'] == 2, 1, 0)
df['daily_worker_status'] = np.where(df['p260314'] == 3, 1, 0)

# 노사정위 기준 비정규직 (foe1)
df['daily_short_term'] = np.where(df['p260508'] == 1, 1, 0) # 일일(단기)근로자
# df['foe1'] = np.where(
#     (df['fixed_term'] == 1) | (df['nonfixed_a'] == 1) | (df['nonfixed_b'] == 1) | (df['part_time'] == 1) |
#     (df['dispatched_worker'] == 1) | (df['subcontracted_worker'] == 1) |
#     (df['contract_worker_special'] == 1) | (df['daily_short_term'] == 1), 1, 0
# )
# 종사상 지위 기준 비정규직 (foe2)
# df['foe2'] = np.where((df['temporary_worker_status'] == 1) | (df['daily_worker_status'] == 1), 1, 0)

# 2. 집단 특성 변수
df['female'] = np.where(df['p260101'] == 2, 1, 0)
bins_age = [14, 39, 64, np.inf]
labels_age = [1, 2, 3]
df['age_group'] = pd.cut(df['p260107'], bins=bins_age, labels=labels_age, right=True).astype('category') # category 타입으로 변경

df['edu_level_category'] = np.nan
df.loc[df['p260110'].isin([1,2,3,4]), 'edu_level_category'] = 1
df.loc[(df['p260110'] == 5) & (df['p260111'] != 1), 'edu_level_category'] = 1
df.loc[(df['p260110'] == 5) & (df['p260111'] == 1), 'edu_level_category'] = 2
df.loc[(df['p260110'] == 6) & (df['p260111'] != 1), 'edu_level_category'] = 2
df.loc[(df['p260110'] == 6) & (df['p260111'] == 1), 'edu_level_category'] = 3
df.loc[(df['p260110'] == 7) & (df['p260111'] != 1), 'edu_level_category'] = 2
df.loc[(df['p260110'] == 7) & (df['p260111'] == 1), 'edu_level_category'] = 4
df.loc[(df['p260110'].isin([8,9])) & (df['p260111'] != 1), 'edu_level_category'] = 4
df.loc[(df['p260110'].isin([8,9])) & (df['p260111'] == 1), 'edu_level_category'] = 5
df['edu_level_category'] = df['edu_level_category'].astype('category')

df['health_subjective'] = df['p266101'] # 순서형 특성 있는 범주형
df['health_limitation'] = np.where(df['p266109'] == 1, 1, 0)

# 3. 노동시장 성과 변수
df['log_wage'] = np.log(df['p261642'].replace(0, np.nan).astype(float))
df['total_work_hours'] = np.nan
df.loc[(df['p261003'] == 1) & (df['p261011'] == 1) & pd.notna(df['p261006']), 'total_work_hours'] = df['p261006']
df.loc[(df['p261003'] == 1) & (df['p261011'] == 2) & (df['p261019'] == 1) & pd.notna(df['p261006']) & pd.notna(df['p261012']), 'total_work_hours'] = df['p261006'] + df['p261012']
df.loc[(df['p261003'] == 1) & (df['p261011'] == 2) & (df['p261019'] == 2) & pd.notna(df['p261006']) & pd.notna(df['p261012']), 'total_work_hours'] = df['p261006'] + (df['p261012'] / 4.3)
df.loc[(df['p261003'] == 2) & pd.notna(df['p261004']), 'total_work_hours'] = df['p261004']

df['job_security_satisfaction'] = df['p264312'] # 순서형 특성 있는 범주형

df_temp_social_insurance = df[df['p262102'] != 1].copy()
df['national_pension'] = np.nan
if not df_temp_social_insurance.empty and 'p262101' in df_temp_social_insurance.columns:
    df.loc[df_temp_social_insurance.index, 'national_pension'] = np.where(df_temp_social_insurance['p262101'] == 1, 1, 0)
del df_temp_social_insurance

df['health_insurance'] = np.where(df['p262103'] == 1, 1, 0)
df['employment_insurance'] = np.where(df['p262104'] == 1, 1, 0)
df['accident_insurance'] = np.where(df['p262105'] == 1, 1, 0)

# 복리후생 수혜 개수 (문서에서 선정한 7개 항목)
key_benefits_vars_doc = {
    'benefit_severance': 'p264101',
    'benefit_paidleave': 'p264104',
    'benefit_bonus': 'p264105',
    'benefit_meal': 'p264106',
    'benefit_healthcheck': 'p264110',
    'benefit_childleave': 'p264113',
    'benefit_training_support': 'p264123'
}
temp_benefits_cols = []
for new_var, klips_var_root in key_benefits_vars_doc.items():
    if klips_var_root in df.columns:
        # 실제 KLIPS 데이터에서는 '본인 수혜 여부'를 나타내는 변수와 코드값을 정확히 확인해야 함
        # 여기서는 klips_var_root 자체가 수혜 여부(1=예, 2=아니오 등)를 나타낸다고 가정
        df[new_var] = np.where(df[klips_var_root] == 1, 1, 0)
        df.loc[df[klips_var_root].isna(), new_var] = np.nan # 원본이 NA면 결과도 NA
        temp_benefits_cols.append(new_var)

if temp_benefits_cols:
    df['num_key_benefits_received'] = df[temp_benefits_cols].sum(axis=1, skipna=False)
else:
    df['num_key_benefits_received'] = np.nan


df['job_satisfaction_overall'] = df['p264321'] # 순서형 특성 있는 범주형
df['training_experience'] = np.where(df['p264501'] == 1, 1, 0)
# ---------------------------------------------------------------

# 1. 랜덤 포레스트 분석을 위한 데이터 준비

target_variable = 'daily_worker_status' # 예시 타겟 변수 (다른 비정규직 정의로 변경 가능)
# fixed_term (기간제 근로자) 
# nonfixed_a (비기간제 한시적 - 계약갱신형) 
# nonfixed_b (비기간제 한시적 - 일시적 사유형) 
# part_time (시간제 근로자) 
# dispatched_worker (파견 근로자) 
# subcontracted_worker (용역 노동자) 
# contract_worker_special (도급 노동자) 
# temporary_worker_status (임시직 근로자 - 종사상 지위) 
# daily_worker_status (일용직 근로자 - 종사상 지위)

# 설명 변수(X) 목록: 연속형과 범주형 구분
numerical_features = [
    'p260107', # 만나이 원본 (age_group 대신 원본 사용 가능)
    'log_wage',
    'total_work_hours',
    'num_key_benefits_received'
]
categorical_features = [
    'female',
    'age_group', # 범주화된 연령대
    'edu_level_category',
    'health_subjective', # 순서형이지만 범주형으로 처리
    'health_limitation',
    'job_security_satisfaction', # 순서형이지만 범주형으로 처리
    'national_pension',
    'health_insurance',
    'employment_insurance',
    'accident_insurance',
    'job_satisfaction_overall', # 순서형이지만 범주형으로 처리
    'training_experience'
    # 'married_status_group', 'is_household_head' 등 필요시 추가
]

# 사용 가능한 feature만 필터링
numerical_features = [f for f in numerical_features if f in df.columns]
categorical_features = [f for f in categorical_features if f in df.columns]
feature_variables = numerical_features + categorical_features


df_analysis = df[[target_variable] + feature_variables].copy()
df_analysis.dropna(subset=[target_variable], inplace=True)

if df_analysis.empty or df_analysis[target_variable].nunique() < 2 :
    print(f"분석할 데이터가 부족하거나 타겟 변수 '{target_variable}'의 유니크한 값이 2개 미만입니다.")
    exit()

X = df_analysis[feature_variables]
y = df_analysis[target_variable]

# 2. 전처리 파이프라인 구성
numerical_transformer = Pipeline(steps=[
    ('imputer', SimpleImputer(strategy='mean')) # 연속형 변수 결측치: 평균값으로 대체
])

categorical_transformer = Pipeline(steps=[
    ('imputer', SimpleImputer(strategy='most_frequent')), # 범주형 변수 결측치: 최빈값으로 대체
    ('onehot', OneHotEncoder(handle_unknown='ignore', drop='first', sparse_output=False)) # 원-핫 인코딩
])

preprocessor = ColumnTransformer(
    transformers=[
        ('num', numerical_transformer, numerical_features),
        ('cat', categorical_transformer, categorical_features)
    ],
    remainder='passthrough'
)

# 3. 랜덤 포레스트 모델 학습
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42, stratify=y)

rf_model_pipeline = Pipeline(steps=[('preprocessor', preprocessor),
                                    ('classifier', RandomForestClassifier(n_estimators=100,
                                                                          random_state=42,
                                                                          class_weight='balanced'))])
try:
    rf_model_pipeline.fit(X_train, y_train)
except Exception as e:
    print(f"모델 학습 중 오류: {e}")
    print("데이터 타입을 확인하세요. 예를 들어, age_group이나 edu_level_category가 category 타입으로 되어 있는지 확인하고, 아니라면 .astype('category')로 변환하거나, OneHotEncoder가 처리할 수 있도록 문자열이나 숫자로 되어 있어야 합니다.")
    print("X_train.dtypes:\n", X_train.dtypes)
    exit()


# 4. 변수 중요도 추출 및 시각화
# preprocessor의 'cat' 트랜스포머에서 OneHotEncoder 단계에 접근
onehot_cols = rf_model_pipeline.named_steps['preprocessor'].named_transformers_['cat'].named_steps['onehot'].get_feature_names_out(categorical_features)
all_feature_names_transformed = numerical_features + list(onehot_cols)

importances = rf_model_pipeline.named_steps['classifier'].feature_importances_

feature_importance_df = pd.DataFrame({'feature': all_feature_names_transformed, 'importance': importances})
feature_importance_df = feature_importance_df.sort_values(by='importance', ascending=False)

print(f"\n--- '{target_variable}' 정의에 대한 변수 중요도 ---")
print(feature_importance_df.head(15))

plt.figure(figsize=(12, 10))
sns.barplot(x='importance', y='feature', data=feature_importance_df.head(15), palette="viridis")
plt.title(f"'{target_variable}' 예측을 위한 주요 변수 중요도 (Top 15)")
plt.xlabel("Importance")
plt.ylabel("Feature")
plt.tight_layout()
plt.savefig(f"feature_importance_{target_variable}.png")
print(f"'{target_variable}' 변수 중요도 그래프가 feature_importance_{target_variable}.png 로 저장되었습니다.")
# plt.show() # 로컬 환경에서 그래프 보기

# 모델 평가 (선택 사항)
# from sklearn.metrics import classification_report, accuracy_score
# y_pred = rf_model_pipeline.predict(X_test)
# print(f"\n--- '{target_variable}' 모델 평가 (Test Set) ---")
# print(f"Accuracy: {accuracy_score(y_test, y_pred)}")
# print(classification_report(y_test, y_pred))

print("\nPython 랜덤 포레스트 분석 코드 실행 완료 (엑셀 파일 가정, 변수 유형 구분 반영).")
