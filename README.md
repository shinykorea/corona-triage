# corona-triage: Corona triage app

## 목표 

설문앱으로 코로나환자 중증도를 분류, 의료자원 고려한 치료전략에 도움을 준다.
- 설문 일단 생략, 엑셀데이터 만들어졌다고 가정.


## 설문(데이터) 에 들어갈 내용 

0. 기본정보: 시도, 시군구, 증상발생/확진일 (YYYYMMDD)

1. 이름 

2. 성별: 남/여 

3. 연령군: 0-9/10-19/20-29/30-39/40-49/50-59/60-69/70-79/80세 이상

4. 기저질환: 고혈압, 당뇨, 기타 심뇌혈관질환, 만성폐쇄성폐질환(COPD), 천식, 암, 만성콩팥질환, 장기이식, 만성간질환, 면역억제제 복용

5. 증상: 측정된 체온/호흡 수(RR)/산소포화도(SpO2)/수축기혈압, 주관적 숨찬 증상 유무


## 중증도 점수 계산: 총 20점

- 연령: 60-69세 1점, 70-79세 2점, 80세 이상 3점

- 기저질환: 하나라도 있으면 3점

- 체온: 35.1-36.0℃ 1점, 37.5-38.0℃ 1점, 38.1-39.0℃ 2점, 39.1℃ 이상 3점

- 호흡 수: 9-11회 1점, 8회 이하 3점 

- 산소포화도: 94-95% 1점, 92-93% 2점, 91% 이하 3점

- 수축기혈압: 101-110mmHg 1점, 91-100mmHg 2점, 90mmHg 이하 3점

- 주관적 숨찬 증상 있으면 2점 


## 필요기능 

1. **로그인**: 관계자만 볼 수 있도록, [shinymanager](https://blog.zarathu.com/posts/2019-08-25-shinymanager/) 추천

2. 환자: **일단 생략** 
- 설문 마치면 중증도 볼 수 있음.
- 매일 설문 수행: **매일 알림** 받을 수 있어야 함. 두 번째 설문부터는 기본정보/나이/성별 등 변하지 않는 것은 빼고 간단한 버전으로.

3. 관계자 
- 각 환자 선택 후, 총점/측정값 변화를 볼 수 있는 **시계열 그래프**
- **데이터 확인** 기능: 필터링, 정렬 


## 프로세스

- 초기 실행 창 (로그인 이전)

![image](https://user-images.githubusercontent.com/6457691/75650107-59d2e800-5c98-11ea-9cb8-286a6bb67938.png)


- 로그인 이후 

![image](https://user-images.githubusercontent.com/6457691/75650178-9272c180-5c98-11ea-8201-5617be6607c3.png)

- 기본 red color 에서 [colorna-sickbed](https://github.com/shinykorea/corona-sickbed) 와 유사하게 purple 계열로 색 교체


- [Example.xlsx](https://github.com/shinykorea/corona-triage/blob/master/Example.xlsx) 업로드 이후

![image](https://user-images.githubusercontent.com/6457691/75650595-b256b500-5c99-11ea-9ae9-4a2b01e71ad4.png)

### Example.xlsx 형태 및 예시

|City|Town|Place|Occurence|Confirm|Name|Sex|Age|Disease|Temperature|BreathCount|Oxygen|BloodPressure|Breath|Date|
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
|Char|Char|Char|Date|Date|Char|Char|Numeric|Boolean|Numeric|Numeric|Numeric|Numeric|Numeric|Date|
|경기도|수원시|경기인재개발원|2020-03-01|2020-03-01|김|남|67|TRUE|38.1|9|98|119|5|2020-03-01|
|경기도|수원시|경기인재개발원|2020-03-01|2020-03-01|김|남|67|TRUE|36.4|8|97|98|6|2020-02-29|
|경기도|수원시|경기인재개발원|2020-03-01|2020-03-01|김|남|67|TRUE|38.8|8|92|97|7|2020-02-28|
|경기도|수원시|경기인재개발원|2020-03-01|2020-03-01|김|남|67|TRUE|38.9|12|92|108|8|2020-02-27|
|경기도|수원시|경기인재개발원|2020-03-01|2020-03-01|김|남|67|TRUE|35.9|8|98|97|95|2020-02-26|

- [파일](https://github.com/shinykorea/corona-triage/blob/master/Example.xlsx) 참조



- 임의 열 선택으로 

![image](https://user-images.githubusercontent.com/6457691/75650651-e92ccb00-5c99-11ea-9cd6-a861477fe944.png)

- 해당 환자의 데이터를 테이블, 그래프 형태로 출력

![image](https://user-images.githubusercontent.com/6457691/75650684-02357c00-5c9a-11ea-9cb3-e6af8fa11bb4.png)


## 웹 	
- http://app.zarathu.com/corona/corona-triage/


## 약자

A 나이
D 기저질병
T 체온
CF 심폐기능
	HB 호흡곤란
		DHB 심한 호흡곤란
		CP 가슴통증
		F 실신
		C 고열-몸살-기침
		BL 파란입술
		HHB 호흡곤란심화
	O 산호포화도
	BC 호흡수
	P 맥박
CO 의식
M 심리상태
	PO 두근거림
	PE 발한
	TR 몸떨림
	CH 질식
	CC 가슴불편
	AC 복부불편
	W 어지러움
	SA 감각이상
	FE 두려움
