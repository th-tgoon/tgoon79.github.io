
install.packages("listviewer")
install.packages("pdftools")
install.packages("magick")
install.packages("purrr")

library(listviewer)
library(pdftools)
library(magick)
library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(purrr)

setwd("~/Rstudy")
resume_first_png <- pdf_render_page("rmarkdown_test/resume.pdf", page = 1, dpi = 300, numeric = FALSE)
image_read(resume_first_png)

resume_second_png <- pdf_render_page("rmarkdown_test/resume.pdf", page = 2, dpi = 300, numeric = FALSE)
image_read(resume_second_png)


cv_dat <- pdf_text("rmarkdown_test/resume.pdf")


cv_dat <- paste0(unlist(cv_dat), collapse = "")

cv_split_dat <- cv_dat %>% 
  str_split(pattern="\n") %>% 
  .[[1]]



# 인적사항 -------------------

`인적사항_idx` <- cv_split_dat %>% 
  str_detect("Summary") %>% 
  which(TRUE)

인적사항 <- cv_split_dat[1:(`인적사항_idx`-1)]




# 요약("Summary") -------------------
`요약_idx` <- cv_split_dat %>% 
  str_detect("Work Experience") %>% 
  which(TRUE)

요약 <- cv_split_dat[(`인적사항_idx`+1):(`요약_idx`-1)]

# 직장경력("Work Experience") -------------------
`직장경력_idx` <- cv_split_dat %>% 
  str_detect("Honors & Awards") %>% 
  which(TRUE)

직장경력 <- cv_split_dat[(`요약_idx`+1):(`직장경력_idx`-1)]

# 수상이력 ("Honors & Awards") -------------------
`수상이력_idx` <- cv_split_dat %>% 
  str_detect("Presentation") %>% 
  which(TRUE)

수상이력 <- cv_split_dat[(`직장경력_idx`+1):(`수상이력_idx`-1)]

# 발표("Presentation") -------------------
`발표_idx` <- cv_split_dat %>% 
  str_detect("Writing") %>% 
  which(TRUE)

발표 <- cv_split_dat[(`수상이력_idx`+1):(`발표_idx`-1)]

# 저서("Writing") -------------------
`저서_idx` <- cv_split_dat %>% 
  str_detect("Program Committees") %>% 
  which(TRUE)

저서 <- cv_split_dat[(`발표_idx`+1):(`저서_idx`-1)]

# 심사("Program Committees") -------------------
`심사_idx` <- cv_split_dat %>% 
  str_detect("Education") %>% 
  which(TRUE)

심사 <- cv_split_dat[(`저서_idx`+1):(`심사_idx`-1)]

# 학교("Education") -------------------
`학교_idx` <- cv_split_dat %>% 
  str_detect("Extracurricular") %>% 
  which(TRUE)

학교 <- cv_split_dat[(`심사_idx`+1):(`학교_idx`-1)]

# 특활활동("Extracurricular") -------------------
특활활동 <- cv_split_dat[(`학교_idx`+1):length(cv_split_dat)]

## 이력서 구분

cv_section_list <- list("인적사항" = 인적사항,
                        "요약" = 요약,
                        "직장경력" = 직장경력,
                        "수상이력" = 수상이력,
                        "발표" = 발표,
                        "저서" = 저서,
                        "심사" = 심사,
                        "학교"=학교, 
                        "특활활동"=특활활동)

listviewer::jsonedit(cv_section_list)


## data standardization

인적사항 <- str_trim(인적사항) %>% str_remove_all(pattern="\uf10b|\uf0e0|\uf015|\uf092|\uf08c")

이름 <- 인적사항[1]
직무 <- 인적사항[2]
주소 <- 인적사항[3]

개인정보 <- str_split(인적사항[4], " \\| ") %>% .[[1]]
전화번호 <- str_trim(개인정보[1])
전자우편 <- str_trim(개인정보[2])
홈페이지 <- str_trim(개인정보[3])
GitHub   <- str_trim(개인정보[4])
링크드인 <- str_trim(개인정보[5])

인적사항_df <- tibble(
  "이름" = 이름,
  "직무" = 직무,
  "주소" = 주소,
  "전화번호" = 전화번호,
  "전자우편" = 전자우편,
  "홈페이지" = 홈페이지,
  "Github"   = GitHub,
  "링크드인" = 링크드인
) 

인적사항_df %>% 
  DT::datatable()


## summary  

요약_df <- tibble(
  "요약" = str_c(요약, collapse=" ")
)
요약_df %>% 
  DT::datatable()  


## Career

직장 <- 직장경력[str_detect(직장경력, "S.Korea")]

직장명 <- str_extract(직장, "^(.*?)[\\s+]{2}") %>% 
  str_trim()
근무지 <- str_remove(직장, "^(.*?)[\\s+]{2}") %>% 
  str_trim()

근무연도 <- 직장경력[str_detect(직장경력, "[A-Z]{1}[a-z]{2}.\\s[0-9]{4}")]
역할     <- str_extract(근무연도, "^(.*?)[\\s+]{2}")
근무기간 <- str_remove(근무연도, "^(.*?)[\\s+]{2}") %>% 
  str_trim


직장_df <- tibble(
  "직장명" = 직장명,
  "근무지" = 근무지,
  "역할" = 역할,
  "근무연도" = 근무기간
)

직장_df %>% 
  DT::datatable()



## Award

수상 <- 수상이력[str_detect(수상이력, "^\\s+[0-9]{4}" )]

수상년도 <- map_chr(수상, str_extract, "[0-9]{4}")

수상등수 <- map_chr(수상, str_remove, "[0-9]{4}") %>% 
  map_chr(str_trim) %>% 
  map_chr(str_extract, "^(.*?),")

수상프로그램 <- map_chr(수상, str_remove, "[0-9]{4}") %>% 
  map_chr(str_trim) %>% 
  map_chr(str_remove, "^(.*?),") %>% 
  map_chr(str_extract, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

수상지역 <- map_chr(수상, str_remove, "[0-9]{4}") %>% 
  map_chr(str_trim) %>% 
  map_chr(str_remove, "^(.*?),") %>% 
  map_chr(str_remove, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

수상_df <- tibble(
  "수상년도" = 수상년도,
  "수상등수" = 수상등수,
  "수상프로그램" = 수상프로그램,
  "수상지역" = 수상지역
)

수상_df %>% 
  DT::datatable()

## 발표
발표제목 <- 발표[str_detect(발표, "Korea")]
발표일 <- 발표[str_detect(발표, "[0-9]{4}")]

발표제목 <- map_chr(발표제목, str_extract, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

발표일 <- map_chr(발표일, str_remove, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

발표_df <- tibble(
  "발표제목" = 발표제목,
  "발표일"   = 발표일
)

발표_df %>% 
  DT::datatable()


발표제목 <- 발표[str_detect(발표, "Korea")]
발표일 <- 발표[str_detect(발표, "[0-9]{4}")]

발표제목 <- map_chr(발표제목, str_extract, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

발표일 <- map_chr(발표일, str_remove, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

발표_df <- tibble(
  "발표제목" = 발표제목,
  "발표일"   = 발표일
)

발표_df %>% 
  DT::datatable()


## 저서

저서명 <- str_extract(저서[1], "^(.*?)[\\s+]{2}") %>% 
  str_trim

저자명 <- str_extract(저서[2], "^(.*?)[\\s+]{2}") %>% 
  str_trim

저작연도 <- str_remove(저서[2], "^(.*?)[\\s+]{2}") %>% 
  str_trim

저서_df <- tibble(
  "저서명" = 저서명,
  "저자명" = 저자명,
  "저작연도" = 저작연도
)

저서_df %>% 
  DT::datatable()


## 심사

심사년도 <- map_chr(심사, str_trim) %>% map_chr(., str_extract, "^[0-9]{4}")

심사정보 <- map_chr(심사, str_trim) %>% map_chr(., str_remove, "^[0-9]{4}") %>% 
  str_split(",") %>% 
  map(., str_trim) 

심사역할 <- map_chr(심사정보, 1)
심사프로그램 <- map_chr(심사정보, 2) %>% 
  map_chr(str_extract, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

심사지역 <- map_chr(심사정보, 2) %>% 
  map_chr(str_remove, "^(.*?)[\\s+]{2}") %>% 
  map_chr(str_trim)

심사_df <- tibble(
  "심사년도" = 심사년도,
  "심사역할" = 심사역할,
  "심사프로그램" = 심사프로그램,
  "심사지역" = 심사지역
)

심사_df %>% 
  DT::datatable()

## 학교
학교임시 <- str_split(학교[1], "    ")

학교임시 <- 학교임시[[1]][학교임시[[1]] != ""]

학교명   <- 학교임시[1]
학교위치 <- 학교임시[2]

학교_df <- tibble(
  "학교명" = 학교명,
  "학교위치" = 학교위치
)

학교_df %>% 
  DT::datatable()


## 특별활동

특활 <- 특활활동[str_detect(특활활동, "S.Korea")]

특활명 <- str_extract(특활, "^(.*?)\\s{2}") %>% 
  str_trim
특활지역 <- str_remove(특활, "^(.*?)\\s{2}") %>% 
  str_trim

특활연도 <- 특활활동[str_detect(특활활동, "[A-Z]{1}[a-z]{2}.\\s[0-9]{4}")]

특활역할  <- str_extract(특활연도, "^(.*?)[\\s+]{2}") %>% str_trim
특활기간 <- str_remove(특활연도, "^(.*?)[\\s+]{2}") %>% 
  str_trim

특활_df <- tibble(
  "특활명" = 특활명,
  "특활지역" = 특활지역,
  "특활역할" = 특활역할,
  "특활기간" = 특활기간
)

특활_df %>% 
  DT::datatable()


## 정리
library(dplyr)
library(readr)
인적사항_df %>% write_rds("rmarkdown_test/인적사항_df")
요약_df %>% write_rds("rmarkdown_test/요약_df")
직장_df %>% write_rds("rmarkdown_test/직장_df")
수상_df %>% write_rds("rmarkdown_test/수상수상_df")
발표_df %>% write_rds("rmarkdown_test/발표_df")
저서_df %>% write_rds("rmarkdown_test/저서_df")
심사_df %>% write_rds("rmarkdown_test/심사_df")
학교_df %>% write_rds("rmarkdown_test/학교_df")
특활_df %>% write_rds("rmarkdown_test/특활_df")



## 내용 바꾸기

## 기본
인적사항_tg <- c("Taehyuk Kwon", "Associate Research Fellow / S&T and Industry Innovation policy Researcher based on data science",
              "Suji-ro 487, Suji-gu, Yongin, Gyeongi, Rep. of Korea", "(+82) 10-8864-4561","supworld@hanmail.net",
              "", "th-tgoon", "")


인적사항_df <- rbind(인적사항_df, 인적사항_tg) %>% select(-홈페이지, -링크드인) %>% filter(이름=="Taehyuk Kwon")


## summary
요약_tg <- c("Innovation policy Researcher utilizing data science. 10+ years experienced in Business Development and Project Planning with specialized knowledge of S&T, Health care Industry field")

요약_df <- rbind(요약_df, 요약_tg) %>% slice(-1)

## career
직장_tg1 <- c("Korea Health Industry Development Institute(KHIDI)", "Osong, S.Korea", "Econmic feasibility analysis for public healthcare project, Heathcare technology forecasting, Health industry Policy Planning, S&T policy research and economic evaluation",
            "2015 ~ 2021")
직장_tg2 <- c("Korea Instute of S&T Evaluation and Planning(KISTEP)", "Seoul, S.Korea", "National R&D Project Meta Evaluation, R&D evaluation Index development, Technical evaluation for Preliminary feasibility Study of National R&D project",
            "2014 ~ 2015")
직장_tg3 <- c("Korea Research Instute of Standards and Science(KRISS)", "Daejeon, S.Korea", "Mega R&D Project Planning, Developing Long-mid term R&D strategy & Standard Policy research",
            "2011 ~ 2014")
직장_tg4 <- c("Lotte Insurance Co. Ltd", "Seoul, S.Korea", "Actuarial Science, Data analysis", "2010 ~ 2011")

직장_tg5 <- c("Repulbic of Korea Navy(1st Fleet & Logistic Command)", "Donghae / Jinhae, S.Korea", "Supply officer, logistics controller ", "2005 ~ 2008")

직장_df <- rbind(직장_df, 직장_tg1, 직장_tg2, 직장_tg3, 직장_tg4, 직장_tg5) %>% slice(7:11)


## Education

학교_tg1 <- c("Bachelor of Economics(Applied Statistics)","2004", "Yonsei Univ.", "Seoul, S. Korea","")
학교_tg2 <- c("Master of Science in Applied Statistics","2010", "Yonsei Univ.", "Seoul, S. Korea","The Study about the usefulness of using the chi-square test with continuity correction")
학교_tg3 <- c("Ph D. in Pubic administration(S&T Policy)","2020", "Hanyang Univ.", "Seoul, S. Korea","What makes Korean firms transfer public technology and commercialize well? : An empirical study on public technology licensee firms")

학교_df$학위 <- c("")
학교_df$졸업년도 <- c("")
학교_df$논문명 <- c("")
학교_df <- 학교_df %>% select(학위, 졸업년도, 학교명, 학교위치, 논문명) %>% rbind(학교_df, 학교_tg1,학교_tg2, 학교_tg3) %>% slice(-1)

학교_df <- 학교_df %>% slice(-1) 

## presentation

발표_tg1 <- c("Is it worth protecting the leader's profit to boost innovation in high-risk high-return market?","2020, Spring", "Korea Technology Innovation Society Conference", "")
발표_tg2 <- c("Open Innovation in developmental state: a potential path-dependency of public technology licensee firms","2019 Summer", "Korea Society of Innovation Management & Economics Conference", "Best Paper Award")
발표_tg3 <- c("An exploratory study on the factors affecting the market performance of public technology licensee firms","2018 Fall", "Korea Technology Innovation Society Conference.", "")
발표_tg4 <- c("A study about Technology licensing and Self-research capability on the Performance of Research focused Firms","2018, Summer", "Korea Society of Innovation Management & Economics Conference", "Best Paper Award")

발표_df$발표대회 <- c("")
발표_df$비고 <- c("")
발표_df <- rbind(발표_df, 발표_tg1,발표_tg2, 발표_tg3, 발표_tg4) %>%
  dplyr::slice(3:6)

발표_df



저서_tg1 <- c("Bio healthcare Trend: The era of Convergence", "With 15 other authors", "2017", "ISBN - 9791187897019")
저서_tg2 <- c("Bio healthcare Trend: Blue chip", "With 11 other authors", "2018", "ISBN - 9791187897248")
저서_df$정보 <- c("")
저서_df <- rbind(저서_df, 저서_tg1, 저서_tg2) %>%
  dplyr::slice(-1)

저서_df


논문_df <- bibliography_entries("My research.bib")
names(논문_df)

인적사항_df %>% write_rds("rmarkdown_test/인적사항_df")
요약_df %>% write_rds("rmarkdown_test/요약_df")
직장_df %>% write_rds("rmarkdown_test/직장_df")
발표_df %>% write_rds("rmarkdown_test/발표_df")
저서_df %>% write_rds("rmarkdown_test/저서_df")
논문_df %>% write_rds("rmarkdown_test/논문_df")
학교_df %>% write_rds("rmarkdown_test/학교_df")

resume_df <- tibble(
  "인적사항" = list(인적사항_df),
  "요약" = list(요약_df),
  "학교" = list(학교_df),
  "직장" = list(직장_df),
  "논문" = list(논문_df),
  "발표" = list(발표_df),
  "저서" = list(저서_df)
)

resume_df %>% 
  DT::datatable()  

resume_df

resume_df %>% 
  write_rds("rmarkdown_test/resume_th_df.rds")

resume_df$논문

