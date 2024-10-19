# 加载必要的包
library(stringr)

# 定义提取函数
extract_name <- function(location) {
  # 提取非括号内的文字
  non_bracket_text <- str_extract(location, "^[^（]+")
  
  # 提取第一个括号内仅有2到3个汉字的文本
  bracket_texts <- str_extract_all(location, "（[\\p{Han}]{2,3}）", simplify = TRUE)
  
  # 找到第一个括号内容有2到3汉字的项
  for (text in bracket_texts) {
    # 如果括号内容符合条件，去掉括号
    if (nchar(gsub("（|）", "", text)) >= 2 && nchar(gsub("（|）", "", text)) <= 3) {
      bracket_text <- gsub("）", "", text)
      bracket_text <- gsub("（", ",", bracket_text)
      return(paste(non_bracket_text, bracket_text, sep = ""))
    }
  }
  
  return(non_bracket_text)
}

#############
# 应用提取函数
library(openxlsx)
file_path <- "data/route.xlsx"
sheet_names <- getSheetNames(file_path)

# 整理数据
poem_df <- data.frame(
  name = character(),
  order = numeric(),
  addr = character(),
  ex_addr = character(),
  cm_addr = character()
)

for (i in sheet_names) {
  file = read.xlsx(file_path, sheet = i)
  extract_text <- sapply(file$addr, extract_name)
  
  split_locations <- t(sapply(extract_text, function(x) {
    # 按逗号拆分
    split_str <- strsplit(x, ",")[[1]]
    # 创建第一列
    first <- split_str[1]
    # 创建第二列
    second <- ifelse(length(split_str) > 1, split_str[2], "")
    # 创建第三列
    third <- ifelse(second == "", first, paste0(first, "(", second, ")"))
    
    c(first, second, third)
  }))
  
  # 创建数据框
  .df <- data.frame(
    name = i,
    order = 1:length(split_locations[, 1]),
    addr = split_locations[, 1],
    ex_addr = split_locations[, 2],
    cm_addr = split_locations[, 3],
    stringsAsFactors = FALSE
  )
  
  poem_df = rbind(poem_df, .df)
}
rownames(poem_df) <- 1:dim(poem_df)[1]

# 查看结果
print(poem_df)
