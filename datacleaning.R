```{r}
# 安装和加载必要的R包
install.packages(c("readr", "dplyr"))
library(readr)
library(dplyr)
```
```{r}
# 1. 读取文件为一列
data <- readLines("IMPC_parameter_description.txt")

# 3. 去除掉前两行（因为它们是列标题和其他不需要的内容）
data_clean <- data[-c(1, 2)]

# 4. 检查并去除任何空行
data_clean <- data_clean[data_clean != ""]

# 5. 用正则表达式分割每一行的内容
# 对于每一行，使用正则表达式将 "line_number" 与后面的字段分开
data_split <- strsplit(data_clean, '" "', perl = TRUE)

# 6. 对拆分后的数据进行进一步处理：去掉引号，并拆分逗号分隔的字段
data_split <- lapply(data_split, function(x) {
  # 移除前后的引号
  x <- gsub('"', '', x)
  # 按逗号分割字段
  strsplit(x, ",\\s*")
})
result_lapply <- lapply(data_split, function(x) {
  # 将 x 扁平化成一个向量
  unlist(x)
})
# 确保每个字符向量都转置成一行
result_as_df <- lapply(result_lapply, function(x) {
  data.frame(t(x))  # 转置每个向量，使其变成一行
})

# 使用 bind_rows 合并，按行绑定
data_split_df <- bind_rows(result_as_df)

data_split_df <- apply(data_split_df, 2, function(x) trimws(x))  # 去除两端空格

# 9. 将数据转换为数据框并为每一列命名
final_data <- as.data.frame(data_split_df, stringsAsFactors = FALSE)

# 10. 给数据框命名
colnames(final_data) <- c("line_number", "impcParameterOrigId", "name", "description", "parameterId")

# 11. 查看结果
# 查看 final_data 中的第1000到2000行
# 显示查看提取的数据

# 假设 final_data 是你的数据框
# 查看 'name' 列为空值的行
empty_rows <- final_data[is.na(final_data$parameterId), ]

# 查看第六列有数据的行
non_empty_rows <- final_data[!is.na(final_data[[6]]), ]
final_data <- final_data[!(final_data$parameterId %in% empty_rows$parameterId) & !(final_data[[6]] %in% non_empty_rows[[6]]), ]

# 步骤 3: 将 empty_rows 和 non_empty_rows 存储到新的 CSV 文件
all_abnormal_data <- rbind(empty_rows, non_empty_rows)
write.csv(all_abnormal_data, "final_data_2.csv", row.names = FALSE)

# 步骤 4: 查看删除后的 final_data
read.csv("final_data_2.csv")
final_data <- final_data[, -((ncol(final_data)-2):ncol(final_data))]
head(final_data)
```
```{r}
# 1. 读取 Modified final_data_2.csv 并转化为 data.frame
modified_data <- read.csv("Modified final_data_2.csv")  # 加载 CSV 文件为 data.frame

# 2. 查看 modified_data 的类型
class(modified_data)  # 应该输出 "data.frame"

head(modified_data)
head(final_data)
write.csv(final_data, "final_data.csv", row.names = FALSE)
```
```{r}
# 读取 final_data.csv 并将其转换为 data.frame，保存到 final_data 变量
final_data <- read.csv("final_data.csv")
tail(final_data)

```
library(tools)

# 目标文件夹路径，假设当前文件夹有一个子文件夹叫 '10'
folder_path <- "10"  # 目标子文件夹
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# 要检查的参数列表
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# 用于保存缺少参数的文件数据框
missing_parameters <- data.frame(
  file_name = character(),  # CSV 文件名
  missing_parameters = character(),  # 缺少的参数
  stringsAsFactors = FALSE
)

# 遍历文件夹中的每个 CSV 文件
for (csv_file in csv_files) {
  # 读取当前 CSV 文件，假设第一列是 A 列
  data <- read.csv(csv_file, header = FALSE)
  
  # 提取文件名（不包括路径）
  file_name_value <- basename(csv_file)
  
  # 获取文件中的 A 列内容
  a_column_values <- data[, 1]
  
  # 查找缺少的参数
  missing_keywords <- setdiff(keywords, a_column_values)
  
  if (length(missing_keywords) > 0) {
    # 如果有缺少的参数，记录该文件和缺少的参数
    missing_parameters <- rbind(missing_parameters, data.frame(
      file_name = file_name_value,
      missing_parameters = paste(missing_keywords, collapse = ", "),  # 缺少的参数以逗号分隔
      stringsAsFactors = FALSE
    ))
  }
}

# 如果有缺少的参数，保存到新的 CSV 文件
if (nrow(missing_parameters) > 0) {
  write.csv(missing_parameters, "missing_parameters.csv", row.names = FALSE)
} else {
  print("所有文件都包含所有参数。")
}

# 打印缺少的参数（可选）
print(missing_parameters)
```{r}
library(tools)
library(stringr)  # 用于处理字符串（如标题格式）


# 获取文件夹中所有的 CSV 文件
csv_files <- list.files(path = "10", pattern = "*.csv", full.names = TRUE)

# final_data 假设已经存在并且包含 parameterId 和 parameter_name 列
# final_data <- read.csv("final_data.csv")  # 如果 final_data 已经在内存中，就不需要这行代码

# 设定 keywords 列表
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# 用于保存异常数据的空数据框
exception_data <- data.frame(
  file_name = character(),  # 文件名
  variable_name = character(),  # 变量名
  abnormal_value = character(),  # 异常数据值
  stringsAsFactors = FALSE
)

# 遍历文件夹中的每个 CSV 文件
for (csv_file in csv_files) {
  # 读取当前 CSV 文件
  data <- read.csv(csv_file, header = FALSE)
  
  # 提取文件名（不包括路径）
  file_name_value <- basename(csv_file)
  
  # 存储当前文件的关键词值
  file_parameters <- list()
  
  # 遍历每个关键词，找到对应的 B 列值并将其存储到参数列表中
  for (keyword in keywords) {
    # 在 A 列中查找关键词，找到对应的 B 列值
    matching_value <- data[data[, 1] == keyword, 2]
    
    # 如果找到对应的值，将其存储到 file_parameters 中
    if (length(matching_value) > 0) {
      file_parameters[[keyword]] <- matching_value
    }
    
    # 处理异常数据的情况（例如匹配值为空或不符合预期格式）
    if (length(matching_value) == 0 || any(is.na(matching_value))) {
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value,
        variable_name = keyword,
        abnormal_value = "Missing or NA",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # 清理并检查每个关键词的数据
  if ("analysis_id" %in% names(file_parameters)) {
    analysis_id <- file_parameters[["analysis_id"]]
    if (nchar(analysis_id) != 15) {
      # 如果 analysis_id 长度不等于 15，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "analysis_id",
        abnormal_value = analysis_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["analysis_id"]] <- NA  # 设置为 NA
    }
  }
  
  if ("gene_accession_id" %in% names(file_parameters)) {
    gene_accession_id <- file_parameters[["gene_accession_id"]]
    if (nchar(gene_accession_id) < 9 | nchar(gene_accession_id) > 11) {
      # 如果 gene_accession_id 长度不在 9 到 11 之间，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "gene_accession_id",
        abnormal_value = gene_accession_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["gene_accession_id"]] <- NA  # 设置为 NA
    }
  }
  
  if ("gene_symbol" %in% names(file_parameters)) {
    gene_symbol <- file_parameters[["gene_symbol"]]
    if (nchar(gene_symbol) < 1 | nchar(gene_symbol) > 13) {
      # 如果 gene_symbol 长度不在 1 到 13 之间，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "gene_symbol",
        abnormal_value = gene_symbol,
        stringsAsFactors = FALSE
      ))
      file_parameters[["gene_symbol"]] <- NA  # 设置为 NA
    } else {
      # 如果 gene_symbol 合法，转换为标题格式
      file_parameters[["gene_symbol"]] <- str_to_title(gene_symbol)
    }
  }
  
  if ("mouse_strain" %in% names(file_parameters)) {
    mouse_strain <- file_parameters[["mouse_strain"]]
    valid_strains <- c("C57BL", "B6J", "C3H", "129SV")
    if (!(mouse_strain %in% valid_strains)) {
      # 如果 mouse_strain 不在有效的品系列表中，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "mouse_strain",
        abnormal_value = mouse_strain,
        stringsAsFactors = FALSE
      ))
      file_parameters[["mouse_strain"]] <- NA  # 设置为 NA
    }
  }
  
  if ("mouse_life_stage" %in% names(file_parameters)) {
    mouse_life_stage <- file_parameters[["mouse_life_stage"]]
    valid_stages <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")
    if (!(mouse_life_stage %in% valid_stages)) {
      # 如果 mouse_life_stage 不在有效的阶段列表中，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "mouse_life_stage",
        abnormal_value = mouse_life_stage,
        stringsAsFactors = FALSE
      ))
      file_parameters[["mouse_life_stage"]] <- NA  # 设置为 NA
    }
  }
  
  if ("parameter_id" %in% names(file_parameters)) {
    parameter_id <- file_parameters[["parameter_id"]]
    if (!(parameter_id %in% final_data$parameterId)) {
      # 如果 parameter_id 不在 final_data 中，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "parameter_id",
        abnormal_value = parameter_id,
        stringsAsFactors = FALSE
      ))
      file_parameters[["parameter_id"]] <- NA  # 设置为 NA
    } else {
      # 如果 parameter_id 匹配，进一步检查 parameter_name 是否一致
      matching_row <- final_data[final_data$parameterId == parameter_id, ]
          final_parameter_name <- NA  # 初始化 final_parameter_name

# 使用循环检查每个值，直到找到有效数据
for (name_value in matching_row$name) {
  if (!is.na(name_value)) {
    final_parameter_name <- name_value
    break  # 找到第一个非 NA 值后跳出循环
  }
}
      if (final_parameter_name != file_parameters[["parameter_name"]]) {
        # 如果不一致，记录异常数据
        exception_data <- rbind(exception_data, data.frame(
          file_name = file_name_value, 
          variable_name = "parameter_name",
          abnormal_value = file_parameters[["parameter_name"]],
          stringsAsFactors = FALSE
        ))
        file_parameters[["parameter_name"]] <- NA  # 设置为 NA
      }
    }
  }
  
  if ("pvalue" %in% names(file_parameters)) {
    pvalue <- file_parameters[["pvalue"]]
    if (pvalue < 0 | pvalue > 1) {
      # 如果 pvalue 不在 0 到 1 之间，记录异常数据
      exception_data <- rbind(exception_data, data.frame(
        file_name = file_name_value, 
        variable_name = "pvalue",
        abnormal_value = pvalue,
        stringsAsFactors = FALSE
      ))
      file_parameters[["pvalue"]] <- NA  # 设置为 NA
    }
  }
}

# 最后保存异常数据到 CSV 文件
write.csv(exception_data, "exception_data.csv", row.names = FALSE)

# 打印异常数据（可选）
print(exception_data)

```

# 获取 data/10 文件夹中的所有 CSV 文件
csv_files <- list.files(path = "data/10", pattern = "*.csv", full.names = TRUE)

# 创建一个空的数据框来存储所有提取的数据
combined_data <- data.frame()

# 定义要查找的关键词（对应 A 列）
keywords <- c("gene_accession_id", "gene_symbol", "mouse_strain", "mouse_life_stage", 
              "parameter_id", "pvalue", "parameter_name", "analysis_id")

# 用于遍历每个 CSV 文件并提取数据
for (file in csv_files) {
  
  # 读取 CSV 文件
  sheet_data <- read.csv(file, header = FALSE)  # 确保没有列名，因为数据是固定行的
  
  # 创建一个空的数据框来存储当前文件提取的数据
  extracted_data <- data.frame(matrix(NA, ncol = length(keywords), nrow = 1))
  colnames(extracted_data) <- keywords  # 设置列名为关键词

  # 遍历每个关键词，在 A 列查找对应的 B 列值
  for (keyword in keywords) {
    
    # 查找关键词所在的行
    matching_row <- which(grepl(keyword, sheet_data$V1, ignore.case = TRUE))  # V1 是 A 列
    
    # 如果找到了匹配的行，提取对应的 B 列值
    if (length(matching_row) > 0) {
      b_value <- sheet_data[matching_row, 2]  # 提取对应的 B 列数据
      extracted_data[[keyword]] <- b_value  # 将 B 列的值放入相应的列
    } else {
      # 如果没有找到该行的值，可以选择不做任何处理，或者可以设置为空字符串（""）
      extracted_data[[keyword]] <- ""  # 设置为空字符串而不是 NA
    }
  }

  # 将当前文件提取的数据添加到总的数据框中
  combined_data <- bind_rows(combined_data, extracted_data)
}

# 为数据框添加序号列
combined_data$Index <- 1:nrow(combined_data)


# 写入合并后的数据到 CSV 文件
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


```
```{r}
data<-read.csv('exception_data.csv')
# 按照 'variable' 列的值对数据进行排序
data_sorted <- data[order(data$variable), ]
head(data_sorted)
# 提取 variable_name 为 "mouse_life_stage" 的数据
mouse_life_stage_data <- subset(data, variable_name == "mouse_life_stage")

# 查看提取后的数据
print(mouse_life_stage_data)

```
```{r}
# 定义目标的 abnormal_value 列表
valid_abnormal_values <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")

# 将 abnormal_value 列转为小写并与目标值的小写进行匹配，避免大小写问题
mouse_life_stage_data$abnormal_value_lower <- tolower(mouse_life_stage_data$abnormal_value)

# 将目标值转为小写
valid_abnormal_values_lower <- tolower(valid_abnormal_values)

# 筛选出匹配的行
matched_data <- mouse_life_stage_data[mouse_life_stage_data$abnormal_value_lower %in% valid_abnormal_values_lower, ]

# 筛选出没有匹配的行
unmatched_data <- mouse_life_stage_data[!mouse_life_stage_data$abnormal_value_lower %in% valid_abnormal_values_lower, ]

# 查看匹配的数据
head(matched_data)

# 查看未匹配的数据
head(unmatched_data)
```
```{r}
# 读取 combined_data.csv 文件
combined_data <- read.csv('combined_data.csv')

# 查看原始数据，确认结构
head(combined_data)

# 读取 mouse_life_stage_data 数据框
# 这里假设 mouse_life_stage_data 已经被提前读取，如果没有，请先读取该数据
# mouse_life_stage_data <- read.csv('path_to_mouse_life_stage_data.csv')

# 确保 mouse_life_stage_data 和 combined_data 的 file_name 和 analysis_id 列存在
if ("file_name" %in% colnames(mouse_life_stage_data) && "analysis_id" %in% colnames(combined_data)) {

  # 创建一个包含目标 mouse_life_stage 的标准值列表
  valid_values <- c("E12.5", "E15.5", "E18.5", "E9.5", "Early adult", "Late adult", "Middle aged adult")
  
  # 去除 mouse_life_stage_data 中 file_name 列中的 .csv 后缀，并转化为小写处理
  mouse_life_stage_data$file_name_clean <- sub("\\.csv$", "", mouse_life_stage_data$file_name)

  # 对于 combined_data 中的 analysis_id 同样去除可能的 .csv 后缀，避免匹配时出错
  combined_data$analysis_id_clean <- sub("\\.csv$", "", combined_data$analysis_id)
  
  # 遍历 mouse_life_stage_data 中的每一行，查找匹配的 analysis_id
  for (i in 1:nrow(mouse_life_stage_data)) {
    # 获取当前 file_name_clean
    current_file <- mouse_life_stage_data$file_name_clean[i]
    
    # 在 combined_data 中找到对应的行（按 analysis_id 匹配）
    matched_row_index <- which(combined_data$analysis_id_clean == current_file)
    
    # 如果找到了匹配的行
    if (length(matched_row_index) > 0) {
      # 获取当前 mouse_life_stage 列的值
      current_value <- combined_data$mouse_life_stage[matched_row_index]
      
      # 如果当前值不在目标值列表中，则进行大小写转换
      if (!(current_value %in% valid_values)) {
        # 自定义大小写转换：只将特定值转换为 "Early adult", "Late adult", "Middle aged adult"
        if (current_value == "early adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Early adult"
        } else if (current_value == "late adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Late adult"
        } else if (current_value == "middle aged adult") {
          combined_data$mouse_life_stage[matched_row_index] <- "Middle aged adult"
        } else {
          # 对其他值进行正常的首字母大写（如 E12.5, E9.5 等）
          combined_data$mouse_life_stage[matched_row_index] <- tools::toTitleCase(current_value)
        }
      }
    }
  }
  
  # 将更新后的数据保存回原始文件
  write.csv(combined_data, 'combined_data.csv', row.names = FALSE)
  
  # 查看更新后的数据
  head(combined_data)

} else {
  print("列 'file_name' 或 'analysis_id' 不存在")
}

```
```{r}
# 读取原始 CSV 文件
combined_data <- read.csv("combined_data.csv")

# 删除 "I" 列
combined_data <- combined_data[, !(names(combined_data) == "I")]

# 保存修改后的数据回原文件
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


```
```{r}
gene_symbol_data <- data_sorted[data_sorted$variable_name == "gene_symbol", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 gene_symbol 数据框

# 查看筛选后的数据
print(gene_symbol_data)


analysis_id_data <- data_sorted[data_sorted$variable_name == "analysis_id", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 analysis_id 数据框

# 查看筛选后的数据
print(analysis_id_data)

gene_symbol_data <- data_sorted[data_sorted$variable_name == "gene_symbol", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 gene_symbol 数据框

# 查看筛选后的数据
print(gene_symbol_data)
```
```{r}
pvalue_data <- data_sorted[data_sorted$variable_name == "pvalue", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 pvalue 数据框

# 查看筛选后的数据
print(pvalue_data)
```
```{r}
# 假设你的数据框是 pvalue_data

# 将 abnormal_value 列从字符型转换为数值型
pvalue_data$abnormal_value <- as.numeric(pvalue_data$abnormal_value)

# 筛选 abnormal_value 不在 0 到 1 之间的行
out_of_range_values <- subset(pvalue_data, abnormal_value <= 0 | abnormal_value >= 1)

# 输出筛选后的数据
print(out_of_range_values)


```
```{r}
# 安装并加载 dplyr 包
library(dplyr)

# 提取 out_of_range_values 中的 file_name 并去除后缀 ".csv"
file_names <- gsub("\\.csv$", "", out_of_range_values$file_name)

# 读取原始的 combined_data.csv 文件
combined_data <- read.csv("combined_data.csv")

# 将 combined_data 中的 analysis_id 列与 file_names 进行匹配，删除匹配行
combined_data <- combined_data %>%
  filter(!analysis_id %in% file_names)

# 保存修改后的数据回原始文件
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


```

```{r}
mouse_strain_data <- data_sorted[data_sorted$variable_name == "mouse_strain", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 mouse_strain 数据框

# 查看筛选后的数据
print(mouse_strain_data)

```
```{r}
# 假设你已经从 data_sorted 筛选出 mouse_strain_data
# 期望的期望值列表
expected_values <- c("C57BL", "B6J", "C3H", "129SV")

# 转换 abnormal_value 列为小写
mouse_strain_data$abnormal_value_lower <- tolower(mouse_strain_data$abnormal_value)

# 筛选出因大小写问题导致没有匹配期望值的数据
case_mismatch_data <- mouse_strain_data[mouse_strain_data$abnormal_value_lower %in% tolower(expected_values) & 
                                        !(mouse_strain_data$abnormal_value %in% expected_values), ]

# 筛选出因其他原因导致没有匹配期望值的数据
other_reasons_data <- mouse_strain_data[!(mouse_strain_data$abnormal_value_lower %in% tolower(expected_values)), ]

# 输出两个数据集
print("Case mismatch data due to case sensitivity:")
print(case_mismatch_data)

print("Other reasons data that does not match expected values:")
print(other_reasons_data)

# 如果需要保存数据框：
# write.csv(case_mismatch_data, "case_mismatch_data.csv", row.names = FALSE)
# write.csv(other_reasons_data, "other_reasons_data.csv", row.names = FALSE)

```

```{r}
# 1. 读取 combined_data.csv
combined_data <- read.csv("combined_data.csv")

# 2. 假设 other_reasons_data 已经是一个数据框
# 去掉 'file_name' 中的 '.csv' 后缀
other_reasons_data$file_name_without_csv <- gsub("\\.csv$", "", other_reasons_data$file_name)

# 3. 匹配 'file_name_without_csv' 和 'analysis_id'，并找出匹配的行
# 使用 %in% 来检查 'analysis_id' 是否出现在 'file_name_without_csv' 中
to_remove <- combined_data$analysis_id %in% other_reasons_data$file_name_without_csv

# 4. 删除匹配的行
combined_data_cleaned <- combined_data[!to_remove, ]

# 5. 保存到原文件
write.csv(combined_data_cleaned, "combined_data.csv", row.names = FALSE)


```

```{r}
parameter_id_data <- data_sorted[data_sorted$variable_name == "parameter_id", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 mouse_strain 数据框

# 查看筛选后的数据
print(parameter_id_data)

```
```{r}
combined_data <- read.csv("combined_data.csv")
# 2. 假设 parameter_id_data 已经是一个数据框
# 去掉 'file_name' 列中的 '.csv' 后缀
parameter_id_data$file_name_without_csv <- gsub("\\.csv$", "", parameter_id_data$file_name)
# 3. 匹配 'file_name_without_csv' 和 'analysis_id'，并找出匹配的行
# 使用 %in% 来检查 'analysis_id' 是否出现在 'file_name_without_csv' 中
to_remove <- combined_data$analysis_id %in% parameter_id_data$file_name_without_csv
# 4. 删除匹配的行
combined_data_cleaned <- combined_data[!to_remove, ]
# 5. 保存到原文件
write.csv(combined_data_cleaned, "combined_data.csv", row.names = FALSE)
```


```{r}
parameter_name_data <- data_sorted[data_sorted$variable_name == "parameter_name", ]

# 合并数据
# 这里合并的意思是把筛选出的数据赋值给 mouse_strain 数据框

# 查看筛选后的数据
print(parameter_name_data)

```
```{r}
# 假设你的数据框是 parameter_name_data
unique_parameters <- unique(parameter_name_data$abnormal_value)

# 查看结果
print(unique_parameters)
#和参考文件做比较发现不用清理
```
```{r}
# 1. 读取 combined_data.csv 文件
combined_data <- read.csv("combined_data.csv")

# 2. 查找完全相同的行，并去除重复，保留第一次出现的行
combined_data_unique <- combined_data[!duplicated(combined_data[c("gene_accession_id", 
                                                                  "gene_symbol", 
                                                                  "mouse_strain", 
                                                                  "mouse_life_stage", 
                                                                  "parameter_id", 
                                                                  "pvalue", 
                                                                  "parameter_name")]), ]

# 3. 查看去重后的数据
head(combined_data_unique)

# 4. 保存去重后的数据回原文件
write.csv(combined_data_unique, "combined_data.csv", row.names = FALSE)

```
