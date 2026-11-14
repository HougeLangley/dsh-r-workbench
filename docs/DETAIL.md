# R 数据分析工作台

用本机 R 完成一条可复现的数据分析流水线：**采集 → 清洗 → 价值评估闸门 → 统计分析 → 报告**。当数据不足以支撑有意义的分析时，闸门会明确返回 `STOP`，后续分析与报告工具拒绝执行。

## 它解决什么问题

日常数据工作最常见的失败不是“不会跑检验”，而是**在不能支撑结论的数据上硬写报告**：样本量不足、字段大面积缺失、全是编号和常量、分组后每组只剩几条记录。这个插件把“值不值得分析”变成一个显式的、可复核的闸门，并把结果写成机器可读的 JSON 与人类可读的报告。

## 六个工具

| 工具 | 作用 |
| --- | --- |
| `r_env` | 检查 R / RStudio / Quarto / pandoc、必需 R 包、中文字体、磁盘与最近运行 |
| `r_ingest` | 采集并清洗 CSV/TSV/文本、Excel/WPS、ODS、JSON/NDJSON、ZIP；识别编码与表头；合并多文件 |
| `r_assess` | 六维价值评估，输出 `PROCEED` / `CONDITIONAL` / `STOP` 与 0–100 分 |
| `r_analyze` | 组间对比、相关、分类关联、月度趋势；效应量、95% CI、BH 校正、假设检查 |
| `r_report` | 自包含 HTML、Markdown、Quarto 源文件；pandoc 可用时生成 DOCX |
| `r_run` | 在运行目录里执行一段 R 代码（RStudio 式探索），数据已预加载为 `clean` |

## 标准流程

```text
r_env
  → r_ingest { paths: [...] }
  → r_assess { run }
      ├─ STOP        → 停止；只保留价值评估报告
      ├─ CONDITIONAL → r_analyze（探索性）→ r_report（报告标注“仅探索性”）
      └─ PROCEED     → r_analyze → r_report
```

`r_analyze` 和 `r_report` 在 R 层再次校验：评估哈希必须与当前数据一致，`STOP` 直接拒绝，`CONDITIONAL` 标记为探索性。绕过闸门不是靠提示词约束，而是工具执行路径本身会失败。

## 价值闸门规则

六个维度加权：

| 维度 | 权重 | 说明 |
| --- | --- | --- |
| 样本量 | 15% | `log10(n+1)/3`，n=30 约 0.50，n=1000 为 1 |
| 完整度 | 20% | 非缺失单元格占比；被整列删除的空列按缺失计 |
| 信息变异 | 20% | 数值列用变异系数，分类列用归一化熵，按可分析列平均 |
| 独立性与冗余 | 15% | 重复行比例与高度相关列（|r|≥0.98） |
| 可分析性 | 20% | 最佳可行方案的最小分组样本量 / 期望频数 |
| 解析可信度 | 10% | 编码置信度、类型转换、清洗警告 |

硬性终止项（任一触发即 `STOP`）：

- 有效样本 < 10 行；
- 整体完整度 < 40%；
- 重复行占比 > 85%；
- 既无数值指标、也没有两个可交叉分析的分组变量；
- 没有任何满足“每组 ≥5 行 / 期望频数 ≥5”的分析方案。

判定规则：无硬性终止且总分 ≥60、可分析性 ≥0.65、样本量分 ≥0.45 → `PROCEED`；总分 ≥40 且可分析性 ≥0.45 → `CONDITIONAL`；否则 `STOP`。

> 分数不是“数据好不好”的绝对真理，而是一道可审计的工程闸门。阈值可以在 R 脚本中调整，但不要在报告里绕过它。

## 支持的输入

- **分隔文本**：`.csv` `.tsv` `.tab` `.txt` `.dat` `.psv` `.log`，自动嗅探 `, ; \t |`，支持前置说明行、自动表头、引号内换行；`format: "delimited"` 可强制。
- **编码**：UTF-8（含 BOM）、UTF-16LE/BE、GB18030/GBK、latin1；检测结果写入 `ingest.json`。
- **Excel / WPS**：`.xlsx` `.xlsm` `.xls`，以及 xlsx 兼容的 WPS `.et`；`sheet: "*"` 读取全部工作表并增加 `source_sheet` 列。
- **ODS**：依赖可选包 `readODS`。
- **JSON / NDJSON**：对象数组、记录流、扁平对象。
- **ZIP**：递归读取包内的 csv/tsv/txt/json/xlsx/xls/ods（最多 20 个文件）。
- **目录**：`paths` 传入目录时读取一层内支持的文件。

## 清洗内容

- 去首尾空白、NBSP、全宽空格、零宽字符；
- 缺失标记：`NA` `N/A` `null` `None` `NaN` `无` `未知` `缺失` `未填` `空`；
- 类型推断：逻辑、整数、数值、日期、日期时间、文本；数值支持千分位、货币符号、百分号、`万`、全角数字；
- 日期格式：`YYYY-MM-DD`、`YYYY/MM/DD`、`YYYY.MM.DD`、`YYYY年MM月DD日` 及常见时间格式；
- 移除空行、全空列、常量列、完全重复行（均可配置）；
- 编号 / 高基数列标记为 `id`，不进入统计分析；
- 合并多文件时增加 `source_file`，合并多工作表时增加 `source_sheet`。

## 统计分析

- 两组数值：正态且方差齐用 Student t，否则 Welch t；非正态用 Wilcoxon 秩和检验；报告 Hedges g（含 CI）或秩二列相关 + Hodges-Lehmann 位置差 CI。
- 多组数值：Welch 单因素方差分析或 Kruskal-Wallis；报告 η² / ε²；三组及以上附带 BH 校正的成对 Wilcoxon 事后比较。
- 分类 × 分类：Pearson 卡方（期望频数不足时蒙特卡洛模拟）；报告 Cramér's V 与标准化残差最大的单元格。
- 数值 × 数值：Pearson 或 Spearman 相关，报告 Fisher z 变换的 95% CI。
- 日期 × 数值：按月聚合后的线性趋势 + Spearman 趋势检验，报告每月斜率与 CI。
- 所有主检验的 p 值做 Benjamini-Hochberg 校正。

报告中的措辞遵循“未显著 ≠ 无差异/无关”，并列出观察性数据、完整案例、多重比较等局限。

## 产物

每次运行是一个独立目录 `<workspace>/r-workbench-runs/<runId>/`，可直接作为 RStudio 项目打开：

```text
<runId>/
├── <runId>.Rproj
├── README.md
├── run.json                 # 运行元数据与状态
├── ingest.json              # 输入、编码、清洗、列字典、数据哈希
├── assessment.json          # 闸门决策与六维证据
├── analysis.json            # 统计结果、效应量、p 值、图表路径
├── report.json / report.html / report.md / report.qmd / report.docx
├── data/clean.csv|clean.rds
├── outputs/assessment.html|analysis.md|tables/|figures/
├── logs/                    # 各阶段 config/result/日志、R 代码片段
└── scripts/explore.R|reproduce.R
```

`report.html` 是自包含的（图片 base64 内嵌），可直接归档或发给同事。`scripts/reproduce.R` 会用保存的阶段配置重跑整条流水线。

## 配置

bundle 行的 `config` 支持：

| 字段 | 默认值 | 说明 |
| --- | --- | --- |
| `runsDirName` | `r-workbench-runs` | 工作目录下的运行根目录名（单段目录名） |
| `rscriptPath` | `Rscript` | Rscript 可执行文件 |
| `pandocPath` | 空（自动探测） | DOCX 转换使用的 pandoc |
| `timeouts` | 见源码 | 各阶段超时毫秒数 |

## 依赖

必需 R 包：`data.table`、`readxl`、`writexl`、`jsonlite`、`digest`、`base64enc`、`ggplot2`。
可选：`readODS`（ODS）、`knitr`/`rmarkdown`（Quarto 执行 R）、`renv`、`lubridate`。

安装：

```bash
Rscript <插件目录>/r/install-deps.R
```

`r_env` 会报告缺失项并给出同一条命令。R 包安装到用户库，不需要 root。

## 已知限制

- **旧版 WPS `.et`**：私有二进制格式。xlsx 兼容的 `.et` 可直接读取；旧格式需要先用 WPS 打开并另存为 `.xlsx`，插件会给出明确错误而不是静默失败。
- **DOCX 中文字体**：pandoc 默认参考文档的字体映射取决于本机 Word；HTML 报告的中文字体由系统字体决定。
- **敏感数据**：插件不做 PII 自动识别或脱敏；报告会包含列示例与统计结果，含敏感字段时请先脱敏或改用聚合数据。
- **因果推断**：所有组间差异与相关性均为观察性结果，报告不会写成因果结论。
- **缺失值**：默认完整案例分析，不做插补；缺失机制需要使用者自行判断。
- **`r_run` 安全边界**：代码以当前用户权限执行，不是沙箱，只应运行可信代码。
- **RStudio 启动**：插件生成 `.Rproj` 与脚本，不自动拉起图形界面；双击项目或运行 `rstudio <目录>` 即可。

## 验证口径

交付前实际跑通（`tests/run-tests.sh` 覆盖 R 层，真实工具调用另行验收）：

- 高价值 CSV + GB18030 文本合并 → `PROCEED` → 分析 → HTML/Markdown/DOCX 报告；
- 高价值 Excel（含日期）→ `PROCEED`；
- 20 行小样本 → `CONDITIONAL`，逐条结论带“探索性”标识，报告可生成；
- 低价值 Excel（常量、编号、极小样本）→ `STOP`，`r_analyze`/`r_report` 均拒绝执行；
- WPS 兼容 `.et`、JSON、ZIP、目录批量采集；
- 篡改 `data/clean.rds` 后分析拒绝执行；
- 缺月数据集的日历月趋势斜率、前导零编号保留、单列表格不被合并；
- `r_env` 报告 R/RStudio/Quarto/pandoc、依赖与最近运行。

`tests/` 中的脚本用于重复这些检查，不替代真实工具调用。
