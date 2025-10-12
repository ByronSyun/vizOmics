# vizOmics matrixPlot() 改进总结

## 📋 改进概述

本次更新显著增强了 `matrixPlot()` 函数的颜色处理能力，解决了PhiSpace原始实现中存在的颜色类型识别和一致性问题。

## 🎯 解决的核心问题

### 问题1：无法区分离散和连续变量
**原始问题**：
- PhiSpace只检查 `is.numeric(colBy)`
- 无法区分整数聚类标签（如 1,2,3）和真正的连续变量
- Factor变量可能被错误处理

**解决方案**：
- 实现智能类型检测函数 `is_discrete_variable()`
- 启发式规则：
  - Factor/character → 自动识别为离散
  - Numeric且unique值<20且<50%总数 → 识别为离散
  - 其他numeric → 识别为连续

### 问题2：颜色一致性问题
**原始问题**：
- 每次运行可能产生不同的颜色映射
- 结果不可重现

**解决方案**：
- 对离散变量排序factor levels（`sort_levels = TRUE`）
- 创建固定的颜色映射
- 确保相同分组在不同运行中获得相同颜色

### 问题3：有限的颜色选项
**原始问题**：
- 只有MATLAB颜色palette
- 用户无法选择其他配色方案

**解决方案**：
- **连续变量**：matlab, viridis, plasma, inferno, magma
- **离散变量**：Set1, Set2, Set3, Dark2, Paired

## 🆕 新增功能

### 1. 智能颜色类型检测
```r
# 自动检测（默认）
matrixPlot(scores, colBy = clusters)  # 自动判断discrete/continuous

# 手动指定
matrixPlot(scores, colBy = clusters, colBy_type = "discrete")
matrixPlot(scores, colBy = clusters, colBy_type = "continuous")
```

### 2. 多种颜色palette支持
```r
# 连续变量
matrixPlot(scores, colBy = expression, color_palette = "viridis")
matrixPlot(scores, colBy = expression, color_palette = "plasma")

# 离散变量
matrixPlot(scores, colBy = groups, color_palette = "Set2")
matrixPlot(scores, colBy = groups, color_palette = "Dark2")
```

### 3. 颜色一致性控制
```r
# 默认：排序levels确保一致性
matrixPlot(scores, colBy = groups, sort_levels = TRUE)

# 保持原始factor顺序
matrixPlot(scores, colBy = groups, sort_levels = FALSE)
```

### 4. 增强的自定义颜色
```r
# 命名向量（推荐）
custom_colors <- c("GroupA" = "red", "GroupB" = "blue", "GroupC" = "green")
matrixPlot(scores, colBy = groups, manualCol = custom_colors)
```

## 📊 测试覆盖

### 完整测试场景（12个测试）
✅ 基本matrix plot（无着色）  
✅ Factor groups（离散，自动检测）  
✅ Integer clusters（离散，自动检测）  
✅ 强制continuous模式  
✅ Character groups（离散，自动检测）  
✅ 真正的连续变量（自动检测）  
✅ 自定义离散颜色  
✅ 多种连续palette（MATLAB, viridis, plasma）  
✅ 多种离散palette（Set1, Set2, Dark2）  
✅ 颜色一致性/可重现性  
✅ 2组分散点图  
✅ 单组分密度图  

## 🔧 技术实现

### 新增内部函数
1. **`is_discrete_variable(x)`**：智能类型检测
2. **`prepare_discrete_coloring(x, sort_levels)`**：准备离散着色
3. **`get_discrete_colors(level_names, palette_name)`**：生成离散颜色
4. **`get_color_palette(palette_name)`**：获取连续颜色palette

### 新增依赖
- `viridisLite`：viridis系列颜色palette
- `grDevices`：颜色生成
- `utils`：工具函数

## 📈 与PhiSpace对比

| 特性 | PhiSpace | vizOmics (改进后) |
|------|----------|-------------------|
| 颜色类型检测 | ❌ 仅检查numeric | ✅ 智能检测discrete/continuous |
| 颜色一致性 | ❌ 不保证 | ✅ 固定映射，可重现 |
| 连续palette | 🟡 仅MATLAB | ✅ 5种选择 |
| 离散palette | ❌ 无选择 | ✅ 5种预设 |
| 手动控制 | 🟡 有限 | ✅ 完全控制 |
| Factor/character | 🟡 可能有问题 | ✅ 正确处理 |
| 整数聚类 | ❌ 错误用渐变色 | ✅ 自动识别为离散 |
| 文档完整性 | 🟡 基本 | ✅ 详细文档+示例 |

## 🎨 使用示例

### 示例1：整数聚类（自动检测为离散）
```r
clusters <- rep(1:3, length.out = 100)
scores <- data.frame(comp1 = rnorm(100), comp2 = rnorm(100))

# 自动检测为离散，使用离散颜色
matrixPlot(scores, max_ncomp = 2, colBy = clusters)

# 强制为连续（渐变色）
matrixPlot(scores, max_ncomp = 2, colBy = clusters, colBy_type = "continuous")
```

### 示例2：连续基因表达（自动检测为连续）
```r
expression <- rnorm(100)

# 使用不同的连续palette
matrixPlot(scores, max_ncomp = 2, colBy = expression, 
           color_palette = "viridis", legendTitle = "Gene Expression")
```

### 示例3：确保颜色一致性
```r
groups <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))

# 两次运行产生相同的颜色
p1 <- matrixPlot(scores, max_ncomp = 2, colBy = groups)  # A=红, B=蓝, C=绿
p2 <- matrixPlot(scores, max_ncomp = 2, colBy = groups)  # A=红, B=蓝, C=绿（相同）
```

## ✨ 结论

**vizOmics的matrixPlot()现在比PhiSpace更加robust、智能和用户友好！**

### 主要优势
1. ✅ 智能识别变量类型，避免错误的可视化
2. ✅ 颜色映射可重现，确保科学严谨性
3. ✅ 丰富的配色方案，满足不同需求
4. ✅ 完整的用户控制，专业级功能
5. ✅ 详细的文档和示例

### 向后兼容性
- ✅ 所有原有功能保持不变
- ✅ 默认行为更智能但不破坏性
- ✅ 可通过参数回退到简单模式

---

**测试状态**: ✅ 所有12项测试通过  
**文档状态**: ✅ 完整的roxygen2文档  
**代码质量**: ✅ 遵循R包开发最佳实践  
**版本**: vizOmics 0.1.0

