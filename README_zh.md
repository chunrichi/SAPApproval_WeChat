[English](README.md) | [中文](README_zh.md)

# SAP 企业微信审批框架

本框架通过预置组件和模板，简化了在企业微信中开发审批功能的流程。

## 最新动态

🎉 现已支持直接在 ABAP Trial Docker 上传本项目！

## 功能特色

- **审批流程管理**：创建、发送并跟踪审批请求
- **用户映射**：通过手机号自动映射 SAP 用户与企业微信用户
- **模板支持**：常用场景预定义审批模板
- **表单控件**：多种表单控件（文本、数字、日期、选择器、表格等）
- **日志记录**：审批事件与数据的全面日志
- **状态跟踪**：审批状态与结果跟踪
- **定时任务**：自动定时检查审批结果

<div align="center">
  <img src="./docs/images/zlog_wx-event-log.png" alt="企业微信审批事件日志" width="600" style="border: 1px solid #ddd; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); margin: 20px 0;">
  
  **图 1**：企业微信审批事件日志界面
</div>

## 核心组件

| 类名 | 说明 |
|------|------|
| `zcl_wx_approval` | 审批主处理类 |
| `zcl_wx_config` | 企业微信配置加载 |
| `zcl_wx_oa_ft` | 审批模板管理 |
| `zcl_wx_oa_fc` | 审批表单控件 |
| `zcl_wx_log_data` | 审批数据日志 |
| `zcl_wx_log_event` | 审批事件日志 |
| `zcl_wx_cache` | Token 缓存机制 |

## 详细流程

### 1. 模板代码生成

使用 `ztool_wechat_approval_temp` 程序根据企业微信审批模板生成代码：

1. 事务码 `SE38` 执行程序 `ztool_wechat_approval_temp`
2. 输入企业微信模板 ID（如 `C4ZXKAttHPFgVA7gQh57zwCgzgeMQ9BN4aC3BVbud`）
   - 可在审批模板配置页面 URL 中找到
3. 程序将生成完整 ABAP 代码，包括：
   - 企业微信接口初始化
   - 基本赋值（创建人、模板 ID）
   - 表单控件赋值
   - 发送审批结构

**注意事项：**
- 生成代码需手动复制到你的实现类
- 替换 `fixme` 占位符为实际值
- 必填字段已用注释标记

示例代码片段：
```abap
" ------------------- 接口初始化 ----------------------
DATA(l_approval) = NEW zcl_wechat_approval( corpid = `fixme` corpsecret = `fixme` ).

" ------------------- 控件赋值 ----------------------
" 控件: Text 示例字段1 - 文本
" 赋值:必输
DATA(l_01_text) = NEW zcl_wx_oa_fc_text( `Text-1572857932948`
  )->set( '<fixme>' ).
APPEND CAST zif_wx_oa_fc( l_01_text ) TO l_fc->apply_data-contents.
```

### 2. 子类实现

创建继承自 `zcl_wx_oa_ft_send` 的子类，实现字段映射逻辑：

1. 新建类继承 `zcl_wx_oa_ft_send`
2. 复制模板生成的相关代码
3. 实现字段赋值方法
4. 根据业务需求补充逻辑

示例实现（`zcl_wx_oa_ft_demo`）：
```abap
CLASS zcl_wx_oa_ft_demo DEFINITION
  PUBLIC
  INHERITING FROM zcl_wx_oa_ft_send
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS map
      IMPORTING
        !text01         TYPE string    " 示例字段1 - 文本
        !selector02     TYPE string    " 示例字段2 - 选项
        !number03       TYPE string    " 示例字段3 - 数字
      RETURNING
        VALUE(instance) TYPE REF TO zcl_wx_oa_ft_demo.
ENDCLASS.

METHOD map.
  instance = me.

  " 1. 设置摘要信息
  me->ft->set_summary( VALUE #( ( text = '发文审批' lang = 'zh_CN' ) ).

  " 2. 设置文本控件
  DATA(l_text) = NEW zcl_wx_oa_fc_text( `Text-1572857932948` )->set( text01 ).
  APPEND CAST zif_wx_oa_fc( l_text ) TO me->ft->apply_data-contents.

  " 3. 设置选择器控件
  DATA(l_selector) = NEW zcl_wx_oa_fc_selector( 
    id = `Selector-1573203804088`
    type = zcl_wx_oa_fc_selector=>single
  )->set( VALUE #( ( key = selector02 ) ) ).
  APPEND CAST zif_wx_oa_fc( l_selector ) TO me->ft->apply_data-contents.
ENDMETHOD.
```

### 3. 审批结果处理

`zjob_wx_approval_result` 定时任务自动检查审批结果：

1. 事务码 `SM36` 定时调度任务
2. 推荐频率：每 5-10 分钟
3. 任务内容：
   - 拉取企业微信待审批数据
   - 更新 SAP 审批结果
   - 根据状态触发后续动作

**配置示例：**
```abap
" 在 zjob_wx_approval_result 程序中
DATA(lo_processor) = NEW zcl_wx_approval_result( ).
lo_processor->set_check_interval( iv_minutes = 10 ). " 每 10 分钟检查一次
lo_processor->process_pending_approvals( ).
```

## 安装与配置

1. **安装**：
   ```bash
   abapGit pull https://github.com/chunrichi/SAPApproval_WeChat
   ```

2. **配置**：
   - 在 `zcl_wechat_config` 中填写企业微信凭证
   - 在 SU01 维护用户手机号映射
   - 在 `zcl_wx_log_data` 配置日志级别

3. **宏设置**（日志必需）：
   - TCode `SM30` 维护表 `TRMAC`
   - 新增行：
   ```
   /ZWX/LOG	001	INCLUDE ZWX_CMACRO_LOG.
   ```

## 最佳实践

1. **模板管理**：
   - 控件命名建议描述性强
   - 在模板注释中标记必填字段
   - 模板 ID 建议版本管理

2. **错误处理**：
   ```abap
   TRY.
       lo_approval->send( ).
     CATCH zcx_wx_error INTO DATA(lx_error).
       " 处理企业微信 API 错误
   ENDTRY.
   ```

3. **测试建议**：
   - 使用 `zdemo_wx_approval_send` 进行初步测试
   - 检查所有必填字段已映射
   - 多场景测试审批流程

## 常见问题

| 问题 | 解决方法 |
|------|----------|
| 模板 ID 无效 | 检查企业微信后台模板是否存在 |
| 权限不足 | 检查企业微信应用权限配置 |
| 用户映射失败 | 检查 SU01 手机号与企业微信一致 |
| API 连接异常 | 检查网络与企业微信 API 连通性 |

## 技术支持

如有问题或建议，请在 [GitHub Issues](https://github.com/chunrichi/SAPApproval_WeChat/issues) 提交。