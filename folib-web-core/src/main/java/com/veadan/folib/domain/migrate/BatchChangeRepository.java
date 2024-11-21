package com.veadan.folib.domain.migrate;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * @author huayanjun
 * @since 2024-11-20 10:24
 */

@Data
public class BatchChangeRepository {


    @ExcelProperty("存储空间")
    private String storage;
    @ExcelProperty("仓库")
    private String repository;

    @ExcelProperty("同步状态")
    private String status;
}
