package com.folib.scanner.entity;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class ScanSumByDate extends ScanSumVo{
    private String date;
    private int countFolib;
}
