package com.folib.scanner.entity;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class ScannerSumDifVo extends ScanSumVo{
    private String storage;
    private String repository;
    private String layout;
    private int countFolib;
    private int star;
    private String id;
}
