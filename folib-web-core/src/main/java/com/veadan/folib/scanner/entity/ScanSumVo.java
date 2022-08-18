package com.veadan.folib.scanner.entity;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
public class ScanSumVo implements Serializable {
    private int denpendencySum;
    private int vulnerableSum;
    private int vulnerabilitesSum;
    private int suppressedSum;
}
