package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.File;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionNodeOptionDto {
    private String storageId;
    private String repostoryId;
    Map<String,File> pathMap;
}
