package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionNodeOptionDto {
    private String storageId;
    private String repostoryId;
    private Map<String, Map<String,InputStream>> pathMap;
    private Map<String, Map<String, Path>> filePathMap;
    private Map<String, Object> fileMetaDataMap;
}
