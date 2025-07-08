package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PromotionFileRelativePath {
    private List<String> list;
    private Map<String, Object> metaData;
}
