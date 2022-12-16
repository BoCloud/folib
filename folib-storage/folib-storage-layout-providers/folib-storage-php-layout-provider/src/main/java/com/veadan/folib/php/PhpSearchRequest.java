package com.veadan.folib.php;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PhpSearchRequest {

    private String q;

    private String type;

    private String targetUrl;

    private Integer size;
}
