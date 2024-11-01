package com.veadan.folib.components.sbom;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2024/10/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BomExternalReference {

    /**
     * URL
     */
    private String url;

    /**
     * 哈希列表
     */
    private List<BomHash> hashes;

    /**
     * 类型
     */
    private String type;
}
