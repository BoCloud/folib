package com.folib.components.sbom;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2024/10/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BomComponent {

    /**
     * 名称
     */
    private String name;

    /**
     * sha1
     */
    private String sha1;

    /**
     * 外部引用列表
     */
    private List<BomExternalReference> externalReferences;

    /**
     * 许可证列表
     */
    private List<BomLicense> licenses;

}
