package com.veadan.folib.enums;

import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 条件类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ConanSearchRepositoryTypeEnum {

    /**
     * conan_group
     */
    CONAN_GROUP("conan_group", RepositoryTypeEnum.GROUP.getType()),
    /**
     * conan_hosted
     */
    CONAN_HOSTED("conan_hosted", RepositoryTypeEnum.HOSTED.getType()),
    /**
     * conan_proxy
     */
    CONAN_PROXY("conan_proxy", RepositoryTypeEnum.PROXY.getType()),
    ;

    /**
     * type
     */
    private String type;

    /**
     * repositoryType
     */
    private String repositoryType;

    public static String resolveType(String repositoryType) {
        String type = "";
        for (ConanSearchRepositoryTypeEnum item : ConanSearchRepositoryTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }

}
