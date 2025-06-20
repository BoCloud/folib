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
public enum PypiRepositoryTypeEnum {

    /**
     * pypi_group
     */
    PYPI_GROUP("pypi_group", RepositoryTypeEnum.GROUP.getType()),
    /**
     * pypi_hosted
     */
    PYPI_HOSTED("pypi_hosted", RepositoryTypeEnum.HOSTED.getType()),
    /**
     * pypi_proxy
     */
    PYPI_PROXY("pypi_proxy", RepositoryTypeEnum.PROXY.getType()),
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
        for (PypiRepositoryTypeEnum item : PypiRepositoryTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }

}
