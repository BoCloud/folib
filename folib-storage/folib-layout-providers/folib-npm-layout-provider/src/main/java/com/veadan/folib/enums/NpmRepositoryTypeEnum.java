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
public enum NpmRepositoryTypeEnum {

    /**
     * npm_group
     */
    NPM_GROUP("npm_group", RepositoryTypeEnum.GROUP.getType()),
    /**
     * npm_hosted
     */
    NPM_HOSTED("npm_hosted", RepositoryTypeEnum.HOSTED.getType()),
    /**
     * npm_proxy
     */
    NPM_PROXY("npm_proxy", RepositoryTypeEnum.PROXY.getType()),
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
        for (NpmRepositoryTypeEnum item : NpmRepositoryTypeEnum.values()) {
            if (item.getRepositoryType().equals(repositoryType)) {
                type = item.getType();
                break;
            }
        }
        return type;
    }

}
